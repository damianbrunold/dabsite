(define-library (damian notepad)
  (import (scheme base)
          (scheme write)
          (scheme char)
          (srfi 1)
          (srfi 13)
          (scm crypto)
          (scm database postgres)
          (scm net http request)
          (scm net http response)
          (scm net http route)
          (damian db)
          (damian util)
          (damian auth)
          (damian markdown)
          (damian views))
  (export install-notepad-routes!)
  (begin

    ;; ------------------------------------------------------------
    ;; Notes are plain text by default. If the first line is exactly
    ;; "!markdown" (optionally with surrounding whitespace), the rest of
    ;; the note is rendered as markdown.
    ;; ------------------------------------------------------------

    (define markdown-marker "!markdown")

    (define (first-line-and-rest s)
      ;; Returns (values first-line rest-including-leading-newline-stripped).
      (let* ((nl (string-index s #\newline)))
        (cond
          ((not nl) (values s ""))
          (else (values (substring s 0 nl)
                        (substring s (+ nl 1) (string-length s)))))))

    (define (markdown-note? body)
      (and (string? body)
           (call-with-values
             (lambda () (first-line-and-rest body))
             (lambda (first _)
               (string=? (string-trim-both first) markdown-marker)))))

    (define (strip-markdown-marker body)
      (call-with-values
        (lambda () (first-line-and-rest body))
        (lambda (_ rest) rest)))

    ;; ------------------------------------------------------------
    ;; A note's "name" is its URL slug. Auto-generated names are three
    ;; short dictionary words joined with dashes (e.g. apple-river-pine).
    ;; ------------------------------------------------------------

    (define word-list
      ;; Short, easily-typed, ambiguity-free. ~64 words → ~256K combos for
      ;; three words. More than enough for personal use.
      '("amber" "apple" "arrow" "aspen" "basil" "birch" "blaze" "brook"
        "cedar" "cliff" "cloud" "coral" "cove" "crane" "creek" "crest"
        "dune" "ember" "fern" "field" "flame" "flint" "frost" "glade"
        "grove" "hawk" "heath" "ivy" "jade" "lake" "leaf" "lemon"
        "lichen" "linen" "lotus" "marsh" "meadow" "mist" "moss" "north"
        "oak" "olive" "orchid" "otter" "peak" "pine" "plum" "quartz"
        "raven" "reef" "ridge" "river" "sage" "shell" "south" "spring"
        "stone" "tide" "topaz" "trail" "valley" "willow" "wren" "yew"))

    (define word-vec (list->vector word-list))

    (define (random-byte)
      (bytevector-u8-ref (random-bytes 1) 0))

    (define (random-word)
      (vector-ref word-vec (modulo (random-byte) (vector-length word-vec))))

    (define (random-note-name)
      (string-append (random-word) "-" (random-word) "-" (random-word)))

    (define (valid-name? s)
      ;; Accept lowercase letters, digits, '-' and '_'. 1..64 chars.
      (let ((n (string-length s)))
        (and (> n 0) (<= n 64)
             (let loop ((i 0))
               (cond
                 ((= i n) #t)
                 (else
                  (let ((c (string-ref s i)))
                    (cond
                      ((or (and (char>=? c #\a) (char<=? c #\z))
                           (and (char>=? c #\0) (char<=? c #\9))
                           (char=? c #\-)
                           (char=? c #\_))
                       (loop (+ i 1)))
                      (else #f)))))))))

    ;; --- DB ---

    (define (note-exists? cfg name)
      (let ((rows (with-db cfg
                    (lambda (c)
                      (pg-result-rows
                        (pg-query c
                          (string-append
                            "SELECT 1 FROM notes WHERE name = "
                            (sql-quote-literal name)
                            " LIMIT 1")))))))
        (pair? rows)))

    (define (note-by-name cfg name)
      (let ((rows (with-db cfg
                    (lambda (c)
                      (pg-result->alist-list
                        (pg-query c
                          (string-append
                            "SELECT id, name, body, "
                            "to_char(created_at, 'YYYY-MM-DD HH24:MI') AS created, "
                            "to_char(updated_at, 'YYYY-MM-DD HH24:MI') AS updated "
                            "FROM notes WHERE name = "
                            (sql-quote-literal name)
                            " LIMIT 1")))))))
        (if (pair? rows) (car rows) #f)))

    (define (list-notes cfg q)
      (let* ((base (string-append "SELECT id, name, "
                                  "  substring(body, 1, 160) AS preview, "
                                  "  to_char(updated_at, 'YYYY-MM-DD HH24:MI') AS updated "
                                  "FROM notes "))
             (where (cond
                      ((and q (> (string-length (string-trim-both q)) 0))
                       (string-append
                         "WHERE to_tsvector('simple', name || ' ' || body) "
                         "@@ plainto_tsquery('simple', "
                         (sql-quote-literal q) ") "))
                      (else "")))
             (order "ORDER BY updated_at DESC LIMIT 200")
             (sql (string-append base where order)))
        (with-db cfg
          (lambda (c)
            (pg-result->alist-list (pg-query c sql))))))

    (define (create-note! cfg name body)
      (with-db cfg
        (lambda (c)
          (pg-exec c
            (string-append
              "INSERT INTO notes (name, body) VALUES ("
              (sql-quote-literal name) ", "
              (sql-quote-literal body) ")")))))

    (define (update-note! cfg name body)
      (with-db cfg
        (lambda (c)
          (pg-exec c
            (string-append
              "UPDATE notes SET body = " (sql-quote-literal body)
              " WHERE name = " (sql-quote-literal name))))))

    (define (delete-note! cfg name)
      (with-db cfg
        (lambda (c)
          (pg-exec c
            (string-append
              "DELETE FROM notes WHERE name = "
              (sql-quote-literal name))))))

    (define (allocate-name cfg)
      ;; Generate random names until one is free. With 64^3 names, this
      ;; should almost always succeed on the first try.
      (let loop ((tries 0))
        (cond
          ((>= tries 20)
           (error "notepad: failed to allocate a unique random name"))
          (else
           (let ((n (random-note-name)))
             (if (note-exists? cfg n) (loop (+ tries 1)) n))))))

    ;; --- views ---

    (define (note-field row key) (or (and row (cdr (assoc key row))) ""))

    (define (render-list-view req auth cfg q)
      (let ((notes (list-notes cfg q))
            (out (open-output-string)))
        (write-string "<header class=\"list-head\">" out)
        (write-string "<h1>Notes</h1>" out)
        (write-string "<form method=\"get\" action=\"/notes\" class=\"search\">" out)
        (write-string "<input type=\"search\" name=\"q\" placeholder=\"search\" value=\"" out)
        (write-string (html-attr-escape (or q "")) out)
        (write-string "\">" out)
        (write-string "<button type=\"submit\">Search</button>" out)
        (write-string "<a class=\"btn\" href=\"/notes/new\">New note</a>" out)
        (write-string "</form>" out)
        (write-string "</header>" out)

        (cond
          ((null? notes)
           (write-string "<p class=\"empty\">No notes yet.</p>" out))
          (else
           (write-string "<ul class=\"notes\">" out)
           (for-each
             (lambda (row)
               (write-string "<li><a href=\"/notes/" out)
               (write-string (html-attr-escape (note-field row "name")) out)
               (write-string "\"><span class=\"name\">" out)
               (write-string (html-escape (note-field row "name")) out)
               (write-string "</span><span class=\"updated\">" out)
               (write-string (html-escape (note-field row "updated")) out)
               (write-string "</span><span class=\"preview\">" out)
               (write-string (html-escape (note-field row "preview")) out)
               (write-string "</span></a></li>" out))
             notes)
           (write-string "</ul>" out)))
        (html-response
          (render-page req auth
                       '((title  . "Notes")
                         (active . notes))
                       (get-output-string out)))))

    (define (render-view req auth cfg name)
      (let ((note (note-by-name cfg name)))
        (cond
          ((not note) (render-error 404 "Note not found."))
          (else
           (let ((out (open-output-string)))
             (write-string "<header class=\"note-head\">" out)
             (write-string "<h1>" out)
             (write-string (html-escape (note-field note "name")) out)
             (write-string "</h1>" out)
             (write-string "<div class=\"meta\">updated " out)
             (write-string (html-escape (note-field note "updated")) out)
             (write-string "</div>" out)
             (write-string "<div class=\"actions\">" out)
             (write-string "<a class=\"btn\" href=\"/notes/" out)
             (write-string (html-attr-escape name) out)
             (write-string "/edit\">Edit</a>" out)
             (write-string "<form method=\"post\" action=\"/notes/" out)
             (write-string (html-attr-escape name) out)
             (write-string (string-append "/delete\" class=\"inline\" "
                                          "data-confirm=\"Delete this note?\">")
                           out)
             (write-string "<button type=\"submit\" class=\"danger\">Delete</button>" out)
             (write-string "</form>" out)
             (write-string "</div>" out)
             (write-string "</header>" out)
             (let ((body (note-field note "body")))
               (cond
                 ((markdown-note? body)
                  ;; "page" class reuses the landing-page typography.
                  (write-string "<article class=\"page\">" out)
                  (write-string (render-markdown (strip-markdown-marker body))
                                out)
                  (write-string "</article>" out))
                 (else
                  (write-string "<pre class=\"note-body\">" out)
                  (write-string (html-escape body) out)
                  (write-string "</pre>" out))))
             (html-response
               (render-page req auth
                            (list (cons 'title (string-append "Note " name))
                                  (cons 'active 'notes))
                            (get-output-string out))))))))

    (define (render-edit-form req auth name body err)
      (let ((out (open-output-string))
            (new? (string=? name "")))
        (write-string "<h1>" out)
        (write-string (if new? "New note" "Edit note") out)
        (write-string "</h1>" out)
        (when err
          (write-string "<p class=\"error\">" out)
          (write-string (html-escape err) out)
          (write-string "</p>" out))
        (write-string "<form method=\"post\" action=\"" out)
        (cond
          (new? (write-string "/notes" out))
          (else (write-string "/notes/" out)
                (write-string (html-attr-escape name) out)
                (write-string "/edit" out)))
        (write-string "\" class=\"note-edit\">" out)

        (when new?
          (write-string "<label>Name (optional)<input type=\"text\" name=\"name\" value=\"" out)
          (write-string "\" placeholder=\"auto: word-word-word\" maxlength=\"64\"></label>" out))

        (write-string "<label>Body<textarea name=\"body\" rows=\"24\" autofocus>" out)
        (write-string (html-escape body) out)
        (write-string "</textarea></label>" out)
        (write-string
          (string-append
            "<p class=\"hint\">Tip: start the first line with "
            "<code>!markdown</code> to render the rest as Markdown.</p>")
          out)

        (write-string "<div class=\"actions\">" out)
        (write-string "<button type=\"submit\">Save</button>" out)
        (write-string "<a href=\"/notes" out)
        (when (not new?)
          (write-string "/" out)
          (write-string (html-attr-escape name) out))
        (write-string "\">Cancel</a>" out)
        (write-string "</div></form>" out)
        (html-response
          (render-page req auth
                       (list (cons 'title (if new? "New note" (string-append "Edit " name)))
                             (cons 'active 'notes)
                             (cons 'body-class "editor"))
                       (get-output-string out)))))

    (define (handle-create req params cfg auth)
      (let* ((form  (parse-www-form (or (http-request-body req) "")))
             (raw   (string-trim-both (form-ref form "name" "")))
             (body  (form-ref form "body" ""))
             (name  (cond ((= 0 (string-length raw)) (allocate-name cfg))
                          (else raw))))
        (cond
          ((not (valid-name? name))
           (render-edit-form req auth "" body
             "Name must be 1-64 chars: lowercase letters, digits, - or _."))
          ((note-exists? cfg name)
           (render-edit-form req auth "" body
             (string-append "A note named '" name "' already exists.")))
          (else
           (create-note! cfg name body)
           (make-http-response 302
             (list (cons "Location" (string-append "/notes/" name))) "")))))

    (define (handle-edit req params cfg auth)
      (let* ((name (params-ref params "name"))
             (form (parse-www-form (or (http-request-body req) "")))
             (body (form-ref form "body" "")))
        (cond
          ((not (note-exists? cfg name)) (render-error 404 "Note not found."))
          (else
           (update-note! cfg name body)
           (make-http-response 302
             (list (cons "Location" (string-append "/notes/" name))) "")))))

    (define (handle-delete req params cfg auth)
      (let ((name (params-ref params "name")))
        (delete-note! cfg name)
        (make-http-response 302 (list (cons "Location" "/notes")) "")))

    ;; --- route registration ---

    (define (install-notepad-routes! router cfg auth)
      (router-add! router "GET" "/notes"
        (require-auth auth
          (lambda (req params)
            (let ((q (cdr (or (assoc "q" (url-query-params (http-request-url req)))
                              '("q" . #f)))))
              (render-list-view req auth cfg
                (cond ((string? q) (percent-decode q)) (else #f)))))))

      (router-add! router "GET" "/notes/new"
        (require-auth auth
          (lambda (req params)
            (render-edit-form req auth "" "" #f))))

      (router-add! router "POST" "/notes"
        (require-auth auth
          (lambda (req params) (handle-create req params cfg auth))))

      (router-add! router "GET" "/notes/:name"
        (require-auth auth
          (lambda (req params)
            (render-view req auth cfg (params-ref params "name")))))

      (router-add! router "GET" "/notes/:name/edit"
        (require-auth auth
          (lambda (req params)
            (let* ((name (params-ref params "name"))
                   (note (note-by-name cfg name)))
              (cond
                ((not note) (render-error 404 "Note not found."))
                (else (render-edit-form req auth
                                        name (note-field note "body") #f)))))))

      (router-add! router "POST" "/notes/:name/edit"
        (require-auth auth
          (lambda (req params) (handle-edit req params cfg auth))))

      (router-add! router "POST" "/notes/:name/delete"
        (require-auth auth
          (lambda (req params) (handle-delete req params cfg auth)))))

))
