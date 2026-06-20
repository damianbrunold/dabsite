(define-library (dabsite notepad)
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
          (scm net http forms)
          (scm html builder)
          (scm uri)
          (scm markdown)
          (dabsite db)
          (dabsite util)
          (dabsite auth)
          (dabsite views))
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
                          "SELECT 1 FROM notes WHERE name = $1 LIMIT 1"
                          name))))))
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
                            "FROM notes WHERE name = $1 LIMIT 1")
                          name))))))
        (and (pair? rows) (car rows))))

    (define (like-escape s)
      ;; Escape ILIKE wildcards so a search term matches literally. The
      ;; default LIKE escape character is backslash.
      (string-fold
        (lambda (c acc)
          (string-append
            acc
            (if (or (char=? c #\\) (char=? c #\%) (char=? c #\_))
                (string #\\ c)
                (string c))))
        ""
        s))

    (define (list-notes cfg q)
      ;; Split the query into whitespace-separated terms. A note matches
      ;; when *every* term occurs (case-insensitively, as a substring) in
      ;; its name or body. ILIKE keeps this to plain, portable SQL.
      (let* ((base (string-append "SELECT id, name, "
                                  "  substring(body, 1, 160) AS preview, "
                                  "  to_char(updated_at, 'YYYY-MM-DD HH24:MI') AS updated "
                                  "FROM notes "))
             (terms (if (and q (non-empty-trimmed? q))
                        (string-tokenize q)
                        '()))
             (params (map (lambda (t)
                            (string-append "%" (like-escape t) "%"))
                          terms))
             (where (if (null? terms)
                        ""
                        (string-append
                          "WHERE "
                          (string-join
                            (map (lambda (i)
                                   (string-append
                                     "(name || ' ' || body) ILIKE $"
                                     (number->string i)))
                                 (iota (length terms) 1))
                            " AND ")
                          " ")))
             (order "ORDER BY updated_at DESC LIMIT 200")
             (sql (string-append base where order)))
        (with-db cfg
          (lambda (c)
            (pg-result->alist-list
              (apply pg-query c sql params))))))

    (define (create-note! cfg name body)
      (with-db cfg
        (lambda (c)
          (pg-exec c
            "INSERT INTO notes (name, body) VALUES ($1, $2)"
            name body))))

    (define (update-note! cfg name body)
      (with-db cfg
        (lambda (c)
          (pg-exec c
            "UPDATE notes SET body = $1 WHERE name = $2"
            body name))))

    (define (delete-note! cfg name)
      (with-db cfg
        (lambda (c)
          (pg-exec c
            "DELETE FROM notes WHERE name = $1"
            name))))

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

    (define (note-list-row-sxml row)
      (let ((name (note-field row "name")))
        `(li (a (@ (href ,(string-append "/notes/" name)))
                (span (@ (class "name"))    ,name)
                (span (@ (class "updated")) ,(note-field row "updated"))
                (span (@ (class "preview")) ,(note-field row "preview"))))))

    (define (render-list-view req auth cfg q)
      (let* ((notes (list-notes cfg q))
             (body
               `((header (@ (class "list-head"))
                   (h1 "Notes")
                   (form (@ (method "get") (action "/notes") (class "search"))
                     (input (@ (type "search") (name "q")
                               (placeholder "search") (value ,(or q ""))))
                     (button (@ (type "submit")) "Search")
                     (a (@ (class "btn") (href "/notes/new")) "New note")))
                 ,(cond
                    ((null? notes)
                     `(p (@ (class "empty")) "No notes yet."))
                    (else
                     `(ul (@ (class "notes"))
                          ,@(map note-list-row-sxml notes)))))))
        (html-response
          (render-page req auth
                       '((title  . "Notes")
                         (active . notes))
                       (html->string body)))))

    (define (render-view req auth cfg name)
      (let ((note (note-by-name cfg name)))
        (cond
          ((not note) (render-error 404 "Note not found."))
          (else
           (let* ((body-text (note-field note "body"))
                  (body-node
                    (cond
                      ((markdown-note? body-text)
                       ;; "page" class reuses landing-page typography.
                       `(article (@ (class "page"))
                          ,(raw (markdown->html
                                  (strip-markdown-marker body-text)))))
                      (else
                       `(pre (@ (class "note-body")) ,body-text))))
                  (page-body
                    `((header (@ (class "note-head"))
                        (h1 ,(note-field note "name"))
                        (div (@ (class "meta"))
                          "updated " ,(note-field note "updated"))
                        (div (@ (class "actions"))
                          (a (@ (class "btn")
                                (href ,(string-append "/notes/" name "/edit")))
                             "Edit")
                          (form (@ (method "post")
                                   (action ,(string-append "/notes/" name "/delete"))
                                   (class "inline")
                                   (data-confirm "Delete this note?"))
                            (button (@ (type "submit") (class "danger"))
                              "Delete"))))
                      ,body-node)))
             (html-response
               (render-page req auth
                            (list (cons 'title (string-append "Note " name))
                                  (cons 'active 'notes))
                            (html->string page-body))))))))

    (define (render-edit-form req auth name body err)
      (let* ((new? (string=? name ""))
             (form-action (cond (new? "/notes")
                                (else (string-append "/notes/" name "/edit"))))
             (cancel-href (cond (new? "/notes")
                                (else (string-append "/notes/" name))))
             (page-body
               `((h1 ,(if new? "New note" "Edit note"))
                 ,@(if err
                       `((p (@ (class "error")) ,err))
                       '())
                 (form (@ (method "post") (action ,form-action)
                          (class "note-edit"))
                   ,@(if new?
                         `((label "Name (optional)"
                             (input (@ (type "text") (name "name") (value "")
                                       (placeholder "auto: word-word-word")
                                       (maxlength "64")))))
                         '())
                   (label "Body"
                     (textarea (@ (name "body") (rows "24") (autofocus #t))
                       ,body))
                   (p (@ (class "hint"))
                      "Tip: start the first line with "
                      (code "!markdown")
                      " to render the rest as Markdown.")
                   (div (@ (class "actions"))
                     (button (@ (type "submit")) "Save")
                     (a (@ (href ,cancel-href)) "Cancel"))))))
        (html-response
          (render-page req auth
                       (list (cons 'title
                                   (if new? "New note"
                                       (string-append "Edit " name)))
                             (cons 'active 'notes)
                             (cons 'body-class "editor"))
                       (html->string page-body)))))

    (define (handle-create req params cfg auth)
      (let* ((form  (parse-www-form (or (http-request-body req) "")))
             (raw   (string-trim-both (form-ref form "name" "")))
             (body  (form-ref form "body" ""))
             (name  (if (string=? raw "") (allocate-name cfg) raw)))
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
                (cond ((string? q) q) (else #f)))))))

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
