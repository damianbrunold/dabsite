(define-library (dabsite polls)
  (import (scheme base)
          (scheme write)
          (scheme char)
          (scheme cxr)
          (srfi 1)
          (srfi 13)
          (scm crypto)
          (scm database postgres)
          (scm net http request)
          (scm net http response)
          (scm net http route)
          (scm net http forms)
          (scm net http cookies)
          (scm html builder)
          (dabsite db)
          (dabsite auth)
          (dabsite views))
  (export install-poll-routes!
          ;; exposed for tests
          tally
          valid-slug?
          random-slug)
  (begin

    ;; ============================================================
    ;; Slugs and validation
    ;; ============================================================

    (define word-list
      '("amber" "apple" "arrow" "aspen" "basil" "birch" "blaze" "brook"
        "cedar" "cliff" "cloud" "coral" "cove" "crane" "creek" "crest"
        "dune" "ember" "fern" "field" "flame" "flint" "frost" "glade"
        "grove" "hawk" "heath" "ivy" "jade" "lake" "leaf" "lemon"
        "lichen" "linen" "lotus" "marsh" "meadow" "mist" "moss" "north"
        "oak" "olive" "orchid" "otter" "peak" "pine" "plum" "quartz"
        "raven" "reef" "ridge" "river" "sage" "shell" "south" "spring"
        "stone" "tide" "topaz" "trail" "valley" "willow" "wren" "yew"))

    (define word-vec (list->vector word-list))

    (define (random-word)
      (vector-ref word-vec (modulo (bytevector-u8-ref (random-bytes 1) 0)
                                   (vector-length word-vec))))

    (define (random-slug)
      (string-append (random-word) "-" (random-word) "-" (random-word)))

    (define (valid-slug? s)
      ;; Mirror the DB CHECK: ^[a-z0-9_-]{1,64}$
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
                           (char=? c #\-) (char=? c #\_))
                       (loop (+ i 1)))
                      (else #f)))))))))

    ;; ============================================================
    ;; Tally
    ;; ============================================================

    (define (tally option-ids choices)
      ;; option-ids   : list of option-id strings, in display order.
      ;; choices      : list of (option-id . value) pairs, value in
      ;;                {"yes","no","maybe"}.
      ;; Returns list parallel to option-ids: each element is
      ;; (yes-count maybe-count no-count).
      (map (lambda (oid)
             (let loop ((cs choices) (y 0) (m 0) (n 0))
               (cond
                 ((null? cs) (list y m n))
                 (else
                  (let ((p (car cs)))
                    (cond
                      ((string=? (car p) oid)
                       (let ((v (cdr p)))
                         (cond
                           ((string=? v "yes")   (loop (cdr cs) (+ y 1) m n))
                           ((string=? v "maybe") (loop (cdr cs) y (+ m 1) n))
                           ((string=? v "no")    (loop (cdr cs) y m (+ n 1)))
                           (else (loop (cdr cs) y m n)))))
                      (else (loop (cdr cs) y m n))))))))
           option-ids))

    ;; ============================================================
    ;; Cookies (owner token)
    ;; ============================================================

    (define cookie-name "poll_owner")
    (define cookie-max-age (* 60 60 24 365 5))  ;; 5y

    (define (request-owner-cookie req)
      (let ((cookies (parse-cookie-header
                       (http-request-header req "Cookie"))))
        (cookie-ref cookies cookie-name)))

    (define (new-owner-cookie)
      ;; 32 random bytes → 64 hex chars. Unique enough across all polls.
      (bytevector->hex (random-bytes 32)))

    (define (set-owner-cookie token secure-cookies?)
      (cond
        (secure-cookies?
         (format-set-cookie cookie-name token cookie-max-age "/" 'secure))
        (else
         (format-set-cookie cookie-name token cookie-max-age "/" 'no-secure))))

    ;; ============================================================
    ;; DB ops
    ;; ============================================================

    (define (alist-rows cfg sql . maybe-params)
      (let ((params (if (null? maybe-params) '() (car maybe-params))))
        (with-db cfg
          (lambda (c)
            (pg-result->alist-list
              (cond ((null? params) (pg-query c sql))
                    (else (pg-query c (pg-format-sql sql params)))))))))

    (define (rows cfg sql . maybe-params)
      (let ((params (if (null? maybe-params) '() (car maybe-params))))
        (with-db cfg
          (lambda (c)
            (pg-result-rows
              (cond ((null? params) (pg-query c sql))
                    (else (pg-query c (pg-format-sql sql params)))))))))

    (define (exec cfg sql . maybe-params)
      (let ((params (if (null? maybe-params) '() (car maybe-params))))
        (with-db cfg
          (lambda (c)
            (cond ((null? params) (pg-exec c sql))
                  (else (pg-exec c (pg-format-sql sql params))))))))

    (define (poll-by-slug cfg slug)
      ;; "closed" in the result alist is the *effective* closed flag (true
      ;; when either the manual flag is set OR closes_at has passed) so
      ;; renderers can just check one field. "closed_manual" reflects only
      ;; the manual flag, used to decide whether to show "reopen" vs
      ;; "close" in admin.
      (let ((r (alist-rows cfg
                 (string-append
                   "SELECT id::text AS id, slug, title, description, "
                   "       CASE WHEN closed "
                   "         OR (closes_at IS NOT NULL AND closes_at < now()) "
                   "         THEN 'yes' ELSE 'no' END AS closed, "
                   "       CASE WHEN closed THEN 'yes' ELSE 'no' END AS closed_manual, "
                   "       COALESCE(to_char(closes_at, 'YYYY-MM-DD HH24:MI'), '') "
                   "         AS closes_at_str, "
                   "       to_char(created_at, 'YYYY-MM-DD HH24:MI') AS created "
                   "FROM polls WHERE slug = $1 LIMIT 1")
                 (list slug))))
        (cond ((pair? r) (car r)) (else #f))))

    (define (poll-options cfg poll-id)
      (alist-rows cfg
        (string-append
          "SELECT id::text AS id, label "
          "FROM poll_options WHERE poll_id = $1 "
          "ORDER BY sort_order, id")
        (list poll-id)))

    (define (poll-responses cfg poll-id)
      (alist-rows cfg
        (string-append
          "SELECT id::text AS id, name, owner_cookie, "
          "       to_char(updated_at, 'YYYY-MM-DD HH24:MI') AS updated "
          "FROM poll_responses WHERE poll_id = $1 "
          "ORDER BY created_at")
        (list poll-id)))

    (define (poll-choices cfg poll-id)
      ;; All (response_id . option_id . value) for a poll.
      (alist-rows cfg
        (string-append
          "SELECT pc.response_id::text AS response_id, "
          "       pc.option_id::text   AS option_id, "
          "       pc.value             AS value "
          "FROM poll_choices pc "
          "JOIN poll_responses pr ON pr.id = pc.response_id "
          "WHERE pr.poll_id = $1")
        (list poll-id)))

    (define (slug-exists? cfg slug)
      (pair? (rows cfg "SELECT 1 FROM polls WHERE slug = $1 LIMIT 1"
                   (list slug))))

    (define (allocate-slug cfg)
      (let loop ((tries 0))
        (cond
          ((>= tries 30)
           (error "polls: failed to allocate a unique slug"))
          (else
           (let ((s (random-slug)))
             (cond ((slug-exists? cfg s) (loop (+ tries 1)))
                   (else s)))))))

    (define (create-poll! cfg slug title description option-labels closes-at)
      ;; closes-at is either a string in "YYYY-MM-DDTHH:MM" form (from a
      ;; datetime-local input) or "" / #f to leave the column NULL.
      (with-db cfg
        (lambda (c)
          (pg-exec c "BEGIN")
          (guard (exn (#t (guard (e (#t #f)) (pg-exec c "ROLLBACK"))
                          (raise exn)))
            (let* ((closes-val
                     (cond
                       ((or (not closes-at) (string=? closes-at "")) #f)
                       (else closes-at)))
                   (res (cond
                          (closes-val
                           (pg-query c
                             (string-append
                               "INSERT INTO polls (slug, title, description, closes_at) "
                               "VALUES ($1, $2, $3, $4::timestamptz) RETURNING id")
                             slug title description closes-val))
                          (else
                           (pg-query c
                             (string-append
                               "INSERT INTO polls (slug, title, description, closes_at) "
                               "VALUES ($1, $2, $3, NULL) RETURNING id")
                             slug title description))))
                   (rows (pg-result-rows res))
                   (id   (cond ((pair? rows) (vector-ref (car rows) 0))
                               (else (error "create-poll!: no id returned"))))
                   (poll-id (string->number id)))
              (let loop ((i 0) (ls option-labels))
                (cond
                  ((null? ls) #t)
                  (else
                   (pg-exec c
                     "INSERT INTO poll_options (poll_id, sort_order, label) VALUES ($1, $2, $3)"
                     poll-id i (car ls))
                   (loop (+ i 1) (cdr ls)))))
              (pg-exec c "COMMIT")
              poll-id)))))

    (define (delete-poll! cfg slug)
      (exec cfg "DELETE FROM polls WHERE slug = $1" (list slug)))

    (define (close-poll! cfg slug)
      (exec cfg "UPDATE polls SET closed = true WHERE slug = $1" (list slug)))

    (define (reopen-poll! cfg slug)
      ;; Manual reopen also clears closes_at — otherwise a poll closed by
      ;; auto-expiry would immediately auto-close again.
      (exec cfg
            "UPDATE polls SET closed = false, closes_at = NULL WHERE slug = $1"
            (list slug)))

    (define (find-response cfg poll-id owner-cookie)
      ;; Returns the response alist or #f.
      (let ((rs (alist-rows cfg
                  (string-append
                    "SELECT id::text AS id, name "
                    "FROM poll_responses "
                    "WHERE poll_id = $1 AND owner_cookie = $2 LIMIT 1")
                  (list poll-id owner-cookie))))
        (cond ((pair? rs) (car rs)) (else #f))))

    (define (find-existing-response-id c poll-id owner-cookie)
      (let ((rows (pg-result-rows
                    (pg-query c
                      (string-append
                        "SELECT id FROM poll_responses "
                        "WHERE poll_id = $1 AND owner_cookie = $2 LIMIT 1")
                      poll-id owner-cookie))))
        (cond
          ((pair? rows) (string->number (vector-ref (car rows) 0)))
          (else #f))))

    (define (update-response-name! c rid name)
      (pg-exec c "UPDATE poll_responses SET name = $1 WHERE id = $2" name rid)
      (pg-exec c "DELETE FROM poll_choices WHERE response_id = $1" rid))

    (define (insert-response! c poll-id name owner-cookie)
      (let ((rows (pg-result-rows
                    (pg-query c
                      (string-append
                        "INSERT INTO poll_responses (poll_id, name, owner_cookie) "
                        "VALUES ($1, $2, $3) RETURNING id")
                      poll-id name owner-cookie))))
        (cond
          ((pair? rows) (string->number (vector-ref (car rows) 0)))
          (else (error "insert-response!: no id returned")))))

    (define (insert-choices! c resp-id choices)
      (when (pair? choices)
        (for-each
          (lambda (p)
            (pg-exec c
              "INSERT INTO poll_choices (response_id, option_id, value) VALUES ($1, $2, $3)"
              resp-id (string->number (car p)) (cdr p)))
          choices)))

    (define (upsert-response! cfg poll-id owner-cookie name choices)
      ;; choices: list of (option-id-string . value-string). Returns the
      ;; response id. The whole upsert runs in a transaction so a partial
      ;; row of choices can't be left behind on a crash.
      (with-db cfg
        (lambda (c)
          (pg-exec c "BEGIN")
          (guard (exn (#t (guard (e (#t #f)) (pg-exec c "ROLLBACK"))
                          (raise exn)))
            (let* ((existing (find-existing-response-id c poll-id owner-cookie))
                   (resp-id  (cond
                               (existing
                                (update-response-name! c existing name)
                                existing)
                               (else
                                (insert-response! c poll-id name owner-cookie)))))
              (insert-choices! c resp-id choices)
              (pg-exec c "COMMIT")
              resp-id)))))

    ;; ============================================================
    ;; Views
    ;; ============================================================

    (define (row-field r k) (let ((p (assoc k r))) (if p (cdr p) "")))

    (define (build-choice-map choices)
      ;; choices: list of alists with keys "response_id" "option_id" "value".
      ;; Returns alist ((response_id . option_id) . value).
      (map (lambda (r)
             (cons (cons (cdr (assoc "response_id" r))
                         (cdr (assoc "option_id"   r)))
                   (cdr (assoc "value" r))))
           choices))

    (define (choice-for-cell cmap response-id option-id)
      (let ((p (assoc (cons response-id option-id) cmap)))
        (cond (p (cdr p)) (else ""))))

    (define (cell-class v)
      (cond ((string=? v "yes")   "y")
            ((string=? v "maybe") "m")
            ((string=? v "no")    "n")
            (else "")))

    (define (cell-glyph v)
      (cond ((string=? v "yes")   "✓")
            ((string=? v "maybe") "~")
            ((string=? v "no")    "✗")
            (else "")))

    (define (poll-header-sxml poll)
      (let ((closes      (row-field poll "closes_at_str"))
            (eff-closed? (string=? (row-field poll "closed") "yes"))
            (desc        (row-field poll "description")))
        `(header (@ (class "poll-head"))
           (h1 ,(row-field poll "title"))
           ,(cond
              (eff-closed?
               `(p (@ (class "poll-status closed"))
                   "Closed"
                   ,@(cond ((not (string=? closes ""))
                            `(,(string-append " — was open until " closes)))
                           (else '()))))
              ((not (string=? closes ""))
               `(p (@ (class "poll-status open"))
                   "Open until " ,closes))
              (else ""))
           ,(cond
              ((not (string=? desc ""))
               `(p (@ (class "poll-desc")) ,desc))
              (else "")))))

    (define (poll-matrix-row-sxml cookie options cmap r)
      (let* ((rid     (row-field r "id"))
             (rname   (row-field r "name"))
             (rcookie (row-field r "owner_cookie"))
             (mine?   (and cookie (string=? cookie rcookie))))
        `(tr (@ (class ,(if mine? "mine" #f)))
             (td (@ (class "name")) ,rname)
             ,@(map (lambda (o)
                      (let* ((oid (row-field o "id"))
                             (v   (choice-for-cell cmap rid oid)))
                        `(td (@ (class ,(cell-class v))) ,(cell-glyph v))))
                    options))))

    (define (poll-matrix-sxml cookie options responses cmap counts)
      `(div (@ (class "poll-matrix-wrap"))
         (table (@ (class "poll-matrix"))
           (thead
             (tr (th "Name")
                 ,@(map (lambda (o)
                          `(th (@ (class "opt"))
                               ,(row-field o "label")))
                        options)))
           (tbody
             ,(cond
                ((null? responses)
                 `(tr (td (@ (colspan ,(number->string (+ 1 (length options))))
                             (class "empty"))
                          "No responses yet.")))
                (else
                 `(,@(map (lambda (r)
                            (poll-matrix-row-sxml cookie options cmap r))
                          responses))))
             (tr (@ (class "tally"))
                 (th "tally")
                 ,@(map
                    (lambda (cnt)
                      (let ((y (car cnt)) (m (cadr cnt)))
                        `(td (span (@ (class "y")) ,(number->string y))
                             ,@(cond ((> m 0)
                                      `(" " (span (@ (class "m"))
                                                  ,(string-append
                                                     "+" (number->string m)))))
                                     (else '())))))
                    counts))))))

    (define (poll-form-field-sxml o existing-rid cmap)
      (let* ((oid (row-field o "id"))
             (current (cond (existing-rid
                             (choice-for-cell cmap
                               (number->string existing-rid) oid))
                            (else "")))
             (name (string-append "option_" oid)))
        `(fieldset (@ (class "opt"))
           (legend ,(row-field o "label"))
           ,@(map (lambda (v)
                    `(label (@ (class ,(string-append "choice " v)))
                       (input (@ (type "radio") (name ,name) (value ,v)
                                 (checked ,(if (string=? current v) #t #f))
                                 (required #t)))
                       (span ,v)))
                  '("yes" "maybe" "no")))))

    (define (render-public-poll req auth cfg poll public-base secure-cookies?)
      (let* ((poll-id   (string->number (row-field poll "id")))
             (slug      (row-field poll "slug"))
             (options   (poll-options    cfg poll-id))
             (responses (poll-responses  cfg poll-id))
             (choices   (poll-choices    cfg poll-id))
             (cmap      (build-choice-map choices))
             (option-ids (map (lambda (o) (row-field o "id")) options))
             (flat-choices
               (map (lambda (c) (cons (cdr (assoc "option_id" c))
                                      (cdr (assoc "value"     c))))
                    choices))
             (counts    (tally option-ids flat-choices))
             (cookie    (request-owner-cookie req))
             (mine      (and cookie (find-response cfg poll-id cookie)))
             (existing-rid (and mine
                                (string->number (row-field mine "id"))))
             (closed? (string=? (row-field poll "closed") "yes"))
             (form-block
               (cond
                 (closed?
                  `(p (@ (class "hint"))
                      "This poll is closed — no further responses can be added."))
                 (else
                  `(form (@ (method "post")
                            (action ,(string-append "/poll/" slug))
                            (class "poll-form"))
                     (h2 ,(if mine "Edit your response" "Add your response"))
                     (label "Your name "
                       (input (@ (type "text") (name "name")
                                 (required #t) (maxlength "40")
                                 (value ,(if mine
                                             (row-field mine "name")
                                             "")))))
                     ,@(map (lambda (o)
                              (poll-form-field-sxml o existing-rid cmap))
                            options)
                     (div (@ (class "actions"))
                       (button (@ (type "submit"))
                         ,(if mine "Update" "Submit")))))))
             (body
               `(,(poll-header-sxml poll)
                 ,(poll-matrix-sxml cookie options responses cmap counts)
                 ,form-block)))
        (html-response
          (render-page req auth
                       (list (cons 'title  (row-field poll "title"))
                             (cons 'active 'polls)
                             (cons 'body-class "poll-public"))
                       (html->string body)))))

    ;; --- admin views ---

    (define (admin-poll-row-sxml p)
      (let* ((slug    (row-field p "slug"))
             (closed? (string=? (row-field p "closed_eff") "yes"))
             (manual? (string=? (row-field p "closed_manual") "yes")))
        `(tr
           (td (@ (class "slug"))
               (a (@ (href ,(string-append "/poll/" slug))) ,slug))
           (td (@ (class "title"))     ,(row-field p "title"))
           (td (@ (class "n-options")) ,(row-field p "n_options"))
           (td (@ (class "n-responses")) ,(row-field p "n_responses"))
           (td (@ (class "closes"))    ,(row-field p "closes_at_str"))
           (td (@ (class "poll-status-cell"))
               (span (@ (class ,(if closed? "badge closed" "badge open")))
                     ,(if closed? "closed" "open")))
           (td (@ (class "created"))   ,(row-field p "created"))
           (td (@ (class "acts"))
               ,(cond
                  (manual?
                   `(form (@ (method "post")
                             (action ,(string-append "/polls/" slug "/reopen"))
                             (class "inline"))
                      (button (@ (class "linkish")) "reopen")))
                  (else
                   `(form (@ (method "post")
                             (action ,(string-append "/polls/" slug "/close"))
                             (class "inline"))
                      (button (@ (class "linkish")) "close"))))
               " "
               (form (@ (method "post")
                        (action ,(string-append "/polls/" slug "/delete"))
                        (class "inline")
                        (data-confirm "Delete this poll and all responses?"))
                 (button (@ (class "linkish danger")) "delete"))))))

    (define (render-admin-list req auth cfg)
      (let* ((polls (alist-rows cfg
                     (string-append
                       "SELECT p.slug, p.title, "
                       "       CASE WHEN p.closed "
                       "         OR (p.closes_at IS NOT NULL AND p.closes_at < now()) "
                       "         THEN 'yes' ELSE 'no' END AS closed_eff, "
                       "       CASE WHEN p.closed THEN 'yes' ELSE 'no' END AS closed_manual, "
                       "       COALESCE(to_char(p.closes_at, 'YYYY-MM-DD HH24:MI'), '') AS closes_at_str, "
                       "       to_char(p.created_at, 'YYYY-MM-DD HH24:MI') AS created, "
                       "       (SELECT count(*) FROM poll_responses pr "
                       "          WHERE pr.poll_id = p.id)::text AS n_responses, "
                       "       (SELECT count(*) FROM poll_options po "
                       "          WHERE po.poll_id = p.id)::text AS n_options "
                       "FROM polls p ORDER BY created_at DESC")))
             (body
               `((header (@ (class "feeds-head"))
                   (h1 "Polls")
                   (a (@ (class "btn") (href "/polls/new")) "New poll"))
                 ,(cond
                    ((null? polls)
                     `(p (@ (class "empty")) "No polls yet."))
                    (else
                     `(table (@ (class "feed-table mobile-cards polls-list"))
                        (thead
                          (tr (th "slug") (th "title") (th "options")
                              (th "responses") (th "closes") (th "status")
                              (th "created") (th)))
                        (tbody ,@(map admin-poll-row-sxml polls))))))))
        (html-response
          (render-page req auth
                       '((title  . "Polls")
                         (active . polls)
                         (body-class . "feeds-page"))
                       (html->string body)))))

    (define (render-admin-new req auth cfg . opt)
      (let* ((msg (cond ((pair? opt) (car opt)) (else #f)))
             (body
               `((header (@ (class "feeds-head"))
                   (h1 "New poll")
                   (a (@ (href "/polls")) ,(raw "← back")))
                 ,@(if msg `((p (@ (class "error")) ,msg)) '())
                 (form (@ (method "post") (action "/polls") (class "page-edit"))
                   (label "Title "
                     (input (@ (type "text") (name "title") (required #t))))
                   (label "Slug (optional) "
                     (input (@ (type "text") (name "slug")
                               (pattern "[a-z0-9_-]{1,64}")
                               (placeholder "auto: word-word-word"))))
                   (label "Description "
                     (textarea (@ (name "description") (rows "3"))))
                   (label "Options (one per line)"
                     (textarea (@ (name "options") (rows "8") (required #t)
                                  (placeholder ,(raw "Mon 2pm&#10;Tue 6pm&#10;Wed 9am")))))
                   (label "Closes at (optional) "
                     (input (@ (type "datetime-local") (name "closes_at"))))
                   (div (@ (class "actions"))
                     (button (@ (type "submit")) "Create")
                     (a (@ (href "/polls")) "Cancel"))))))
        (html-response
          (render-page req auth
                       '((title  . "New poll")
                         (active . polls)
                         (body-class . "feeds-page editor"))
                       (html->string body)))))

    ;; ============================================================
    ;; Routes
    ;; ============================================================

    (define (split-lines s)
      ;; Splits on \n, trims each, drops empties.
      (let* ((n (string-length s))
             (acc '()))
        (let loop ((i 0) (start 0))
          (cond
            ((= i n)
             (let ((last (string-trim-both (substring s start n))))
               (cond
                 ((string=? last "") (reverse acc))
                 (else (reverse (cons last acc))))))
            ((char=? (string-ref s i) #\newline)
             (let ((line (string-trim-both (substring s start i))))
               (cond
                 ((string=? line "") (loop (+ i 1) (+ i 1)))
                 (else (set! acc (cons line acc))
                       (loop (+ i 1) (+ i 1))))))
            (else (loop (+ i 1) start))))))

    (define (install-poll-routes! router cfg auth secure-cookies?)

      ;; --- Admin (auth required) ---

      (router-add! router "GET" "/polls"
        (require-auth auth
          (lambda (req params) (render-admin-list req auth cfg))))

      (router-add! router "GET" "/polls/new"
        (require-auth auth
          (lambda (req params) (render-admin-new req auth cfg))))

      (router-add! router "POST" "/polls"
        (require-auth auth
          (lambda (req params)
            (let* ((form (parse-www-form (or (http-request-body req) "")))
                   (title (string-trim-both (form-ref form "title" "")))
                   (desc  (string-trim-both (form-ref form "description" "")))
                   (raw-slug (string-trim-both (form-ref form "slug" "")))
                   (opts-raw (form-ref form "options" ""))
                   (options  (split-lines opts-raw))
                   (closes-at (string-trim-both (form-ref form "closes_at" ""))))
              (cond
                ((string=? title "")
                 (render-admin-new req auth cfg "Title is required."))
                ((null? options)
                 (render-admin-new req auth cfg "At least one option is required."))
                ((and (not (string=? raw-slug ""))
                      (not (valid-slug? raw-slug)))
                 (render-admin-new req auth cfg
                   "Slug must match [a-z0-9_-]{1,64}."))
                (else
                 (let ((slug (cond ((string=? raw-slug "")
                                    (allocate-slug cfg))
                                   (else raw-slug))))
                   (cond
                     ((and (not (string=? raw-slug ""))
                           (slug-exists? cfg slug))
                      (render-admin-new req auth cfg
                        (string-append "Slug '" slug "' is already taken.")))
                     (else
                      (create-poll! cfg slug title desc options closes-at)
                      (make-http-response 302
                        (list (cons "Location"
                                    (string-append "/poll/" slug)))
                        ""))))))))))

      (router-add! router "POST" "/polls/:slug/delete"
        (require-auth auth
          (lambda (req params)
            (let ((slug (params-ref params "slug")))
              (when (and slug (valid-slug? slug))
                (delete-poll! cfg slug))
              (make-http-response 302
                (list (cons "Location" "/polls")) "")))))

      (router-add! router "POST" "/polls/:slug/close"
        (require-auth auth
          (lambda (req params)
            (let ((slug (params-ref params "slug")))
              (when (and slug (valid-slug? slug))
                (close-poll! cfg slug))
              (make-http-response 302
                (list (cons "Location" "/polls")) "")))))

      (router-add! router "POST" "/polls/:slug/reopen"
        (require-auth auth
          (lambda (req params)
            (let ((slug (params-ref params "slug")))
              (when (and slug (valid-slug? slug))
                (reopen-poll! cfg slug))
              (make-http-response 302
                (list (cons "Location" "/polls")) "")))))

      ;; --- Public ---

      (router-add! router "GET" "/poll/:slug"
        (lambda (req params)
          (let* ((slug (params-ref params "slug"))
                 (poll (and slug (valid-slug? slug)
                            (poll-by-slug cfg slug))))
            (cond
              ((not poll) (render-error 404 "Poll not found."))
              (else
               (render-public-poll req auth cfg poll
                                   "" secure-cookies?))))))

      (router-add! router "POST" "/poll/:slug"
        (lambda (req params)
          (let* ((slug (params-ref params "slug"))
                 (poll (and slug (valid-slug? slug)
                            (poll-by-slug cfg slug))))
            (cond
              ((not poll) (render-error 404 "Poll not found."))
              ((string=? (row-field poll "closed") "yes")
               (render-error 403 "This poll is closed."))
              (else
               (let* ((poll-id (string->number (row-field poll "id")))
                      (form    (parse-www-form (or (http-request-body req) "")))
                      (name    (string-trim-both (form-ref form "name" "")))
                      (radios  (form-refs-by-prefix form "option_"))
                      (existing-cookie (request-owner-cookie req))
                      (cookie  (or existing-cookie (new-owner-cookie)))
                      (set-cookie? (not existing-cookie)))
                 (cond
                   ((string=? name "")
                    (render-error 400 "Name is required."))
                   ((null? radios)
                    (render-error 400 "Please answer every option."))
                   (else
                    (let* ((choices
                             (map (lambda (p)
                                    (let* ((k (car p))
                                           ;; key looks like "option_<id>"
                                           (oid (substring k 7
                                                  (string-length k))))
                                      (cons oid (cdr p))))
                                  radios)))
                      (upsert-response! cfg poll-id cookie name choices)
                      (let ((headers (list
                                       (cons "Location"
                                             (string-append "/poll/" slug)))))
                        (cond
                          (set-cookie?
                           (make-http-response 302
                             (cons (cons "Set-Cookie"
                                         (set-owner-cookie cookie
                                                           secure-cookies?))
                                   headers)
                             ""))
                          (else
                           (make-http-response 302 headers ""))))))))))))))

))
