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
          (scm html)
          (dabsite db)
          (dabsite util)
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

    (define (alist-rows cfg sql)
      (with-db cfg (lambda (c) (pg-result->alist-list (pg-query c sql)))))

    (define (rows cfg sql)
      (with-db cfg (lambda (c) (pg-result-rows (pg-query c sql)))))

    (define (exec cfg sql)
      (with-db cfg (lambda (c) (pg-exec c sql))))

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
                   "FROM polls WHERE slug = "
                   (sql-quote-literal slug)
                   " LIMIT 1"))))
        (cond ((pair? r) (car r)) (else #f))))

    (define (poll-options cfg poll-id)
      (alist-rows cfg
        (string-append
          "SELECT id::text AS id, label "
          "FROM poll_options WHERE poll_id = " (sql-quote-int poll-id)
          " ORDER BY sort_order, id")))

    (define (poll-responses cfg poll-id)
      (alist-rows cfg
        (string-append
          "SELECT id::text AS id, name, owner_cookie, "
          "       to_char(updated_at, 'YYYY-MM-DD HH24:MI') AS updated "
          "FROM poll_responses WHERE poll_id = " (sql-quote-int poll-id)
          " ORDER BY created_at")))

    (define (poll-choices cfg poll-id)
      ;; All (response_id . option_id . value) for a poll.
      (alist-rows cfg
        (string-append
          "SELECT pc.response_id::text AS response_id, "
          "       pc.option_id::text   AS option_id, "
          "       pc.value             AS value "
          "FROM poll_choices pc "
          "JOIN poll_responses pr ON pr.id = pc.response_id "
          "WHERE pr.poll_id = " (sql-quote-int poll-id))))

    (define (slug-exists? cfg slug)
      (pair? (rows cfg (string-append
                         "SELECT 1 FROM polls WHERE slug = "
                         (sql-quote-literal slug) " LIMIT 1"))))

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
            (let* ((closes-sql
                     (cond
                       ((or (not closes-at) (string=? closes-at "")) "NULL")
                       (else (string-append
                               (sql-quote-literal closes-at)
                               "::timestamptz"))))
                   (res (pg-query c
                          (string-append
                            "INSERT INTO polls "
                            "(slug, title, description, closes_at) VALUES ("
                            (sql-quote-literal slug) ", "
                            (sql-quote-literal title) ", "
                            (sql-quote-literal description) ", "
                            closes-sql
                            ") RETURNING id")))
                   (rows (pg-result-rows res))
                   (id   (cond ((pair? rows) (vector-ref (car rows) 0))
                               (else (error "create-poll!: no id returned"))))
                   (poll-id (string->number id)))
              (let loop ((i 0) (ls option-labels))
                (cond
                  ((null? ls) #t)
                  (else
                   (pg-exec c
                     (string-append
                       "INSERT INTO poll_options (poll_id, sort_order, label) VALUES ("
                       (sql-quote-int poll-id) ", "
                       (sql-quote-int i) ", "
                       (sql-quote-literal (car ls)) ")"))
                   (loop (+ i 1) (cdr ls)))))
              (pg-exec c "COMMIT")
              poll-id)))))

    (define (delete-poll! cfg slug)
      (exec cfg (string-append "DELETE FROM polls WHERE slug = "
                               (sql-quote-literal slug))))

    (define (close-poll! cfg slug)
      (exec cfg (string-append "UPDATE polls SET closed = true WHERE slug = "
                               (sql-quote-literal slug))))

    (define (reopen-poll! cfg slug)
      ;; Manual reopen also clears closes_at — otherwise a poll closed by
      ;; auto-expiry would immediately auto-close again.
      (exec cfg (string-append "UPDATE polls SET closed = false, "
                               "closes_at = NULL WHERE slug = "
                               (sql-quote-literal slug))))

    (define (find-response cfg poll-id owner-cookie)
      ;; Returns the response alist or #f.
      (let ((rs (alist-rows cfg
                  (string-append
                    "SELECT id::text AS id, name "
                    "FROM poll_responses WHERE poll_id = "
                    (sql-quote-int poll-id)
                    " AND owner_cookie = "
                    (sql-quote-literal owner-cookie)
                    " LIMIT 1"))))
        (cond ((pair? rs) (car rs)) (else #f))))

    (define (find-existing-response-id c poll-id owner-cookie)
      (let ((rows (pg-result-rows
                    (pg-query c
                      (string-append
                        "SELECT id FROM poll_responses WHERE poll_id = "
                        (sql-quote-int poll-id)
                        " AND owner_cookie = "
                        (sql-quote-literal owner-cookie)
                        " LIMIT 1")))))
        (cond
          ((pair? rows) (string->number (vector-ref (car rows) 0)))
          (else #f))))

    (define (update-response-name! c rid name)
      (pg-exec c
        (string-append
          "UPDATE poll_responses SET name = " (sql-quote-literal name)
          " WHERE id = " (sql-quote-int rid)))
      (pg-exec c
        (string-append
          "DELETE FROM poll_choices WHERE response_id = "
          (sql-quote-int rid))))

    (define (insert-response! c poll-id name owner-cookie)
      (let ((rows (pg-result-rows
                    (pg-query c
                      (string-append
                        "INSERT INTO poll_responses (poll_id, name, owner_cookie) "
                        "VALUES ("
                        (sql-quote-int poll-id) ", "
                        (sql-quote-literal name) ", "
                        (sql-quote-literal owner-cookie)
                        ") RETURNING id")))))
        (cond
          ((pair? rows) (string->number (vector-ref (car rows) 0)))
          (else (error "insert-response!: no id returned")))))

    (define (insert-choices! c resp-id choices)
      (when (pair? choices)
        (let ((rows-sql
                (map (lambda (p)
                       (string-append
                         "(" (sql-quote-int resp-id) ", "
                         (sql-quote-int (string->number (car p))) ", "
                         (sql-quote-literal (cdr p)) ")"))
                     choices)))
          (pg-exec c
            (string-append
              "INSERT INTO poll_choices (response_id, option_id, value) VALUES "
              (string-join rows-sql ", "))))))

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

    (define (render-public-poll req auth cfg poll public-base secure-cookies?)
      (let* ((poll-id   (string->number (row-field poll "id")))
             (slug      (row-field poll "slug"))
             (options   (poll-options    cfg poll-id))
             (responses (poll-responses  cfg poll-id))
             (choices   (poll-choices    cfg poll-id))
             (cmap      (build-choice-map choices))
             (option-ids (map (lambda (o) (row-field o "id")) options))
             ;; Tally only over yes/maybe response choice values, NOT all.
             (flat-choices
               (map (lambda (c) (cons (cdr (assoc "option_id" c))
                                      (cdr (assoc "value"     c))))
                    choices))
             (counts    (tally option-ids flat-choices))
             (cookie    (request-owner-cookie req))
             (mine      (and cookie (find-response cfg poll-id cookie)))
             (out       (open-output-string)))

        (out! out "<header class=\"poll-head\"><h1>"
                  (html-escape (row-field poll "title"))
                  "</h1>")
        (let ((closes (row-field poll "closes_at_str"))
              (eff-closed? (string=? (row-field poll "closed") "yes")))
          (cond
            (eff-closed?
             (out! out "<p class=\"poll-status closed\">Closed"
                       (cond ((not (string=? closes ""))
                              (string-append " — was open until "
                                             (html-escape closes)))
                             (else ""))
                       "</p>"))
            ((not (string=? closes ""))
             (out! out "<p class=\"poll-status open\">Open until "
                       (html-escape closes) "</p>"))))
        (when (not (string=? (row-field poll "description") ""))
          (out! out "<p class=\"poll-desc\">"
                    (html-escape (row-field poll "description"))
                    "</p>"))
        (out! out "</header>")

        ;; Response matrix.
        (out! out "<div class=\"poll-matrix-wrap\">"
                  "<table class=\"poll-matrix\"><thead><tr><th>Name</th>")
        (for-each
          (lambda (o)
            (out! out "<th class=\"opt\">"
                      (html-escape (row-field o "label")) "</th>"))
          options)
        (out! out "</tr></thead><tbody>")
        (cond
          ((null? responses)
           (out! out "<tr><td colspan=\"" (number->string (+ 1 (length options)))
                     "\" class=\"empty\">No responses yet.</td></tr>"))
          (else
           (for-each
             (lambda (r)
               (let ((rid    (row-field r "id"))
                     (rname  (row-field r "name"))
                     (rcookie (row-field r "owner_cookie")))
                 (out! out "<tr")
                 (when (and cookie (string=? cookie rcookie))
                   (out! out " class=\"mine\""))
                 (out! out "><td class=\"name\">" (html-escape rname) "</td>")
                 (for-each
                   (lambda (o)
                     (let* ((oid (row-field o "id"))
                            (v   (choice-for-cell cmap rid oid)))
                       (out! out "<td class=\"" (cell-class v) "\">"
                                 (cell-glyph v) "</td>")))
                   options)
                 (out! out "</tr>")))
             responses)))
        ;; Tally row.
        (out! out "<tr class=\"tally\"><th>tally</th>")
        (for-each
          (lambda (cnt)
            (let ((y (car cnt)) (m (cadr cnt)) (n (caddr cnt)))
              (out! out "<td>"
                        "<span class=\"y\">" (number->string y) "</span>"
                        (cond ((> m 0)
                               (string-append " <span class=\"m\">+"
                                              (number->string m) "</span>"))
                              (else ""))
                        "</td>")))
          counts)
        (out! out "</tr>")
        (out! out "</tbody></table></div>")

        ;; Response form. We don't show the form if the poll is closed
        ;; (manually or by expiry); existing responses remain visible above.
        (cond
          ((string=? (row-field poll "closed") "yes")
           (out! out "<p class=\"hint\">This poll is closed — "
                     "no further responses can be added.</p>"))
          (else
           (out! out "<form method=\"post\" action=\"/poll/"
                     (html-attr-escape slug) "\" class=\"poll-form\">"
                     "<h2>"
                     (cond (mine "Edit your response")
                           (else "Add your response"))
                     "</h2>"
                     "<label>Your name "
                     "<input type=\"text\" name=\"name\" required maxlength=\"40\""
                     " value=\"")
           (when mine (out! out (html-attr-escape (row-field mine "name"))))
           (out! out "\"></label>")
           (let ((existing-rid (and mine
                                    (string->number (row-field mine "id")))))
             (for-each
               (lambda (o)
                 (let* ((oid (row-field o "id"))
                        (current (cond (existing-rid
                                        (choice-for-cell cmap
                                          (number->string existing-rid) oid))
                                       (else ""))))
                   (out! out "<fieldset class=\"opt\"><legend>"
                             (html-escape (row-field o "label"))
                             "</legend>")
                   (for-each
                     (lambda (v glyph)
                       (let ((name (string-append "option_" oid)))
                         (out! out "<label class=\"choice " v "\">"
                                   "<input type=\"radio\" name=\""
                                   (html-attr-escape name)
                                   "\" value=\"" v "\"")
                         (when (string=? current v)
                           (out! out " checked"))
                         (out! out " required>"
                                   "<span>" glyph "</span></label>")))
                     '("yes" "maybe" "no")
                     '("yes" "maybe" "no")))
                 (out! out "</fieldset>"))
               options))
           (out! out "<div class=\"actions\"><button type=\"submit\">"
                     (cond (mine "Update") (else "Submit"))
                     "</button></div></form>")))

        (html-response
          (render-page req auth
                       (list (cons 'title  (row-field poll "title"))
                             (cons 'active 'polls)
                             (cons 'body-class "poll-public"))
                       (get-output-string out)))))

    ;; --- admin views ---

    (define (render-admin-list req auth cfg)
      (let ((polls (alist-rows cfg
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
            (out   (open-output-string)))
        (out! out "<header class=\"feeds-head\"><h1>Polls</h1>"
                  "<a class=\"btn\" href=\"/polls/new\">New poll</a></header>")
        (cond
          ((null? polls)
           (out! out "<p class=\"empty\">No polls yet.</p>"))
          (else
           (out! out "<table class=\"feed-table mobile-cards polls-list\">"
                     "<thead><tr>"
                     "<th>slug</th><th>title</th><th>options</th>"
                     "<th>responses</th><th>closes</th><th>status</th>"
                     "<th>created</th><th></th>"
                     "</tr></thead><tbody>")
           (for-each
             (lambda (p)
               (let* ((slug    (row-field p "slug"))
                      (closed? (string=? (row-field p "closed_eff") "yes"))
                      (manual? (string=? (row-field p "closed_manual") "yes")))
                 (out! out "<tr>"
                           "<td class=\"slug\"><a href=\"/poll/"
                           (html-attr-escape slug) "\">"
                           (html-escape slug) "</a></td>"
                           "<td class=\"title\">"
                           (html-escape (row-field p "title")) "</td>"
                           "<td class=\"n-options\">"
                           (html-escape (row-field p "n_options")) "</td>"
                           "<td class=\"n-responses\">"
                           (html-escape (row-field p "n_responses")) "</td>"
                           "<td class=\"closes\">"
                           (html-escape (row-field p "closes_at_str")) "</td>"
                           "<td class=\"poll-status-cell\">"
                           (cond (closed? "<span class=\"badge closed\">closed</span>")
                                 (else    "<span class=\"badge open\">open</span>"))
                           "</td>"
                           "<td class=\"created\">"
                           (html-escape (row-field p "created")) "</td>"
                           "<td class=\"acts\">")
                 ;; close / reopen toggle
                 (cond
                   (manual?
                    (out! out "<form method=\"post\" action=\"/polls/"
                              (html-attr-escape slug)
                              "/reopen\" class=\"inline\">"
                              "<button class=\"linkish\">reopen</button>"
                              "</form> "))
                   (else
                    (out! out "<form method=\"post\" action=\"/polls/"
                              (html-attr-escape slug)
                              "/close\" class=\"inline\">"
                              "<button class=\"linkish\">close</button>"
                              "</form> ")))
                 (out! out "<form method=\"post\" action=\"/polls/"
                           (html-attr-escape slug)
                           "/delete\" class=\"inline\" "
                           "data-confirm=\"Delete this poll and all responses?\">"
                           "<button class=\"linkish danger\">delete</button>"
                           "</form></td></tr>")))
             polls)
           (out! out "</tbody></table>")))
        (html-response
          (render-page req auth
                       '((title  . "Polls")
                         (active . polls)
                         (body-class . "feeds-page"))
                       (get-output-string out)))))

    (define (render-admin-new req auth cfg . opt)
      (let ((msg (cond ((pair? opt) (car opt)) (else #f)))
            (out (open-output-string)))
        (out! out "<header class=\"feeds-head\"><h1>New poll</h1>"
                  "<a href=\"/polls\">← back</a></header>")
        (when msg
          (out! out "<p class=\"error\">" (html-escape msg) "</p>"))
        (out! out "<form method=\"post\" action=\"/polls\" class=\"page-edit\">"
                  "<label>Title <input type=\"text\" name=\"title\" required></label>"
                  "<label>Slug (optional) <input type=\"text\" name=\"slug\" "
                  "pattern=\"[a-z0-9_-]{1,64}\" "
                  "placeholder=\"auto: word-word-word\"></label>"
                  "<label>Description "
                  "<textarea name=\"description\" rows=\"3\"></textarea></label>"
                  "<label>Options (one per line)"
                  "<textarea name=\"options\" rows=\"8\" required "
                  "placeholder=\"Mon 2pm&#10;Tue 6pm&#10;Wed 9am\"></textarea></label>"
                  "<label>Closes at (optional) "
                  "<input type=\"datetime-local\" name=\"closes_at\"></label>"
                  "<div class=\"actions\"><button type=\"submit\">Create</button>"
                  "<a href=\"/polls\">Cancel</a></div></form>")
        (html-response
          (render-page req auth
                       '((title  . "New poll")
                         (active . polls)
                         (body-class . "feeds-page editor"))
                       (get-output-string out)))))

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
