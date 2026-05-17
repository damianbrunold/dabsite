(define-library (dabsite tracker)
  (import (scheme base)
          (scheme write)
          (scheme char)
          (scheme cxr)
          (srfi 1)
          (srfi 13)
          (scm database postgres)
          (scm net http request)
          (scm net http response)
          (scm net http route)
          (scm net http forms)
          (scm html builder)
          (scm uri)
          (dabsite db)
          (dabsite auth)
          (dabsite views))
  (export install-tracker-routes!
          ;; exposed for tests
          format-minutes
          parse-minutes
          csv-escape
          csv-row
          split-topics)
  (begin

    ;; ============================================================
    ;; Pure helpers (no DB)
    ;; ============================================================

    (define (format-minutes n)
      ;; n is non-negative integer. Returns "H:MMh".
      (let* ((h (quotient n 60))
             (m (modulo n 60))
             (h-str (number->string h))
             (m-str (number->string m)))
        (string-append h-str ":"
                       (cond ((< m 10) (string-append "0" m-str))
                             (else m-str))
                       "h")))

    (define (parse-minutes s)
      ;; Accepts "90", "1:30", "1h30", "2h", "45m". Returns integer
      ;; minutes ≥ 0, or #f on failure.
      (cond
        ((or (not (string? s)) (string=? (string-trim-both s) "")) #f)
        (else
         (let ((s (string-trim-both s)))
           (cond
             ;; H:MM
             ((string-index s #\:)
              (let* ((idx (string-index s #\:))
                     (h   (string->number (substring s 0 idx)))
                     (m   (string->number (substring s (+ idx 1)
                                                    (string-length s)))))
                (cond
                  ((and h m (integer? h) (integer? m) (>= h 0)
                        (>= m 0) (< m 60))
                   (exact (+ (* h 60) m)))
                  (else #f))))
             ;; "1h30", "1h", "1.5h"
             ((string-index s #\h)
              (let* ((idx (string-index s #\h))
                     (h   (string->number (substring s 0 idx)))
                     (rest (substring s (+ idx 1) (string-length s))))
                (cond
                  ((not (and h (real? h) (>= h 0))) #f)
                  ((string=? rest "")
                   (let ((total (* h 60)))
                     (cond ((and (integer? total) (exact? total)) total)
                           (else (exact (round total))))))
                  ;; Fractional hours can't be combined with an explicit
                  ;; minutes tail.
                  ((not (integer? h)) #f)
                  (else
                   (let ((m (string->number rest)))
                     (cond
                       ((and m (integer? m) (>= m 0) (< m 60))
                        (exact (+ (* h 60) m)))
                       (else #f)))))))
             ;; "45m"
             ((let ((n (string-length s)))
                (and (> n 0) (char=? (string-ref s (- n 1)) #\m)))
              (let ((m (string->number (substring s 0 (- (string-length s) 1)))))
                (cond ((and m (integer? m) (>= m 0)) (exact m))
                      (else #f))))
             ;; plain integer = minutes
             (else
              (let ((n (string->number s)))
                (cond ((and n (integer? n) (>= n 0)) (exact n))
                      (else #f)))))))))

    (define (csv-escape s)
      ;; If the field contains , or " or newlines, wrap in quotes and
      ;; double internal quotes. Otherwise return as-is.
      (cond
        ((not (string? s)) "")
        ((or (string-index s #\,)
             (string-index s #\")
             (string-index s #\newline)
             (string-index s #\return))
         (let* ((n (string-length s))
                (out (open-output-string)))
           (write-char #\" out)
           (let loop ((i 0))
             (cond
               ((= i n) (write-char #\" out) (get-output-string out))
               (else
                (let ((c (string-ref s i)))
                  (when (char=? c #\") (write-char #\" out))
                  (write-char c out)
                  (loop (+ i 1))))))))
        (else s)))

    (define (csv-row fields)
      (string-append (string-join (map csv-escape fields) ",") "\r\n"))

    (define (split-topics s)
      ;; Comma-separated topic names. Trims each, drops empties, lowercases
      ;; nothing — topic names are case-sensitive as the user typed.
      (cond
        ((not (string? s)) '())
        (else
         (filter (lambda (x) (not (string=? x "")))
                 (map string-trim-both
                      (let ((n (string-length s)))
                        (let loop ((i 0) (start 0) (acc '()))
                          (cond
                            ((= i n)
                             (reverse (cons (substring s start n) acc)))
                            ((char=? (string-ref s i) #\,)
                             (loop (+ i 1) (+ i 1)
                                   (cons (substring s start i) acc)))
                            (else (loop (+ i 1) start acc))))))))))

    ;; ============================================================
    ;; DB helpers
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

    ;; --- topics ---

    (define (list-topics cfg include-archived?)
      (alist-rows cfg
        (string-append
          "SELECT id::text AS id, name, "
          "       CASE WHEN archived THEN 'yes' ELSE 'no' END AS archived "
          "FROM tracker_topics "
          (cond (include-archived? "")
                (else "WHERE archived = false "))
          "ORDER BY name")))

    (define (find-topic-id cfg name)
      (let ((rs (rows cfg
                  "SELECT id FROM tracker_topics WHERE name = $1 LIMIT 1"
                  (list name))))
        (cond
          ((pair? rs)
           (string->number (vector-ref (car rs) 0)))
          (else #f))))

    (define (ensure-topic! cfg name)
      ;; Returns the topic id (creates if missing).
      (let ((existing (find-topic-id cfg name)))
        (cond
          (existing existing)
          (else
           (with-db cfg
             (lambda (c)
               (let ((res (pg-query c
                            "INSERT INTO tracker_topics (name) VALUES ($1) RETURNING id"
                            name)))
                 (string->number (vector-ref (car (pg-result-rows res)) 0)))))))))

    (define (create-topic! cfg name)
      (exec cfg
            "INSERT INTO tracker_topics (name) VALUES ($1) ON CONFLICT (name) DO NOTHING"
            (list name)))

    (define (toggle-topic-archived! cfg id)
      (exec cfg
            "UPDATE tracker_topics SET archived = NOT archived WHERE id = $1"
            (list id)))

    (define (delete-topic! cfg id)
      ;; Only deletes if no entries reference this topic — the FK is
      ;; RESTRICT on tracker_done_topics. If it's in use the user must
      ;; archive instead.
      (exec cfg "DELETE FROM tracker_topics WHERE id = $1" (list id)))

    ;; --- entries ---

    (define default-list-window-days 30)

    (define (build-where q topics from-d to-d apply-default-window?)
      ;; Returns (sql . params): WHERE expression text (without leading
      ;; WHERE) and the list of values referenced by its $N placeholders.
      ;; When apply-default-window? is true AND no explicit from-d is set,
      ;; the result is clamped to the last `default-list-window-days`.
      (let ((clauses (list "1=1"))
            (params  '())
            (n 0))
        (define (add-param! v)
          (set! n (+ n 1))
          (set! params (append params (list v)))
          (string-append "$" (number->string n)))
        (when (and q (> (string-length (string-trim-both q)) 0))
          (set! clauses
                (cons (string-append
                        "to_tsvector('simple', d.text) "
                        "@@ plainto_tsquery('simple', " (add-param! q) ")")
                      clauses)))
        (when (pair? topics)
          (set! clauses
                (cons (string-append
                        "d.id IN (SELECT done_id FROM tracker_done_topics "
                        "         WHERE topic_id IN " (add-param! topics) ")")
                      clauses)))
        (cond
          ((and from-d (not (string=? from-d "")))
           (set! clauses
                 (cons (string-append
                         "d.completed >= " (add-param! from-d) "::timestamptz")
                       clauses)))
          (apply-default-window?
           (set! clauses
                 (cons (string-append
                         "d.completed >= now() - interval '"
                         (number->string default-list-window-days)
                         " days'")
                       clauses))))
        (when (and to-d (not (string=? to-d "")))
          (set! clauses
                (cons (string-append
                        "d.completed < (" (add-param! to-d)
                        "::date + 1)::timestamptz")
                      clauses)))
        (cons (string-join clauses " AND ") params)))

    (define (list-entries cfg q topics from-d to-d apply-default-window? limit)
      (let ((where (build-where q topics from-d to-d apply-default-window?)))
        (alist-rows cfg
          (string-append
            "SELECT d.id::text AS id, d.text AS text, "
            "       d.minutes::text AS minutes, "
            "       to_char(d.completed, 'YYYY-MM-DD HH24:MI') AS completed, "
            "       COALESCE(string_agg(t.name, ', ' ORDER BY t.name), '') AS topics "
            "FROM tracker_done d "
            "LEFT JOIN tracker_done_topics dt ON dt.done_id = d.id "
            "LEFT JOIN tracker_topics t ON t.id = dt.topic_id "
            "WHERE " (car where) " "
            "GROUP BY d.id "
            "ORDER BY d.completed DESC, d.id DESC "
            "LIMIT " (number->string limit))
          (cdr where))))

    ;; The summary is always scoped to TODAY regardless of list filters.
    ;; Day boundary is taken at the postgres server's timezone.
    (define today-clause
      (string-append
        "d.completed >= date_trunc('day', now()) "
        "AND d.completed < date_trunc('day', now()) + interval '1 day'"))

    (define (summary-totals cfg)
      ;; Returns alist with keys "total_minutes" and "n_entries". Each
      ;; entry counts ONCE regardless of how many topics it has.
      (let ((rs (alist-rows cfg
                  (string-append
                    "SELECT COALESCE(SUM(minutes), 0)::text AS total_minutes, "
                    "       COUNT(*)::text AS n_entries "
                    "FROM tracker_done d WHERE " today-clause))))
        (cond ((pair? rs) (car rs))
              (else '(("total_minutes" . "0") ("n_entries" . "0"))))))

    (define (summary-by-topic cfg)
      ;; Returns alist rows with topic, total_minutes, entries for TODAY.
      ;; An entry with multiple topics contributes to each of its topics.
      (alist-rows cfg
        (string-append
          "SELECT COALESCE(t.name, '(no topic)') AS topic, "
          "       SUM(d.minutes)::text AS total_minutes, "
          "       COUNT(DISTINCT d.id)::text AS entries "
          "FROM tracker_done d "
          "LEFT JOIN tracker_done_topics dt ON dt.done_id = d.id "
          "LEFT JOIN tracker_topics t ON t.id = dt.topic_id "
          "WHERE " today-clause " "
          "GROUP BY COALESCE(t.name, '(no topic)') "
          "ORDER BY SUM(d.minutes) DESC NULLS LAST")))

    (define (create-entry! cfg text minutes completed-str topic-names)
      ;; completed-str: "YYYY-MM-DDTHH:MM" or "" for now().
      (with-db cfg
        (lambda (c)
          (pg-exec c "BEGIN")
          (guard (exn (#t (guard (e (#t #f)) (pg-exec c "ROLLBACK"))
                          (raise exn)))
            (let* ((use-now? (or (not completed-str) (string=? completed-str "")))
                   (res (cond
                          (use-now?
                           (pg-query c
                             "INSERT INTO tracker_done (text, minutes, completed) VALUES ($1, $2, now()) RETURNING id"
                             text minutes))
                          (else
                           (pg-query c
                             "INSERT INTO tracker_done (text, minutes, completed) VALUES ($1, $2, $3::timestamptz) RETURNING id"
                             text minutes completed-str))))
                   (did (string->number (vector-ref (car (pg-result-rows res)) 0))))
              ;; Topics — ensure each exists, then link.
              (for-each
                (lambda (name)
                  (let ((tid (ensure-topic-c! c name)))
                    (pg-exec c
                      "INSERT INTO tracker_done_topics (done_id, topic_id) VALUES ($1, $2) ON CONFLICT DO NOTHING"
                      did tid)))
                topic-names)
              (pg-exec c "COMMIT")
              did)))))

    (define (ensure-topic-c! c name)
      ;; In-transaction variant of ensure-topic!.
      (let* ((sel (pg-result-rows
                    (pg-query c
                      "SELECT id FROM tracker_topics WHERE name = $1 LIMIT 1"
                      name))))
        (cond
          ((pair? sel) (string->number (vector-ref (car sel) 0)))
          (else
           (let ((res (pg-query c
                        "INSERT INTO tracker_topics (name) VALUES ($1) RETURNING id"
                        name)))
             (string->number (vector-ref (car (pg-result-rows res)) 0)))))))

    (define (delete-entry! cfg id)
      (exec cfg "DELETE FROM tracker_done WHERE id = $1" (list id)))

    (define (find-entry-row cfg id)
      ;; Returns the same alist shape list-entries does, plus a separate
      ;; completed_iso suitable for <input type=datetime-local>.
      (let ((rs (alist-rows cfg
                  (string-append
                    "SELECT d.id::text AS id, d.text AS text, "
                    "       d.minutes::text AS minutes, "
                    "       to_char(d.completed, 'YYYY-MM-DD\"T\"HH24:MI') "
                    "         AS completed_iso, "
                    "       COALESCE(string_agg(t.name, ', ' ORDER BY t.name), '') "
                    "         AS topics "
                    "FROM tracker_done d "
                    "LEFT JOIN tracker_done_topics dt ON dt.done_id = d.id "
                    "LEFT JOIN tracker_topics t ON t.id = dt.topic_id "
                    "WHERE d.id = $1 "
                    "GROUP BY d.id")
                  (list id))))
        (cond ((pair? rs) (car rs)) (else #f))))

    (define (edit-when-string s)
      ;; Pass through; the SQL already formats the value as YYYY-MM-DDTHH:MM
      ;; which is what <input type="datetime-local"> expects.
      (or s ""))

    ;; ============================================================
    ;; Views
    ;; ============================================================

    (define (row-field r k) (let ((p (assoc k r))) (if p (cdr p) "")))

    (define (filters-sxml q topics-selected from-d to-d topics)
      `(form (@ (method "get") (action "/tracker") (class "feed-filters"))
         (input (@ (type "search") (name "q")
                   (placeholder "search text") (value ,(or q ""))))
         (input (@ (type "date") (name "from") (value ,(or from-d ""))))
         (input (@ (type "date") (name "to")   (value ,(or to-d ""))))
         (select (@ (name "topic") (multiple #t) (size "3")
                    (class "tracker-topic-select"))
           ,@(map (lambda (t)
                    (let ((tid (row-field t "id")))
                      `(option (@ (value ,tid)
                                  (selected
                                   ,(if (member tid topics-selected string=?) #t #f)))
                               ,(row-field t "name"))))
                  topics))
         (button (@ (type "submit")) "Apply")
         (a (@ (class "admin-link") (href "/tracker/topics")) "topics")))

    (define (summary-sxml totals per-topic)
      (let* ((total-mins (or (string->number
                               (row-field totals "total_minutes")) 0))
             (n-entries  (row-field totals "n_entries"))
             (entry-word (cond ((equal? n-entries "1") " entry")
                               (else " entries"))))
        `(section (@ (class "tracker-summary"))
           (h2 "Today "
               (span (@ (class "total"))
                     ,(format-minutes total-mins)
                     " across " ,n-entries ,entry-word))
           ,@(cond
               ((null? per-topic) '())
               (else
                `((ul (@ (class "tracker-summary-list"))
                    ,@(map
                       (lambda (s)
                         (let ((mins (or (string->number
                                           (row-field s "total_minutes")) 0)))
                           `(li (span (@ (class "topic"))
                                      ,(row-field s "topic"))
                                (span (@ (class "mins"))
                                      ,(format-minutes mins))
                                (span (@ (class "count"))
                                      ,(row-field s "entries")
                                      ,(raw "&nbsp;") "entr."))))
                       per-topic))))))))

    (define (quick-add-sxml topics prefill)
      ;; prefill is an alist with keys 'text, 'minutes, 'topics, 'when.
      (let ((pf-text    (or (and prefill (assq-ref prefill 'text))    ""))
            (pf-minutes (or (and prefill (assq-ref prefill 'minutes)) ""))
            (pf-topics  (or (and prefill (assq-ref prefill 'topics))  ""))
            (pf-when    (or (and prefill (assq-ref prefill 'when))    "")))
        `(form (@ (method "post") (action "/tracker")
                  (class "feed-new tracker-add") (data-tracker-add #t))
           (h2 "Add")
           (label (@ (class "tracker-what")) "What "
             (textarea (@ (name "text") (required #t) (maxlength "1000")
                          (rows "3") (autofocus #t)
                          (placeholder
                           "What did you do? Prefixes: +topic / -topic, !90m or !1:30, 20260616-1000"))
               ,pf-text))
           (label "Duration "
             (input (@ (type "text") (name "minutes") (required #t)
                       (placeholder "90, 1:30, 45m")
                       (value ,pf-minutes))))
           (label "Topics "
             (input (@ (type "text") (name "topics")
                       (placeholder "comma, separated")
                       (list "tracker-topics-list")
                       (value ,pf-topics))))
           (datalist (@ (id "tracker-topics-list"))
             ,@(map (lambda (t)
                      `(option (@ (value ,(row-field t "name")))))
                    topics))
           (label "When (optional) "
             (input (@ (type "datetime-local") (name "completed")
                       (value ,pf-when))))
           (button (@ (type "submit")) "Log it"))))

    (define (assq-ref alist key)
      (let ((p (assq key alist))) (cond (p (cdr p)) (else #f))))

    (define (entry-row-sxml e)
      (let ((id     (row-field e "id"))
            (text   (row-field e "text"))
            (tops   (row-field e "topics"))
            (mins   (or (string->number (row-field e "minutes")) 0))
            (when-s (row-field e "completed")))
        `(tr
           (td (@ (class "when")) ,when-s)
           (td ,text)
           (td (@ (class "topics")) ,tops)
           (td (@ (class "r")) ,(format-minutes mins))
           (td (@ (class "acts"))
               (form (@ (method "post")
                        (action ,(string-append "/tracker/" id "/edit"))
                        (class "inline"))
                 (button (@ (class "linkish")) "edit"))
               " "
               (form (@ (method "post")
                        (action ,(string-append "/tracker/" id "/delete"))
                        (class "inline")
                        (data-confirm "Delete this entry?"))
                 (button (@ (class "linkish danger")) "delete"))))))

    (define (entry-list-sxml entries)
      (cond
        ((null? entries) `(p (@ (class "empty")) "Nothing matches."))
        (else
         `(table (@ (class "tracker-list mobile-cards"))
            (thead (tr (th "when") (th "what") (th "topics")
                       (th (@ (class "r")) "duration") (th)))
            (tbody ,@(map entry-row-sxml entries))))))

    (define (render-main req auth cfg q topics-selected from-d to-d all? prefill)
      (let* ((all-topics (list-topics cfg #t))
             (active     (filter (lambda (t)
                                   (string=? (row-field t "archived") "no"))
                                 all-topics))
             (apply-window? (and (not all?)
                                 (or (not from-d) (string=? from-d ""))))
             (entries    (list-entries cfg q (map string->number topics-selected)
                                       from-d to-d apply-window? 500))
             (totals     (summary-totals cfg))
             (per-topic  (summary-by-topic cfg))
             (export-href
               (string-append "/tracker/export.csv?"
                              (export-query q topics-selected from-d to-d all?)))
             (window-note
               (cond
                 (apply-window?
                  `(p (@ (class "tracker-window-note"))
                      "Showing last "
                      ,(number->string default-list-window-days)
                      " days. "
                      (a (@ (href ,(string-append
                                     "/tracker?"
                                     (export-query q topics-selected
                                                   from-d to-d #t))))
                         "Show all")
                      "."))
                 (else "")))
             (body
               `((header (@ (class "feeds-head"))
                   (h1 "Tracker")
                   (a (@ (class "admin-link") (href ,export-href))
                      "export CSV"))
                 ,(filters-sxml q topics-selected from-d to-d active)
                 ,(quick-add-sxml active prefill)
                 ,(summary-sxml totals per-topic)
                 ,window-note
                 ,(entry-list-sxml entries))))
        (html-response
          (render-page req auth
                       '((title  . "Tracker")
                         (active . tracker)
                         (body-class . "feeds-page"))
                       (html->string body)))))

    (define (export-query q topics-selected from-d to-d all?)
      ;; Builds a query-string preserving the current filters for the CSV
      ;; export link.
      (let ((parts '()))
        (when (and q (not (string=? q "")))
          (set! parts (cons (string-append "q=" (percent-encode q)) parts)))
        (when (and from-d (not (string=? from-d "")))
          (set! parts (cons (string-append "from=" (percent-encode from-d)) parts)))
        (when (and to-d (not (string=? to-d "")))
          (set! parts (cons (string-append "to=" (percent-encode to-d)) parts)))
        (for-each
          (lambda (t)
            (set! parts (cons (string-append "topic=" (percent-encode t)) parts)))
          topics-selected)
        (when all?
          (set! parts (cons "all=1" parts)))
        (string-join (reverse parts) "&")))

    (define (topic-row-sxml t)
      (let* ((id   (row-field t "id"))
             (name (row-field t "name"))
             (arch (row-field t "archived"))
             (archived? (string=? arch "yes")))
        `(tr (@ (class ,(if archived? "disabled" #f)))
           (td ,name)
           (td ,(if archived? "archived" "active"))
           (td (@ (class "acts"))
               (form (@ (method "post")
                        (action ,(string-append "/tracker/topics/" id "/archive"))
                        (class "inline"))
                 (button (@ (class "linkish"))
                   ,(if archived? "unarchive" "archive")))
               " "
               (form (@ (method "post")
                        (action ,(string-append "/tracker/topics/" id "/delete"))
                        (class "inline")
                        (data-confirm "Delete this topic? Only allowed if no entries reference it."))
                 (button (@ (class "linkish danger")) "delete"))))))

    (define (render-topics-admin req auth cfg)
      (let* ((topics (list-topics cfg #t))
             (body
               `((header (@ (class "feeds-head"))
                   (h1 "Tracker topics")
                   (a (@ (href "/tracker")) ,(raw "← back")))
                 (form (@ (method "post") (action "/tracker/topics")
                          (class "skip-new inline"))
                   (input (@ (type "text") (name "name") (required #t)
                             (placeholder "new topic")))
                   (button (@ (type "submit")) "Add"))
                 (table (@ (class "feed-table"))
                   (thead (tr (th "name") (th "status") (th)))
                   (tbody ,@(map topic-row-sxml topics))))))
        (html-response
          (render-page req auth
                       '((title  . "Tracker topics")
                         (active . tracker)
                         (body-class . "feeds-page"))
                       (html->string body)))))

    ;; ============================================================
    ;; Routes
    ;; ============================================================

    (define (param-or req name default)
      (let ((p (assoc name (url-query-params (http-request-url req)))))
        (cond
          ((and p (string? (cdr p))) (percent-decode (cdr p)))
          (else default))))

    (define (params-all req name)
      ;; Returns all query-string values for the given key (?topic=1&topic=2).
      (filter-map
        (lambda (p)
          (cond
            ((and (string=? (car p) name) (string? (cdr p)))
             (percent-decode (cdr p)))
            (else #f)))
        (url-query-params (http-request-url req))))

    (define (install-tracker-routes! router cfg auth)

      (router-add! router "GET" "/tracker"
        (require-auth auth
          (lambda (req params)
            (let* ((q       (param-or  req "q" ""))
                   (from-d  (param-or  req "from" ""))
                   (to-d    (param-or  req "to" ""))
                   (all?    (string=? (param-or req "all" "") "1"))
                   (tops    (params-all req "topic"))
                   (prefill (list
                              (cons 'text    (param-or req "edit_text" ""))
                              (cons 'minutes (param-or req "edit_minutes" ""))
                              (cons 'topics  (param-or req "edit_topics" ""))
                              (cons 'when    (param-or req "edit_when" "")))))
              (render-main req auth cfg
                           (cond ((string=? q "") #f) (else q))
                           tops
                           (cond ((string=? from-d "") #f) (else from-d))
                           (cond ((string=? to-d "") #f) (else to-d))
                           all?
                           prefill)))))

      (router-add! router "POST" "/tracker"
        (require-auth auth
          (lambda (req params)
            (let* ((form      (parse-www-form (or (http-request-body req) "")))
                   (text      (string-trim-both (form-ref form "text" "")))
                   (mins-raw  (form-ref form "minutes" ""))
                   (mins      (parse-minutes mins-raw))
                   (when-s    (string-trim-both (form-ref form "completed" "")))
                   (topics-s  (form-ref form "topics" ""))
                   (topic-names (split-topics topics-s)))
              (cond
                ((string=? text "")
                 (render-error 400 "Text is required."))
                ((not mins)
                 (render-error 400 "Duration is not understood (try 90 or 1:30)."))
                (else
                 (create-entry! cfg text mins when-s topic-names)
                 (make-http-response 302
                   (list (cons "Location" "/tracker")) "")))))))

      (router-add! router "POST" "/tracker/:id/edit"
        (require-auth auth
          (lambda (req params)
            (let* ((id  (string->number (params-ref params "id")))
                   (row (and id (find-entry-row cfg id))))
              (cond
                ((not row) (render-error 404 "Entry not found."))
                (else
                 (let* ((text    (row-field row "text"))
                        (minutes (or (string->number
                                       (row-field row "minutes")) 0))
                        (topics  (row-field row "topics"))
                        (when-s  (edit-when-string
                                   (row-field row "completed_iso"))))
                   (delete-entry! cfg id)
                   (make-http-response 302
                     (list (cons "Location"
                                 (string-append
                                   "/tracker?edit_text="
                                   (percent-encode text)
                                   "&edit_minutes="
                                   (percent-encode (number->string minutes))
                                   "&edit_topics="
                                   (percent-encode topics)
                                   "&edit_when="
                                   (percent-encode when-s))))
                     ""))))))))

      (router-add! router "POST" "/tracker/:id/delete"
        (require-auth auth
          (lambda (req params)
            (let ((id (string->number (params-ref params "id"))))
              (when id (delete-entry! cfg id))
              (make-http-response 302
                (list (cons "Location" "/tracker")) "")))))

      ;; --- CSV export ---
      (router-add! router "GET" "/tracker/export.csv"
        (require-auth auth
          (lambda (req params)
            (let* ((q      (param-or  req "q" ""))
                   (from-d (param-or  req "from" ""))
                   (to-d   (param-or  req "to" ""))
                   (all?   (string=? (param-or req "all" "") "1"))
                   (tops   (params-all req "topic"))
                   (apply-window? (and (not all?) (string=? from-d "")))
                   (entries (list-entries cfg
                              (cond ((string=? q "") #f) (else q))
                              (map string->number tops)
                              (cond ((string=? from-d "") #f) (else from-d))
                              (cond ((string=? to-d "") #f) (else to-d))
                              apply-window?
                              10000))
                   (out    (open-output-string)))
              (write-string (csv-row '("completed" "minutes" "topics" "text"))
                            out)
              (for-each
                (lambda (e)
                  (write-string
                    (csv-row (list (row-field e "completed")
                                   (row-field e "minutes")
                                   (row-field e "topics")
                                   (row-field e "text")))
                    out))
                entries)
              (make-http-response 200
                (list (cons "Content-Type" "text/csv; charset=utf-8")
                      (cons "Content-Disposition"
                            "attachment; filename=\"tracker.csv\""))
                (get-output-string out))))))

      ;; --- Topics admin ---
      (router-add! router "GET" "/tracker/topics"
        (require-auth auth
          (lambda (req params) (render-topics-admin req auth cfg))))

      (router-add! router "POST" "/tracker/topics"
        (require-auth auth
          (lambda (req params)
            (let* ((form (parse-www-form (or (http-request-body req) "")))
                   (name (string-trim-both (form-ref form "name" ""))))
              (cond
                ((string=? name "")
                 (render-error 400 "Name is required."))
                (else
                 (create-topic! cfg name)
                 (make-http-response 302
                   (list (cons "Location" "/tracker/topics")) "")))))))

      (router-add! router "POST" "/tracker/topics/:id/archive"
        (require-auth auth
          (lambda (req params)
            (let ((id (string->number (params-ref params "id"))))
              (when id (toggle-topic-archived! cfg id))
              (make-http-response 302
                (list (cons "Location" "/tracker/topics")) "")))))

      (router-add! router "POST" "/tracker/topics/:id/delete"
        (require-auth auth
          (lambda (req params)
            (let ((id (string->number (params-ref params "id"))))
              (when id
                (guard (exn (#t #f))
                  (delete-topic! cfg id)))
              (make-http-response 302
                (list (cons "Location" "/tracker/topics")) ""))))))

))
