(define-library (dabsite feeds)
  (import (scheme base)
          (scheme char)
          (scheme write)
          (scheme time)
          (scheme cxr)
          (srfi 1)
          (srfi 13)
          (srfi 18)
          (scm database postgres)
          (scm net http request)
          (scm net http response)
          (scm net http route)
          (scm net http forms)
          (scm html)
          (scm html builder)
          (scm uri)
          (scm log)
          (scm duration)
          (scm datetime)
          (dabsite db)
          (dabsite auth)
          (dabsite views)
          (dabsite feeds-fetcher))
  (export install-feed-routes!
          start-feed-scheduler!
          ;; exposed for unit tests
          round-robin-by-label)
  (begin

    ;; ==============================================================
    ;; DB ops. All SQL flows through (dabsite db) helpers.
    ;; ==============================================================

    (define (rows cfg sql . maybe-params)
      (let ((params (if (null? maybe-params) '() (car maybe-params))))
        (with-db cfg
          (lambda (c)
            (pg-result-rows
              (cond ((null? params) (pg-query c sql))
                    (else (pg-query c (pg-format-sql sql params)))))))))

    (define (alist-rows cfg sql . maybe-params)
      (let ((params (if (null? maybe-params) '() (car maybe-params))))
        (with-db cfg
          (lambda (c)
            (pg-result->alist-list
              (cond ((null? params) (pg-query c sql))
                    (else (pg-query c (pg-format-sql sql params)))))))))

    (define (exec cfg sql . maybe-params)
      (let ((params (if (null? maybe-params) '() (car maybe-params))))
        (with-db cfg
          (lambda (c)
            (cond ((null? params) (pg-exec c sql))
                  (else (pg-exec c (pg-format-sql sql params))))))))

    ;; --- entries listing ---

    (define (build-filter q category show-all?)
      ;; Returns a SQL WHERE clause (without leading WHERE) and ORDER BY.
      (let ((clauses (list "1=1")))
        (when (not show-all?)
          (set! clauses (cons "fe.read_at IS NULL" clauses)))
        (when (and category (not (string=? category "")))
          (set! clauses
                (cons (string-append "f.category = "
                                     (pg-quote-literal category))
                      clauses)))
        (when (and q (> (string-length (string-trim-both q)) 0))
          (set! clauses
                (cons (string-append
                        "to_tsvector('simple', fe.title || ' ' || fe.summary) "
                        "@@ plainto_tsquery('simple', "
                        (pg-quote-literal q) ")")
                      clauses)))
        (string-join clauses " AND ")))

    (define summary-preview-chars 200)

    (define (list-entries cfg q category show-all?)
      ;; Returns alist rows with: id, label, category, title, link, summary,
      ;; published (formatted), read.
      ;;
      ;; Ordering is by fetched_at (when we first saw the entry) so a feed
      ;; can't pop back to the top by rewriting its own published_at field
      ;; — the dedup layer collapses republished titles before they even
      ;; get a fresh fetched_at.
      ;;
      ;; There is no per-category cap: round-robin in the renderer
      ;; interleaves labels for visual balance, and the user marks
      ;; entries read as they triage them, which naturally shrinks the
      ;; page. min_entries on a feed is now effectively a no-op but the
      ;; column is retained for forward compatibility.
      (let ((sql (string-append
                   "SELECT fe.id::text AS id, f.label AS label, "
                   "  f.category AS category, fe.title AS title, "
                   "  fe.link AS link, "
                   "  substring(fe.summary, 1, "
                   (number->string summary-preview-chars) ") AS summary, "
                   "  to_char(fe.fetched_at, 'YYYY-MM-DD HH24:MI') AS published, "
                   "  CASE WHEN fe.read_at IS NULL THEN 'no' ELSE 'yes' END AS read "
                   "FROM feed_entries fe JOIN feeds f ON f.id = fe.feed_id "
                   "WHERE " (build-filter q category show-all?) " "
                   "ORDER BY fe.fetched_at DESC, fe.id ASC")))
        (alist-rows cfg sql)))

    ;; Returns category names ordered by the categories table (lowest
    ;; sort_order first, then alphabetical). Feeds whose category is not
    ;; yet in the table are still listed; the LEFT JOIN substitutes a
    ;; large default order so they appear at the end.
    (define (list-categories cfg)
      ;; SELECT DISTINCT requires the sort key to appear in the projection,
      ;; so we include sort_order in the select and pull the first column.
      (map (lambda (row) (vector-ref row 0))
           (rows cfg
             (string-append
               "SELECT DISTINCT f.category, "
               "       COALESCE(c.sort_order, 1000) AS sort_order "
               "FROM feeds f LEFT JOIN categories c ON c.name = f.category "
               "ORDER BY 2, 1"))))

    (define (list-category-orders cfg)
      ;; All categories known to the categories table + any orphans from
      ;; feeds. Used by the admin UI to edit sort_order.
      (alist-rows cfg
        (string-append
          "SELECT name, sort_order::text AS sort_order FROM ("
          "  SELECT c.name AS name, c.sort_order AS sort_order "
          "  FROM categories c "
          "  UNION ALL "
          "  SELECT DISTINCT f.category AS name, 1000 AS sort_order "
          "  FROM feeds f "
          "  WHERE f.category NOT IN (SELECT name FROM categories)"
          ") x ORDER BY sort_order, name")))

    (define (count-unread cfg)
      ;; Per-category map: alist (category . count).
      (let ((alist (alist-rows cfg
                     (string-append
                       "SELECT f.category AS category, "
                       "       COUNT(*)::text AS n "
                       "FROM feed_entries fe JOIN feeds f ON f.id = fe.feed_id "
                       "WHERE fe.read_at IS NULL "
                       "GROUP BY f.category"))))
        (map (lambda (r)
               (cons (cdr (assoc "category" r))
                     (string->number (cdr (assoc "n" r)))))
             alist)))

    ;; --- mark read/unread ---

    (define (mark-entry-read! cfg id)
      (exec cfg "UPDATE feed_entries SET read_at = now() WHERE id = $1"
            (list id)))

    (define (mark-entry-unread! cfg id)
      (exec cfg "UPDATE feed_entries SET read_at = NULL WHERE id = $1"
            (list id)))

    (define (mark-all-read! cfg category)
      (cond
        ((and category (not (string=? category "")))
         (exec cfg
               (string-append
                 "UPDATE feed_entries SET read_at = now() "
                 "WHERE read_at IS NULL AND feed_id IN "
                 "(SELECT id FROM feeds WHERE category = $1)")
               (list category)))
        (else
         (exec cfg
               "UPDATE feed_entries SET read_at = now() WHERE read_at IS NULL"))))

    (define (mark-older-than! cfg category days)
      ;; Marks unread entries older than `days` days as read. The cutoff
      ;; is based on fetched_at — the same field that drives display
      ;; ordering — so what looks "old" in the list is what gets dismissed.
      (cond
        ((and category (not (string=? category "")))
         (exec cfg
               (string-append
                 "UPDATE feed_entries SET read_at = now() "
                 "WHERE read_at IS NULL "
                 "AND feed_id IN (SELECT id FROM feeds WHERE category = $1) "
                 "AND fetched_at < now() - make_interval(days => $2)")
               (list category days)))
        (else
         (exec cfg
               (string-append
                 "UPDATE feed_entries SET read_at = now() "
                 "WHERE read_at IS NULL "
                 "AND fetched_at < now() - make_interval(days => $1)")
               (list days)))))

    ;; Cap of read entries kept per label. Unread entries are never pruned.
    ;; Feeds without a label are bucketed per-feed so an unlabelled spammy
    ;; feed can't crowd out an unlabelled quiet one.
    (define read-entries-per-label-cap 100)

    (define (prune-read-entries! cfg)
      (exec cfg
        (string-append
          "DELETE FROM feed_entries WHERE id IN ("
          "  SELECT id FROM ("
          "    SELECT fe.id, row_number() OVER ("
          "      PARTITION BY COALESCE(NULLIF(f.label, ''), "
          "                            'feed#' || f.id::text) "
          "      ORDER BY fe.read_at DESC, fe.id DESC) AS rn "
          "    FROM feed_entries fe JOIN feeds f ON f.id = fe.feed_id "
          "    WHERE fe.read_at IS NOT NULL"
          "  ) t WHERE rn > " (number->string read-entries-per-label-cap) ")")))

    (define (update-category-order! cfg name sort-order)
      ;; Upserts a category row. Used by the admin UI to reorder.
      (exec cfg
        (string-append
          "INSERT INTO categories (name, sort_order) VALUES ($1, $2) "
          "ON CONFLICT (name) DO UPDATE SET sort_order = EXCLUDED.sort_order")
        (list name sort-order)))

    ;; --- admin (feeds CRUD) ---

    (define (list-feeds cfg)
      (alist-rows cfg
        (string-append
          "SELECT f.id::text AS id, f.url AS url, f.title AS title, "
          "       f.label AS label, f.category AS category, "
          "       f.refresh_seconds::text AS refresh_seconds, "
          "       f.min_entries::text AS min_entries, "
          "       f.failure_count::text AS failure_count, "
          "       CASE WHEN f.enabled THEN 'yes' ELSE 'no' END AS enabled, "
          "       COALESCE(to_char(f.last_fetched_at, 'YYYY-MM-DD HH24:MI'), '') AS last_fetched, "
          "       COALESCE(f.last_error, '') AS last_error "
          "FROM feeds f "
          "LEFT JOIN categories c ON c.name = f.category "
          "ORDER BY COALESCE(c.sort_order, 1000), f.category, f.title")))

    (define (ensure-category! cfg category)
      (exec cfg
            (string-append
              "INSERT INTO categories (name, sort_order) VALUES ($1, 100) "
              "ON CONFLICT (name) DO NOTHING")
            (list category)))

    (define (add-feed! cfg url title label category refresh-seconds)
      (ensure-category! cfg category)
      (exec cfg
        (string-append
          "INSERT INTO feeds (url, title, label, category, refresh_seconds) "
          "VALUES ($1, $2, $3, $4, $5) "
          "ON CONFLICT (url) DO NOTHING")
        (list url title label category refresh-seconds)))

    (define (delete-feed! cfg id)
      (exec cfg "DELETE FROM feeds WHERE id = $1" (list id)))

    (define (toggle-feed! cfg id)
      (exec cfg "UPDATE feeds SET enabled = NOT enabled WHERE id = $1"
            (list id)))

    (define (force-refresh! cfg id)
      (exec cfg
            (string-append
              "UPDATE feeds SET last_fetched_at = NULL, failure_count = 0 "
              "WHERE id = $1")
            (list id)))

    (define (set-refresh-seconds! cfg id refresh-seconds)
      (exec cfg "UPDATE feeds SET refresh_seconds = $1 WHERE id = $2"
            (list refresh-seconds id)))

    (define (set-min-entries! cfg id n)
      (exec cfg "UPDATE feeds SET min_entries = $1 WHERE id = $2"
            (list n id)))

    ;; --- skip patterns ---

    (define (list-skip-patterns cfg)
      (alist-rows cfg
        (string-append
          "SELECT id::text AS id, pattern, kind "
          "FROM feed_skip_patterns ORDER BY kind, pattern")))

    (define (load-skip-patterns cfg)
      ;; Returns a list of (kind . pattern) pairs, both strings.
      (let ((rs (rows cfg
                  "SELECT kind, pattern FROM feed_skip_patterns")))
        (map (lambda (row) (cons (vector-ref row 0) (vector-ref row 1)))
             rs)))

    (define (add-skip-pattern! cfg pattern kind)
      (exec cfg
        (string-append
          "INSERT INTO feed_skip_patterns (pattern, kind) VALUES ($1, $2) "
          "ON CONFLICT (kind, pattern) DO NOTHING")
        (list pattern kind)))

    (define (delete-skip-pattern! cfg id)
      (exec cfg "DELETE FROM feed_skip_patterns WHERE id = $1" (list id)))

    (define (title-matches-skip? title patterns)
      ;; patterns is (kind . pattern) list. Returns #t if any matches.
      ;; Comparison is case-insensitive against the trimmed title.
      (let ((tt (string-trim-both title)))
        (let loop ((ps patterns))
          (cond
            ((null? ps) #f)
            (else
             (let* ((p    (car ps))
                    (kind (car p))
                    (pat  (cdr p)))
               (cond
                 ((string=? kind "prefix")
                  (cond
                    ((string-prefix-ci? pat tt) #t)
                    (else (loop (cdr ps)))))
                 ((string=? kind "contains")
                  (cond
                    ((string-contains-ci tt pat) #t)
                    (else (loop (cdr ps)))))
                 (else (loop (cdr ps))))))))))

    ;; ==============================================================
    ;; Scheduler
    ;; ==============================================================

    ;; The effective refresh interval is the configured refresh_seconds
    ;; multiplied by 2^min(failure_count, 6). That caps the backoff at 64×
    ;; the configured interval, so a hard-broken feed at 1h refresh tops
    ;; out around 64h between attempts rather than hammering the upstream.
    (define (list-due-feeds cfg)
      (alist-rows cfg
        (string-append
          "SELECT id::text AS id, url, title "
          "FROM feeds WHERE enabled = true "
          "AND (last_fetched_at IS NULL "
          "  OR EXTRACT(EPOCH FROM (now() - last_fetched_at)) >= "
          "     refresh_seconds * power(2, LEAST(failure_count, 6))) "
          "ORDER BY last_fetched_at NULLS FIRST LIMIT 5")))

    (define (mark-feed-result! cfg id error-msg)
      ;; On success: reset failure_count to 0; on failure: increment it
      ;; so the next due-check applies the backoff multiplier above.
      (cond
        (error-msg
         (exec cfg
               (string-append
                 "UPDATE feeds SET last_fetched_at = now(), last_error = $1, "
                 "failure_count = failure_count + 1 WHERE id = $2")
               (list error-msg id)))
        (else
         (exec cfg
               (string-append
                 "UPDATE feeds SET last_fetched_at = now(), last_error = NULL, "
                 "failure_count = 0 WHERE id = $1")
               (list id)))))

    (define (assoc-val alist key)
      (let ((p (assoc key alist))) (if p (cdr p) #f)))

    (define (title-key title)
      ;; Mirror the generated-column expression in the DB so the membership
      ;; test compares the same canonical form. Lowercase + collapse all
      ;; whitespace runs to single spaces; we don't trim because the
      ;; generated column doesn't either (only collapses runs).
      (let* ((s    (or title ""))
             (n    (string-length s))
             (out  (open-output-string)))
        (let loop ((i 0) (in-ws? #f))
          (cond
            ((= i n) (get-output-string out))
            (else
             (let ((c (string-ref s i)))
               (cond
                 ((or (char=? c #\space) (char=? c #\tab)
                      (char=? c #\newline) (char=? c #\return))
                  (when (not in-ws?) (write-char #\space out))
                  (loop (+ i 1) #t))
                 (else
                  (write-char (char-downcase c) out)
                  (loop (+ i 1) #f)))))))))

    (define dedup-window-days 30)

    (define (recent-title-keys cfg keys)
      ;; Returns the set (as a list) of title_keys already present in the
      ;; last `dedup-window-days` days across ALL feeds. We don't exclude
      ;; the calling feed: that way a same-feed republish with a fresh
      ;; guid (different date, same title) is also collapsed.
      (cond
        ((null? keys) '())
        (else
         (let ((rs (rows cfg
                     (string-append
                       "SELECT DISTINCT title_key FROM feed_entries "
                       "WHERE title_key <> '' "
                       "AND title_key IN $1 "
                       "AND fetched_at > now() - make_interval(days => $2)")
                     (list keys dedup-window-days))))
           (map (lambda (row) (vector-ref row 0)) rs)))))

    (define (member-string? s xs)
      (cond ((null? xs) #f)
            ((string=? (car xs) s) #t)
            (else (member-string? s (cdr xs)))))

    (define (entries->insert-row e feed-id)
      ;; Returns the values tuple "(feed_id, 'guid', 'title', 'link',
      ;; 'summary', to_timestamp(N))" for one entry, or #f if the entry
      ;; lacks the bare minimum (a usable guid).
      (let* ((guid (or (assoc-val e "guid")
                       (assoc-val e "link")
                       (assoc-val e "title")))
             (pubs (assoc-val e "published"))
             (unix (and pubs (parse-pubdate pubs))))
        (cond
          ((or (not guid) (string=? guid "")) #f)
          (else
           (string-append
             "(" (pg-quote-int feed-id) ", "
             (pg-quote-literal guid) ", "
             (pg-quote-literal (or (assoc-val e "title") "")) ", "
             (pg-quote-literal (or (assoc-val e "link") "")) ", "
             (pg-quote-literal (or (assoc-val e "summary") "")) ", "
             (cond
               (unix (string-append "to_timestamp("
                                    (pg-quote-int unix) ")"))
               (else "NULL::timestamptz"))
             ")")))))

    (define (upsert-entries! cfg feed-id entries)
      ;; Three filters before INSERT:
      ;;   1. title-prefix/contains skip patterns from feed_skip_patterns
      ;;   2. cross-feed / recent-republish dedup by title_key
      ;;   3. ON CONFLICT (feed_id, guid) DO NOTHING for same-feed re-fetch
      (when (pair? entries)
        (let* ((skips     (load-skip-patterns cfg))
               (kept-1    (filter (lambda (e)
                                    (not (title-matches-skip?
                                           (or (assoc-val e "title") "")
                                           skips)))
                                  entries))
               (keys      (map (lambda (e) (title-key (or (assoc-val e "title") "")))
                               kept-1))
               (dups      (recent-title-keys cfg
                                             (filter (lambda (k)
                                                       (not (string=? k "")))
                                                     keys)))
               (kept-2    (filter
                            (lambda (pair)
                              (let ((k (car pair)))
                                (or (string=? k "")
                                    (not (member-string? k dups)))))
                            (map cons keys kept-1)))
               (rows-sql  (map (lambda (pair)
                                 (entries->insert-row (cdr pair) feed-id))
                               kept-2))
               (rows-sql  (filter (lambda (x) x) rows-sql)))
          (cond
            ((null? rows-sql) #t)
            (else
             (exec cfg
               (string-append
                 "INSERT INTO feed_entries "
                 "(feed_id, guid, title, link, summary, published_at) VALUES "
                 (string-join rows-sql ", ")
                 " ON CONFLICT (feed_id, guid) DO NOTHING")))))))

    (define (tick! cfg)
      (let ((due (list-due-feeds cfg)))
        (for-each
          (lambda (feed)
            (let* ((id    (cdr (assoc "id"    feed)))
                   (url   (cdr (assoc "url"   feed)))
                   (title (cdr (assoc "title" feed)))
                   (id-n  (string->number id)))
              (log-info "feeds"
                (string-append "fetching " title " (" url ")"))
              (let ((r (fetch-feed url)))
                (cond
                  ((fetch-result-ok? r)
                   (guard (exn (#t
                                 (let ((msg (string-append
                                              "db error during upsert: "
                                              (cond ((string? exn) exn)
                                                    (else "exception")))))
                                   (log-error "feeds"
                                     (string-append title ": " msg))
                                   (mark-feed-result! cfg id-n msg))))
                     (upsert-entries! cfg id-n (fetch-result-entries r))
                     (mark-feed-result! cfg id-n #f)
                     (log-info "feeds" (string-append "ok " title))))
                  (else
                   (mark-feed-result! cfg id-n (fetch-result-error r))
                   (log-warn "feeds"
                     (string-append "error " title " — "
                                    (fetch-result-error r))))))))
          due)
        (guard (exn (#t (log-error "feeds"
                          "prune-read-entries! failed; continuing")))
          (prune-read-entries! cfg))))

    (define scheduler-tick-seconds 30)

    (define (scheduler-loop cfg)
      (let loop ()
        (guard (exn (#t (log-error "feeds"
                          "scheduler tick raised; continuing")))
          (tick! cfg))
        (thread-sleep! scheduler-tick-seconds)
        (loop)))

    (define (start-feed-scheduler! cfg)
      (thread-start!
        (make-thread (lambda () (scheduler-loop cfg)) "feeds-scheduler"))
      (log-info "feeds"
        (string-append "scheduler thread started (tick every "
                       (number->string scheduler-tick-seconds) "s)")))

    ;; ==============================================================
    ;; Views
    ;; ==============================================================

    (define (row-field r k) (let ((p (assoc k r))) (if p (cdr p) "")))

    (define (feed-entry-sxml row)
      ;; Returns the SXML for one feed entry. The tooltip combines the
      ;; publication date and the (truncated, plain-text) summary so
      ;; hovering surfaces both without consuming column width.
      (let* ((id    (row-field row "id"))
             (label (row-field row "label"))
             (cat   (row-field row "category"))
             (title (row-field row "title"))
             (link  (row-field row "link"))
             (sumr  (strip-html-tags (row-field row "summary")))
             (pub   (row-field row "published"))
             (read  (row-field row "read"))
             (read? (string=? read "yes"))
             (tip   (cond
                      ((and (not (string=? pub  ""))
                            (not (string=? sumr "")))
                       (string-append pub " — " sumr))
                      ((not (string=? pub  "")) pub)
                      (else sumr))))
        `(li (@ (class ,(string-append "feed-entry"
                                       (if read? " is-read" "")))
                (data-id ,id)
                (data-cat ,cat))
             (form (@ (method "post")
                      (action ,(string-append "/feeds/entry/" id "/"
                                              (if read? "unread" "read")))
                      (class "mark"))
                (button (@ (type "submit")
                           (title ,(if read? "mark unread" "mark read")))
                  ,(if read? "↩" "✓")))
             (a (@ (class "entry-link")
                   (href ,link)
                   (target "_blank")
                   (rel "noopener")
                   (title ,tip))
                ,(if (string=? label "")
                     ""
                     `(span (@ (class "label")) ,label))
                ,(if (string=? label "") "" " ")
                ,title))))

    (define (entries-by-category entries)
      ;; Returns alist (category . entries-in-order).
      (let loop ((es entries) (acc '()))
        (cond
          ((null? es) (reverse (map (lambda (p) (cons (car p) (reverse (cdr p))))
                                    acc)))
          (else
           (let* ((cat (row-field (car es) "category"))
                  (p   (assoc cat acc)))
             (cond
               (p (set-cdr! p (cons (car es) (cdr p)))
                  (loop (cdr es) acc))
               (else (loop (cdr es)
                           (cons (cons cat (list (car es))) acc)))))))))

    (define (round-robin-by-label entries)
      ;; Reorders entries so labels alternate within each pass; within a
      ;; label the original (time-sorted) order is preserved. Feeds that
      ;; share a label (e.g. several Guardian feeds all tagged "G") share
      ;; a slot, which is the intent.
      ;;
      ;; Algorithm: bucket by label (in first-seen order), then repeatedly
      ;; take the head from each non-empty bucket until all are drained.
      (let ((buckets '()))
        (for-each
          (lambda (e)
            (let* ((lab (row-field e "label"))
                   (p   (assoc lab buckets)))
              (cond
                (p (set-cdr! p (cons e (cdr p))))
                (else
                 ;; preserve first-seen order — append, not prepend
                 (set! buckets
                       (append buckets (list (cons lab (list e)))))))))
          entries)
        ;; Each bucket built via cons is in reverse; flip them back.
        (let ((qs (map (lambda (b) (cons (car b) (reverse (cdr b))))
                       buckets)))
          (let outer ((qs qs) (acc '()))
            (cond
              ((null? qs) (reverse acc))
              (else
               (let inner ((qs qs) (next '()) (acc acc))
                 (cond
                   ((null? qs)
                    (outer (reverse next) acc))
                   (else
                    (let* ((q     (car qs))
                           (queue (cdr q)))
                      (cond
                        ((null? queue)
                         (inner (cdr qs) next acc))
                        (else
                         (inner (cdr qs)
                                (cons (cons (car q) (cdr queue)) next)
                                (cons (car queue) acc))))))))))))))

    (define (category-section-sxml cat-entries category)
      (let ((cat (car cat-entries))
            (es  (cdr cat-entries)))
        `(section (@ (class "feed-cat") (data-cat ,cat))
           (header
             (h2 ,cat)
             (form (@ (method "post")
                      (action "/feeds/mark-all-read")
                      (class "inline"))
               (input (@ (type "hidden") (name "cat") (value ,cat)))
               (button (@ (type "submit") (class "linkish")
                          (title "mark all read in this category"))
                 "mark all read")))
           (ul (@ (class "feed-entries"))
             ,@(map feed-entry-sxml es)))))

    (define (sort-grouped-by-cats grouped cats)
      ;; cats is the list of category names in display order; grouped is
      ;; ((cat . entries) ...) in arbitrary order. Returns grouped sorted
      ;; so cats found earlier in `cats` come first. Categories present
      ;; in grouped but missing from cats are appended at the end, in
      ;; their original order.
      (let* ((in-cats (filter-map
                        (lambda (c) (assoc c grouped))
                        cats))
             (extras  (filter (lambda (p)
                                (not (member (car p) cats string=?)))
                              grouped)))
        (append in-cats extras)))

    (define (category-option-sxml unread current-category c)
      (let* ((n (cdr (or (assoc c unread) (cons "" 0))))
             (label (cond ((> n 0)
                           (string-append c " (" (number->string n) ")"))
                          (else c))))
        `(option (@ (value ,c)
                    (selected ,(and current-category (string=? current-category c) #t)))
                 ,label)))

    (define (render-feeds-page req auth cfg q category show-all?)
      (let* ((entries (list-entries cfg q category show-all?))
             (cats    (list-categories cfg))
             ;; Group → round-robin within each category → order
             ;; categories by configured sort_order.
             (by-cat  (entries-by-category entries))
             (by-cat  (map (lambda (p)
                             (cons (car p) (round-robin-by-label (cdr p))))
                           by-cat))
             (grouped (sort-grouped-by-cats by-cat cats))
             (unread  (count-unread cfg))
             (body
               `((header (@ (class "feeds-head"))
                   (h1 "Feeds "
                       (span (@ (class "qual"))
                             ,(if show-all? "archive" "unread")))
                   ;; Filter form: search + category select +
                   ;; show-archive + admin link.
                   (form (@ (method "get") (action "/feeds")
                            (class "feed-filters"))
                     (input (@ (type "search") (name "q")
                               (placeholder "search archive")
                               (value ,(or q ""))))
                     (select (@ (name "cat"))
                       (option (@ (value "")) "all categories")
                       ,@(map (lambda (c)
                                (category-option-sxml unread category c))
                              cats))
                     (label (@ (class "checkbox"))
                       (input (@ (type "checkbox") (name "all") (value "1")
                                 (checked ,(if show-all? #t #f))))
                       "show archive")
                     (button (@ (type "submit")) "Apply")
                     (a (@ (class "admin-link") (href "/feeds/admin"))
                        "admin"))
                   ;; Mark older-than form. Carries the active category
                   ;; filter so a user looking at e.g. only Tech can
                   ;; sweep just Tech.
                   (form (@ (method "post") (action "/feeds/mark-older")
                            (class "feed-mark-older"))
                     ,(if category
                          `(input (@ (type "hidden") (name "cat")
                                     (value ,category)))
                          "")
                     (label "mark read older than "
                       (input (@ (type "number") (name "days") (min "1")
                                 (value "2") (inputmode "numeric")))
                       " days")
                     (button (@ (type "submit") (class "linkish")) "go")))
                 ,(cond
                    ((null? grouped)
                     `(p (@ (class "empty"))
                         "Nothing to read."
                         ,@(if show-all?
                               '()
                               `(" " (a (@ (href "/feeds?all=1"))
                                        "Show archive")
                                 "."))))
                    (else
                     `(div (@ (class "feed-grid"))
                           ,@(map (lambda (ce)
                                    (category-section-sxml ce category))
                                  grouped)))))))
        (html-response
          (render-page req auth
                       (list (cons 'title  "Feeds")
                             (cons 'active 'feeds)
                             (cons 'body-class
                                   (cond
                                     (category "feeds-page feeds-page-single")
                                     (else     "feeds-page"))))
                       (html->string body)))))

    (define (cat-order-row-sxml c)
      (let ((name  (row-field c "name"))
            (order (row-field c "sort_order")))
        `(tr (form (@ (method "post") (action "/feeds/admin/category-order"))
               (td ,name
                   (input (@ (type "hidden") (name "name") (value ,name))))
               (td (input (@ (type "number") (name "sort_order")
                             (value ,order) (min "0") (max "10000"))))
               (td (button (@ (type "submit") (class "linkish")) "save"))))))

    (define (skip-row-sxml sp)
      (let ((sid  (row-field sp "id"))
            (kind (row-field sp "kind"))
            (pat  (row-field sp "pattern")))
        `(tr (td ,kind)
             (td ,pat)
             (td (form (@ (method "post")
                          (action ,(string-append "/feeds/admin/skip-pattern/"
                                                  sid "/delete"))
                          (class "inline"))
                   (button (@ (type "submit") (class "linkish danger"))
                     "remove"))))))

    (define (feed-row-sxml f)
      (let* ((id      (row-field f "id"))
             (url     (row-field f "url"))
             (title   (row-field f "title"))
             (label   (row-field f "label"))
             (cat     (row-field f "category"))
             (refresh (row-field f "refresh_seconds"))
             (refresh-int (or (string->number refresh) 0))
             (pin     (row-field f "min_entries"))
             (fails   (row-field f "failure_count"))
             (enabled (row-field f "enabled"))
             (last    (row-field f "last_fetched"))
             (err     (row-field f "last_error"))
             (base    (string-append "/feeds/admin/" id)))
        `(tr (@ (class ,(if (string=? enabled "no") "disabled" #f)))
           (td ,id) (td ,cat) (td ,label) (td ,title)
           (td (@ (class "url")) ,url)
           (td (form (@ (method "post")
                        (action ,(string-append base "/refresh-interval"))
                        (class "inline"))
                 (input (@ (type "text") (name "refresh") (size "5")
                           (value ,(format-duration refresh-int))))
                 (button (@ (type "submit") (class "linkish")) "↵")))
           (td (form (@ (method "post")
                        (action ,(string-append base "/min-entries"))
                        (class "inline"))
                 (input (@ (type "number") (name "min_entries") (size "3")
                           (min "0") (max "50") (value ,pin)))
                 (button (@ (type "submit") (class "linkish")) "↵")))
           (td ,last) (td ,fails) (td (@ (class "err")) ,err)
           (td (@ (class "acts"))
               (form (@ (method "post") (action ,(string-append base "/toggle"))
                        (class "inline"))
                 (button (@ (class "linkish"))
                   ,(if (string=? enabled "yes") "disable" "enable")))
               " "
               (form (@ (method "post") (action ,(string-append base "/refresh"))
                        (class "inline"))
                 (button (@ (class "linkish")) "refresh now"))
               " "
               (form (@ (method "post") (action ,(string-append base "/delete"))
                        (class "inline")
                        (data-confirm "Delete this feed and all its entries?"))
                 (button (@ (class "linkish danger")) "delete"))))))

    (define (render-admin-page req auth cfg)
      (let* ((feeds      (list-feeds cfg))
             (cat-orders (list-category-orders cfg))
             (skips      (list-skip-patterns cfg))
             (body
               `((header (@ (class "feeds-head"))
                   (h1 "Feed admin")
                   (a (@ (href "/feeds")) ,(raw "← back to feeds")))
                 ;; Add-feed form: refresh accepts duration strings like 1h.
                 (form (@ (method "post") (action "/feeds/admin")
                          (class "feed-new"))
                   (h2 "Add feed")
                   (label "URL "
                     (input (@ (type "url") (name "url") (required #t))))
                   (label "Title "
                     (input (@ (type "text") (name "title") (required #t))))
                   (label "Label "
                     (input (@ (type "text") (name "label") (maxlength "8"))))
                   (label "Category "
                     (input (@ (type "text") (name "category") (value "misc"))))
                   (label "Refresh "
                     (input (@ (type "text") (name "refresh") (value "1h")
                               (placeholder "30s, 10m, 1h, 1d"))))
                   (button (@ (type "submit")) "Add"))
                 (section (@ (class "cat-orders"))
                   (h2 "Categories")
                   (table (@ (class "feed-table"))
                     (thead (tr (th "name") (th "sort order") (th)))
                     (tbody ,@(map cat-order-row-sxml cat-orders))))
                 (section (@ (class "skip-patterns"))
                   (h2 "Skip patterns")
                   (p (@ (class "hint"))
                      "Incoming entries whose title matches any of these "
                      "are silently dropped at fetch time.")
                   (form (@ (method "post") (action "/feeds/admin/skip-pattern")
                            (class "skip-new inline"))
                     (select (@ (name "kind"))
                       (option (@ (value "prefix")) "prefix")
                       (option (@ (value "contains")) "contains"))
                     (input (@ (type "text") (name "pattern") (required #t)
                               (placeholder "e.g. Anzeige")))
                     (button (@ (type "submit")) "Add"))
                   (table (@ (class "feed-table"))
                     (thead (tr (th "kind") (th "pattern") (th)))
                     (tbody ,@(map skip-row-sxml skips))))
                 (table (@ (class "feed-table"))
                   (thead (tr (th "id") (th "cat") (th "label") (th "title")
                              (th "url") (th "refresh")
                              (th (@ (title "minimum entries pinned per feed regardless of category cap"))
                                  "pin")
                              (th "last") (th "fails") (th "error") (th)))
                   (tbody ,@(map feed-row-sxml feeds))))))
        (html-response
          (render-page req auth
                       '((title  . "Feed admin")
                         (active . feeds)
                         (body-class . "feeds-page"))
                       (html->string body)))))

    ;; ==============================================================
    ;; Routes
    ;; ==============================================================

    (define (param-or req params name default)
      (let* ((q (url-query-params (http-request-url req)))
             (p (assoc name q)))
        (cond
          ((and p (string? (cdr p))) (percent-decode (cdr p)))
          (else default))))

    (define (install-feed-routes! router cfg auth)
      (router-add! router "GET" "/feeds"
        (require-auth auth
          (lambda (req params)
            (let* ((q   (param-or req params "q" ""))
                   (cat (param-or req params "cat" ""))
                   (all (param-or req params "all" "")))
              (render-feeds-page req auth cfg
                                 (cond ((string=? q "") #f) (else q))
                                 (cond ((string=? cat "") #f) (else cat))
                                 (string=? all "1"))))))

      (router-add! router "POST" "/feeds/entry/:id/read"
        (require-auth auth
          (lambda (req params)
            (let ((id (string->number (params-ref params "id"))))
              (when id (mark-entry-read! cfg id))
              (make-http-response 302
                (list (cons "Location" "/feeds")) "")))))

      (router-add! router "POST" "/feeds/entry/:id/unread"
        (require-auth auth
          (lambda (req params)
            (let ((id (string->number (params-ref params "id"))))
              (when id (mark-entry-unread! cfg id))
              (make-http-response 302
                (list (cons "Location" "/feeds?all=1")) "")))))

      (router-add! router "POST" "/feeds/mark-all-read"
        (require-auth auth
          (lambda (req params)
            (let* ((form (parse-www-form (or (http-request-body req) "")))
                   (cat  (form-ref form "cat" "")))
              (mark-all-read! cfg (cond ((string=? cat "") #f) (else cat)))
              (make-http-response 302
                (list (cons "Location"
                            (cond ((string=? cat "") "/feeds")
                                  (else (string-append "/feeds?cat="
                                                       (percent-encode cat))))))
                "")))))

      (router-add! router "POST" "/feeds/mark-older"
        (require-auth auth
          (lambda (req params)
            (let* ((form (parse-www-form (or (http-request-body req) "")))
                   (cat  (form-ref form "cat" ""))
                   (days (or (string->number (form-ref form "days" "2")) 2)))
              (when (and (integer? days) (> days 0))
                (mark-older-than! cfg
                                  (cond ((string=? cat "") #f) (else cat))
                                  days))
              (make-http-response 302
                (list (cons "Location"
                            (cond ((string=? cat "") "/feeds")
                                  (else (string-append "/feeds?cat="
                                                       (percent-encode cat))))))
                "")))))

      (router-add! router "GET" "/feeds/admin"
        (require-auth auth
          (lambda (req params) (render-admin-page req auth cfg))))

      (router-add! router "POST" "/feeds/admin"
        (require-auth auth
          (lambda (req params)
            (let* ((form    (parse-www-form (or (http-request-body req) "")))
                   (url     (string-trim-both (form-ref form "url" "")))
                   (title   (string-trim-both (form-ref form "title" "")))
                   (label   (string-trim-both (form-ref form "label" "")))
                   (cat     (string-trim-both (form-ref form "category" "misc")))
                   ;; Accept either the new free-form "refresh" or the
                   ;; legacy "refresh_seconds" numeric input. Default 1h.
                   (raw     (cond
                              ((not (string=? (form-ref form "refresh" "") ""))
                               (form-ref form "refresh" ""))
                              (else
                               (form-ref form "refresh_seconds" "3600"))))
                   (refresh (or (parse-duration raw) 3600)))
              (cond
                ((or (string=? url "") (string=? title ""))
                 (render-error 400 "URL and title are required."))
                ((< refresh 30)
                 (render-error 400 "Refresh must be at least 30 seconds."))
                (else
                 (add-feed! cfg url title label cat refresh)
                 (make-http-response 302
                   (list (cons "Location" "/feeds/admin")) "")))))))

      (router-add! router "POST" "/feeds/admin/:id/refresh-interval"
        (require-auth auth
          (lambda (req params)
            (let* ((id      (string->number (params-ref params "id")))
                   (form    (parse-www-form (or (http-request-body req) "")))
                   (raw     (form-ref form "refresh" ""))
                   (refresh (parse-duration raw)))
              (cond
                ((and id refresh (>= refresh 30))
                 (set-refresh-seconds! cfg id refresh)
                 (make-http-response 302
                   (list (cons "Location" "/feeds/admin")) ""))
                (else
                 (render-error 400 "Refresh must be a duration ≥ 30s.")))))))

      (router-add! router "POST" "/feeds/admin/category-order"
        (require-auth auth
          (lambda (req params)
            (let* ((form  (parse-www-form (or (http-request-body req) "")))
                   (name  (string-trim-both (form-ref form "name" "")))
                   (order (string->number (form-ref form "sort_order" "100"))))
              (cond
                ((or (string=? name "") (not order))
                 (render-error 400 "Name and sort order are required."))
                (else
                 (update-category-order! cfg name (exact order))
                 (make-http-response 302
                   (list (cons "Location" "/feeds/admin")) "")))))))

      (router-add! router "POST" "/feeds/admin/:id/min-entries"
        (require-auth auth
          (lambda (req params)
            (let* ((id   (string->number (params-ref params "id")))
                   (form (parse-www-form (or (http-request-body req) "")))
                   (n    (string->number (form-ref form "min_entries" "0"))))
              (cond
                ((and id n (integer? n) (>= n 0))
                 (set-min-entries! cfg id (exact n))
                 (make-http-response 302
                   (list (cons "Location" "/feeds/admin")) ""))
                (else
                 (render-error 400 "min_entries must be a non-negative integer.")))))))

      (router-add! router "POST" "/feeds/admin/skip-pattern"
        (require-auth auth
          (lambda (req params)
            (let* ((form (parse-www-form (or (http-request-body req) "")))
                   (pat  (string-trim-both (form-ref form "pattern" "")))
                   (kind (form-ref form "kind" "prefix")))
              (cond
                ((string=? pat "")
                 (render-error 400 "Pattern is required."))
                ((not (or (string=? kind "prefix") (string=? kind "contains")))
                 (render-error 400 "Unknown kind."))
                (else
                 (add-skip-pattern! cfg pat kind)
                 (make-http-response 302
                   (list (cons "Location" "/feeds/admin")) "")))))))

      (router-add! router "POST" "/feeds/admin/skip-pattern/:id/delete"
        (require-auth auth
          (lambda (req params)
            (let ((id (string->number (params-ref params "id"))))
              (when id (delete-skip-pattern! cfg id))
              (make-http-response 302
                (list (cons "Location" "/feeds/admin")) "")))))

      (router-add! router "POST" "/feeds/admin/:id/delete"
        (require-auth auth
          (lambda (req params)
            (let ((id (string->number (params-ref params "id"))))
              (when id (delete-feed! cfg id))
              (make-http-response 302
                (list (cons "Location" "/feeds/admin")) "")))))

      (router-add! router "POST" "/feeds/admin/:id/toggle"
        (require-auth auth
          (lambda (req params)
            (let ((id (string->number (params-ref params "id"))))
              (when id (toggle-feed! cfg id))
              (make-http-response 302
                (list (cons "Location" "/feeds/admin")) "")))))

      (router-add! router "POST" "/feeds/admin/:id/refresh"
        (require-auth auth
          (lambda (req params)
            (let ((id (string->number (params-ref params "id"))))
              (when id (force-refresh! cfg id))
              (make-http-response 302
                (list (cons "Location" "/feeds/admin")) ""))))))

))
