(define-library (damian feeds)
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
          (damian db)
          (damian util)
          (damian auth)
          (damian views)
          (damian feeds-parser)
          (damian feeds-fetcher)
          (damian log))
  (export install-feed-routes!
          start-feed-scheduler!
          ;; exposed for unit tests
          round-robin-by-label)
  (begin

    ;; ==============================================================
    ;; DB ops. All SQL flows through (damian db) helpers.
    ;; ==============================================================

    (define (rows cfg sql)
      (with-db cfg (lambda (c) (pg-result-rows (pg-query c sql)))))

    (define (alist-rows cfg sql)
      (with-db cfg (lambda (c) (pg-result->alist-list (pg-query c sql)))))

    (define (exec cfg sql)
      (with-db cfg (lambda (c) (pg-exec c sql))))

    ;; --- entries listing ---

    (define (build-filter q category show-all?)
      ;; Returns a SQL WHERE clause (without leading WHERE) and ORDER BY.
      (let ((clauses (list "1=1")))
        (when (not show-all?)
          (set! clauses (cons "fe.read_at IS NULL" clauses)))
        (when (and category (not (string=? category "")))
          (set! clauses
                (cons (string-append "f.category = "
                                     (sql-quote-literal category))
                      clauses)))
        (when (and q (> (string-length (string-trim-both q)) 0))
          (set! clauses
                (cons (string-append
                        "to_tsvector('simple', fe.title || ' ' || fe.summary) "
                        "@@ plainto_tsquery('simple', "
                        (sql-quote-literal q) ")")
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
      (exec cfg (string-append
                  "UPDATE feed_entries SET read_at = now() WHERE id = "
                  (sql-quote-int id))))

    (define (mark-entry-unread! cfg id)
      (exec cfg (string-append
                  "UPDATE feed_entries SET read_at = NULL WHERE id = "
                  (sql-quote-int id))))

    (define (mark-all-read! cfg category)
      (let ((sql (cond
                   ((and category (not (string=? category "")))
                    (string-append
                      "UPDATE feed_entries SET read_at = now() "
                      "WHERE read_at IS NULL AND feed_id IN "
                      "(SELECT id FROM feeds WHERE category = "
                      (sql-quote-literal category) ")"))
                   (else
                    "UPDATE feed_entries SET read_at = now() WHERE read_at IS NULL"))))
        (exec cfg sql)))

    (define (mark-older-than! cfg category days)
      ;; Marks unread entries older than `days` days as read. The cutoff
      ;; is based on fetched_at — the same field that drives display
      ;; ordering — so what looks "old" in the list is what gets dismissed.
      (let* ((cutoff (string-append
                       "now() - interval '" (sql-quote-int days) " days'"))
             (cat-clause (cond
                           ((and category (not (string=? category "")))
                            (string-append
                              "AND feed_id IN (SELECT id FROM feeds WHERE category = "
                              (sql-quote-literal category) ") "))
                           (else "")))
             (sql (string-append
                    "UPDATE feed_entries SET read_at = now() "
                    "WHERE read_at IS NULL "
                    cat-clause
                    "AND fetched_at < " cutoff)))
        (exec cfg sql)))

    (define (update-category-order! cfg name sort-order)
      ;; Upserts a category row. Used by the admin UI to reorder.
      (exec cfg
        (string-append
          "INSERT INTO categories (name, sort_order) VALUES ("
          (sql-quote-literal name) ", " (sql-quote-int sort-order) ") "
          "ON CONFLICT (name) DO UPDATE SET sort_order = EXCLUDED.sort_order")))

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
          "INSERT INTO categories (name, sort_order) VALUES ("
          (sql-quote-literal category) ", 100) "
          "ON CONFLICT (name) DO NOTHING")))

    (define (add-feed! cfg url title label category refresh-seconds)
      (ensure-category! cfg category)
      (exec cfg
        (string-append
          "INSERT INTO feeds (url, title, label, category, refresh_seconds) VALUES ("
          (sql-quote-literal url) ", "
          (sql-quote-literal title) ", "
          (sql-quote-literal label) ", "
          (sql-quote-literal category) ", "
          (sql-quote-int refresh-seconds) ") "
          "ON CONFLICT (url) DO NOTHING")))

    (define (delete-feed! cfg id)
      (exec cfg (string-append "DELETE FROM feeds WHERE id = "
                               (sql-quote-int id))))

    (define (toggle-feed! cfg id)
      (exec cfg (string-append "UPDATE feeds SET enabled = NOT enabled "
                               "WHERE id = " (sql-quote-int id))))

    (define (force-refresh! cfg id)
      (exec cfg (string-append "UPDATE feeds SET last_fetched_at = NULL, "
                               "failure_count = 0 "
                               "WHERE id = " (sql-quote-int id))))

    (define (set-refresh-seconds! cfg id refresh-seconds)
      (exec cfg (string-append "UPDATE feeds SET refresh_seconds = "
                               (sql-quote-int refresh-seconds)
                               " WHERE id = " (sql-quote-int id))))

    (define (set-min-entries! cfg id n)
      (exec cfg (string-append "UPDATE feeds SET min_entries = "
                               (sql-quote-int n)
                               " WHERE id = " (sql-quote-int id))))

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
          "INSERT INTO feed_skip_patterns (pattern, kind) VALUES ("
          (sql-quote-literal pattern) ", "
          (sql-quote-literal kind) ") "
          "ON CONFLICT (kind, pattern) DO NOTHING")))

    (define (delete-skip-pattern! cfg id)
      (exec cfg (string-append "DELETE FROM feed_skip_patterns WHERE id = "
                               (sql-quote-int id))))

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
      (exec cfg
        (string-append
          "UPDATE feeds SET last_fetched_at = now(), last_error = "
          (cond (error-msg (sql-quote-literal error-msg))
                (else "NULL"))
          ", failure_count = "
          (cond (error-msg "failure_count + 1")
                (else "0"))
          " WHERE id = " (sql-quote-int id))))

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
         (let ((in-list (open-output-string)))
           (let loop ((ks keys) (first? #t))
             (cond
               ((null? ks) #t)
               (else
                (when (not first?) (write-string ", " in-list))
                (write-string (sql-quote-literal (car ks)) in-list)
                (loop (cdr ks) #f))))
           (let* ((sql (string-append
                         "SELECT DISTINCT title_key FROM feed_entries "
                         "WHERE title_key <> '' "
                         "AND title_key IN (" (get-output-string in-list) ") "
                         "AND fetched_at > now() - interval '"
                         (number->string dedup-window-days) " days'"))
                  (rs (rows cfg sql)))
             (map (lambda (row) (vector-ref row 0)) rs))))))

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
             "(" (sql-quote-int feed-id) ", "
             (sql-quote-literal guid) ", "
             (sql-quote-literal (or (assoc-val e "title") "")) ", "
             (sql-quote-literal (or (assoc-val e "link") "")) ", "
             (sql-quote-literal (or (assoc-val e "summary") "")) ", "
             (cond
               (unix (string-append "to_timestamp("
                                    (sql-quote-int unix) ")"))
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
          due)))

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

    (define (render-feed-entry out row)
      (let* ((id    (row-field row "id"))
             (label (row-field row "label"))
             (cat   (row-field row "category"))
             (title (row-field row "title"))
             (link  (row-field row "link"))
             (sumr  (strip-html-tags (row-field row "summary")))
             (pub   (row-field row "published"))
             (read  (row-field row "read"))
             ;; The link tooltip carries both the publication date and the
             ;; (truncated, plain-text) summary so hovering the entry
             ;; surfaces both without consuming column width.
             (tip   (cond
                      ((and (not (string=? pub  ""))
                            (not (string=? sumr "")))
                       (string-append pub " — " sumr))
                      ((not (string=? pub  "")) pub)
                      (else sumr))))
        (write-string "<li class=\"feed-entry" out)
        (when (string=? read "yes") (write-string " is-read" out))
        (write-string "\" data-id=\"" out)
        (write-string (html-attr-escape id) out)
        (write-string "\" data-cat=\"" out)
        (write-string (html-attr-escape cat) out)
        (write-string "\">" out)

        (write-string "<form method=\"post\" action=\"/feeds/entry/" out)
        (write-string (html-attr-escape id) out)
        (write-string "/" out)
        (write-string (if (string=? read "yes") "unread" "read") out)
        (write-string "\" class=\"mark\">" out)
        (write-string "<button type=\"submit\" title=\"" out)
        (write-string (if (string=? read "yes") "mark unread" "mark read") out)
        (write-string "\">" out)
        (write-string (if (string=? read "yes") "↩" "✓") out)
        (write-string "</button></form>" out)

        (write-string "<a class=\"entry-link\" href=\"" out)
        (write-string (html-attr-escape link) out)
        (write-string "\" target=\"_blank\" rel=\"noopener\" title=\"" out)
        (write-string (html-attr-escape tip) out)
        (write-string "\">" out)
        (when (not (string=? label ""))
          (write-string "<span class=\"label\">" out)
          (write-string (html-escape label) out)
          (write-string "</span> " out))
        (write-string (html-escape title) out)
        (write-string "</a>" out)

        (write-string "</li>" out)))

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

    (define (render-category-section out cat-entries category)
      (let ((cat (car cat-entries))
            (es  (cdr cat-entries)))
        (write-string "<section class=\"feed-cat\" data-cat=\"" out)
        (write-string (html-attr-escape cat) out)
        (write-string "\">" out)
        (write-string "<header><h2>" out)
        (write-string (html-escape cat) out)
        (write-string "</h2>" out)
        (write-string "<form method=\"post\" action=\"/feeds/mark-all-read\" class=\"inline\">" out)
        (write-string "<input type=\"hidden\" name=\"cat\" value=\"" out)
        (write-string (html-attr-escape cat) out)
        (write-string "\">" out)
        (write-string (string-append
                        "<button type=\"submit\" class=\"linkish\" "
                        "title=\"mark all read in this category\">mark all read</button>")
                      out)
        (write-string "</form></header>" out)
        (write-string "<ul class=\"feed-entries\">" out)
        (for-each (lambda (e) (render-feed-entry out e)) es)
        (write-string "</ul></section>" out)))

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
             (out     (open-output-string)))
        (write-string "<header class=\"feeds-head\">" out)
        (write-string "<h1>Feeds" out)
        (cond
          (show-all? (write-string " <span class=\"qual\">archive</span>" out))
          (else      (write-string " <span class=\"qual\">unread</span>" out)))
        (write-string "</h1>" out)

        ;; Filter form: search + category select + show-archive + admin link.
        (write-string "<form method=\"get\" action=\"/feeds\" class=\"feed-filters\">" out)
        (write-string "<input type=\"search\" name=\"q\" placeholder=\"search archive\" value=\"" out)
        (write-string (html-attr-escape (or q "")) out)
        (write-string "\">" out)
        (write-string "<select name=\"cat\">" out)
        (write-string "<option value=\"\">all categories</option>" out)
        (for-each
          (lambda (c)
            (write-string "<option value=\"" out)
            (write-string (html-attr-escape c) out)
            (write-string "\"" out)
            (when (and category (string=? category c))
              (write-string " selected" out))
            (write-string ">" out)
            (write-string (html-escape c) out)
            (let ((n (cdr (or (assoc c unread) (cons "" 0)))))
              (when (> n 0)
                (write-string " (" out) (write-string (number->string n) out)
                (write-string ")" out)))
            (write-string "</option>" out))
          cats)
        (write-string "</select>" out)
        (write-string "<label class=\"checkbox\"><input type=\"checkbox\" name=\"all\" value=\"1\"" out)
        (when show-all? (write-string " checked" out))
        (write-string ">show archive</label>" out)
        (write-string "<button type=\"submit\">Apply</button>" out)
        (write-string "<a class=\"admin-link\" href=\"/feeds/admin\">admin</a>" out)
        (write-string "</form>" out)

        ;; Mark older-than form. Carries the active category filter so a
        ;; user looking at e.g. only Tech can sweep just Tech.
        (write-string (string-append
                        "<form method=\"post\" action=\"/feeds/mark-older\" "
                        "class=\"feed-mark-older\">")
                      out)
        (when category
          (write-string "<input type=\"hidden\" name=\"cat\" value=\"" out)
          (write-string (html-attr-escape category) out)
          (write-string "\">" out))
        (write-string "<label>mark read older than " out)
        (write-string (string-append
                        "<input type=\"number\" name=\"days\" min=\"1\" "
                        "value=\"2\" inputmode=\"numeric\"> days")
                      out)
        (write-string "</label><button type=\"submit\" class=\"linkish\">go</button>" out)
        (write-string "</form>" out)
        (write-string "</header>" out)

        (cond
          ((null? grouped)
           (write-string "<p class=\"empty\">Nothing to read." out)
           (when (not show-all?)
             (write-string " <a href=\"/feeds?all=1\">Show archive</a>." out))
           (write-string "</p>" out))
          (else
           (write-string "<div class=\"feed-grid\">" out)
           (for-each
             (lambda (ce) (render-category-section out ce category))
             grouped)
           (write-string "</div>" out)))

        (html-response
          (render-page req auth
                       (list (cons 'title  "Feeds")
                             (cons 'active 'feeds)
                             (cons 'body-class
                                   (cond
                                     (category "feeds-page feeds-page-single")
                                     (else     "feeds-page"))))
                       (get-output-string out)))))

    (define (render-admin-page req auth cfg)
      (let ((feeds      (list-feeds cfg))
            (cat-orders (list-category-orders cfg))
            (skips      (list-skip-patterns cfg))
            (out        (open-output-string)))
        (write-string "<header class=\"feeds-head\"><h1>Feed admin</h1>" out)
        (write-string "<a href=\"/feeds\">← back to feeds</a></header>" out)

        ;; --- Add-feed form: refresh accepts duration strings like 1h ---
        (write-string "<form method=\"post\" action=\"/feeds/admin\" class=\"feed-new\">" out)
        (write-string "<h2>Add feed</h2>" out)
        (write-string "<label>URL <input type=\"url\" name=\"url\" required></label>" out)
        (write-string "<label>Title <input type=\"text\" name=\"title\" required></label>" out)
        (write-string "<label>Label <input type=\"text\" name=\"label\" maxlength=\"8\"></label>" out)
        (write-string "<label>Category <input type=\"text\" name=\"category\" value=\"misc\"></label>" out)
        (write-string (string-append
                        "<label>Refresh <input type=\"text\" name=\"refresh\" "
                        "value=\"1h\" placeholder=\"30s, 10m, 1h, 1d\"></label>")
                      out)
        (write-string "<button type=\"submit\">Add</button>" out)
        (write-string "</form>" out)

        ;; --- Categories: edit sort order ---
        (write-string "<section class=\"cat-orders\">" out)
        (write-string "<h2>Categories</h2>" out)
        (write-string "<table class=\"feed-table\">" out)
        (write-string (string-append
                        "<thead><tr><th>name</th><th>sort order</th>"
                        "<th></th></tr></thead><tbody>")
                      out)
        (for-each
          (lambda (c)
            (let ((name  (row-field c "name"))
                  (order (row-field c "sort_order")))
              (write-string (string-append
                              "<tr><form method=\"post\" "
                              "action=\"/feeds/admin/category-order\">")
                            out)
              (write-string "<td>" out) (write-string (html-escape name) out)
              (write-string "<input type=\"hidden\" name=\"name\" value=\"" out)
              (write-string (html-attr-escape name) out)
              (write-string "\"></td>" out)
              (write-string (string-append
                              "<td><input type=\"number\" name=\"sort_order\" "
                              "value=\"")
                            out)
              (write-string (html-attr-escape order) out)
              (write-string "\" min=\"0\" max=\"10000\"></td>" out)
              (write-string (string-append
                              "<td><button type=\"submit\" class=\"linkish\">save"
                              "</button></td></form></tr>")
                            out)))
          cat-orders)
        (write-string "</tbody></table></section>" out)

        ;; --- Skip patterns ---
        (write-string "<section class=\"skip-patterns\">" out)
        (write-string "<h2>Skip patterns</h2>" out)
        (write-string "<p class=\"hint\">Incoming entries whose title matches " out)
        (write-string "any of these are silently dropped at fetch time.</p>" out)
        (out! out "<form method=\"post\" action=\"/feeds/admin/skip-pattern\" "
                  "class=\"skip-new inline\">")
        (out! out "<select name=\"kind\">"
                  "<option value=\"prefix\">prefix</option>"
                  "<option value=\"contains\">contains</option>"
                  "</select>")
        (out! out "<input type=\"text\" name=\"pattern\" required "
                  "placeholder=\"e.g. Anzeige\">")
        (out! out "<button type=\"submit\">Add</button></form>"
                  "<table class=\"feed-table\">"
                  "<thead><tr><th>kind</th><th>pattern</th><th></th></tr>"
                  "</thead><tbody>")
        (for-each
          (lambda (sp)
            (let ((sid  (row-field sp "id"))
                  (kind (row-field sp "kind"))
                  (pat  (row-field sp "pattern")))
              (write-string "<tr><td>" out)
              (write-string (html-escape kind) out)
              (write-string "</td><td>" out)
              (write-string (html-escape pat)  out)
              (write-string "</td><td><form method=\"post\" action=\"/feeds/admin/skip-pattern/" out)
              (write-string (html-attr-escape sid) out)
              (write-string "/delete\" class=\"inline\">" out)
              (write-string "<button type=\"submit\" class=\"linkish danger\">remove</button>" out)
              (write-string "</form></td></tr>" out)))
          skips)
        (write-string "</tbody></table></section>" out)

        ;; --- Feeds table ---
        (write-string "<table class=\"feed-table\"><thead><tr>" out)
        (write-string "<th>id</th><th>cat</th><th>label</th><th>title</th>" out)
        (write-string "<th>url</th><th>refresh</th><th title=\"minimum entries pinned per feed regardless of category cap\">pin</th>" out)
        (write-string "<th>last</th><th>fails</th><th>error</th><th></th></tr></thead><tbody>" out)
        (for-each
          (lambda (f)
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
                   (err     (row-field f "last_error")))
              (write-string "<tr" out)
              (when (string=? enabled "no") (write-string " class=\"disabled\"" out))
              (write-string "><td>" out) (write-string (html-escape id) out)
              (write-string "</td><td>" out) (write-string (html-escape cat) out)
              (write-string "</td><td>" out) (write-string (html-escape label) out)
              (write-string "</td><td>" out) (write-string (html-escape title) out)
              (write-string "</td><td class=\"url\">" out)
              (write-string (html-escape url) out)
              (write-string (string-append
                              "</td><td><form method=\"post\" "
                              "action=\"/feeds/admin/")
                            out)
              (write-string (html-attr-escape id) out)
              (write-string "/refresh-interval\" class=\"inline\">" out)
              (write-string (string-append
                              "<input type=\"text\" name=\"refresh\" size=\"5\" "
                              "value=\"")
                            out)
              (write-string (html-attr-escape (format-duration refresh-int)) out)
              (write-string (string-append
                              "\"><button type=\"submit\" "
                              "class=\"linkish\">↵</button></form></td>")
                            out)
              (write-string "<td><form method=\"post\" action=\"/feeds/admin/" out)
              (write-string (html-attr-escape id) out)
              (write-string "/min-entries\" class=\"inline\">" out)
              (write-string "<input type=\"number\" name=\"min_entries\" size=\"3\" min=\"0\" max=\"50\" value=\"" out)
              (write-string (html-attr-escape pin) out)
              (write-string (string-append
                              "\"><button type=\"submit\" "
                              "class=\"linkish\">↵</button></form></td>")
                            out)
              (write-string "<td>" out) (write-string (html-escape last) out)
              (write-string "</td><td>" out) (write-string (html-escape fails) out)
              (write-string "</td><td class=\"err\">" out)
              (write-string (html-escape err) out)
              (write-string "</td><td class=\"acts\">" out)
              (write-string "<form method=\"post\" action=\"/feeds/admin/" out)
              (write-string (html-attr-escape id) out)
              (write-string "/toggle\" class=\"inline\"><button class=\"linkish\">" out)
              (write-string (if (string=? enabled "yes") "disable" "enable") out)
              (write-string "</button></form> " out)
              (write-string "<form method=\"post\" action=\"/feeds/admin/" out)
              (write-string (html-attr-escape id) out)
              (write-string "/refresh\" class=\"inline\"><button class=\"linkish\">refresh now</button></form> " out)
              (write-string "<form method=\"post\" action=\"/feeds/admin/" out)
              (write-string (html-attr-escape id) out)
              (write-string (string-append
                              "/delete\" class=\"inline\" "
                              "data-confirm=\"Delete this feed and all its entries?\">")
                            out)
              (write-string "<button class=\"linkish danger\">delete</button></form>" out)
              (write-string "</td></tr>" out)))
          feeds)
        (write-string "</tbody></table>" out)
        (html-response
          (render-page req auth
                       '((title  . "Feed admin")
                         (active . feeds)
                         (body-class . "feeds-page"))
                       (get-output-string out)))))

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
