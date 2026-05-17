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
          (scm net http client)
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
      (let ((clauses (list "fe.deleted_at IS NULL")))
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
      ;; Manual pins (feed_id NULL) never appear here — this view is the
      ;; unread/archive triage list for feed-sourced items. Manual pins
      ;; live in /feeds/pinned. The inner join enforces that.
      (let ((sql (string-append
                   "SELECT fe.id::text AS id, f.label AS label, "
                   "  f.category AS category, fe.title AS title, "
                   "  fe.link AS link, "
                   "  substring(fe.summary, 1, "
                   (number->string summary-preview-chars) ") AS summary, "
                   "  to_char(fe.fetched_at, 'YYYY-MM-DD HH24:MI') AS published, "
                   "  CASE WHEN fe.read_at IS NULL THEN 'no' ELSE 'yes' END AS read, "
                   "  CASE WHEN fe.pinned_at IS NULL THEN 'no' ELSE 'yes' END AS pinned "
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
                       "WHERE fe.read_at IS NULL AND fe.deleted_at IS NULL "
                       "GROUP BY f.category"))))
        (map (lambda (r)
               (cons (cdr (assoc "category" r))
                     (string->number (cdr (assoc "n" r)))))
             alist)))

    ;; --- mark read/unread ---

    (define (mark-entry-read! cfg id)
      (exec cfg
            (string-append
              "UPDATE feed_entries SET read_at = now() "
              "WHERE id = $1 AND deleted_at IS NULL")
            (list id)))

    (define (mark-entry-unread! cfg id)
      (exec cfg
            (string-append
              "UPDATE feed_entries SET read_at = NULL "
              "WHERE id = $1 AND deleted_at IS NULL")
            (list id)))

    (define (mark-all-read! cfg category)
      (cond
        ((and category (not (string=? category "")))
         (exec cfg
               (string-append
                 "UPDATE feed_entries SET read_at = now() "
                 "WHERE read_at IS NULL AND deleted_at IS NULL "
                 "AND feed_id IN "
                 "(SELECT id FROM feeds WHERE category = $1)")
               (list category)))
        (else
         (exec cfg
               (string-append
                 "UPDATE feed_entries SET read_at = now() "
                 "WHERE read_at IS NULL AND deleted_at IS NULL")))))

    (define (mark-older-than! cfg category days)
      ;; Marks unread entries older than `days` days as read. The cutoff
      ;; is based on fetched_at — the same field that drives display
      ;; ordering — so what looks "old" in the list is what gets dismissed.
      (cond
        ((and category (not (string=? category "")))
         (exec cfg
               (string-append
                 "UPDATE feed_entries SET read_at = now() "
                 "WHERE read_at IS NULL AND deleted_at IS NULL "
                 "AND feed_id IN (SELECT id FROM feeds WHERE category = $1) "
                 "AND fetched_at < now() - make_interval(days => $2)")
               (list category days)))
        (else
         (exec cfg
               (string-append
                 "UPDATE feed_entries SET read_at = now() "
                 "WHERE read_at IS NULL AND deleted_at IS NULL "
                 "AND fetched_at < now() - make_interval(days => $1)")
               (list days)))))

    ;; Cap of read entries kept per label. Unread entries are never pruned.
    ;; Feeds without a label are bucketed per-feed so an unlabelled spammy
    ;; feed can't crowd out an unlabelled quiet one.
    (define read-entries-per-label-cap 100)

    ;; Soft-prune: mark read entries beyond the per-label cap as deleted.
    ;; The rows stay in the table so the (feed_id, guid) unique row and
    ;; the title_key recency scan can still suppress re-inserts when the
    ;; same item shows up in a later refresh. Pinned entries are immune
    ;; to pruning. Manual pins (feed_id NULL) are filtered out by the
    ;; inner join too, but the pinned_at guard is the load-bearing one.
    (define (prune-read-entries! cfg)
      (exec cfg
        (string-append
          "UPDATE feed_entries SET deleted_at = now() WHERE id IN ("
          "  SELECT id FROM ("
          "    SELECT fe.id, row_number() OVER ("
          "      PARTITION BY COALESCE(NULLIF(f.label, ''), "
          "                            'feed#' || f.id::text) "
          "      ORDER BY fe.read_at DESC, fe.id DESC) AS rn "
          "    FROM feed_entries fe JOIN feeds f ON f.id = fe.feed_id "
          "    WHERE fe.read_at IS NOT NULL AND fe.deleted_at IS NULL "
          "    AND fe.pinned_at IS NULL"
          "  ) t WHERE rn > " (number->string read-entries-per-label-cap) ")")))

    ;; Second-stage: physically remove rows whose deleted_at is past the
    ;; dedup window plus a small safety margin (see dedup-window-days
    ;; below). After that point the row can no longer suppress a
    ;; republish anyway.
    (define hard-prune-after-days 40)

    (define (hard-prune-entries! cfg)
      (exec cfg
            (string-append
              "DELETE FROM feed_entries "
              "WHERE deleted_at IS NOT NULL "
              "AND pinned_at IS NULL "
              "AND deleted_at < now() - make_interval(days => $1)")
            (list hard-prune-after-days)))

    ;; --- pinning -------------------------------------------------------

    ;; Pinning also marks the entry read (if not already) so it leaves
    ;; the unread triage list. Unpinning leaves read_at alone — the
    ;; entry was already read when pinned.
    (define (pin-entry! cfg id)
      (exec cfg
            (string-append
              "UPDATE feed_entries "
              "SET pinned_at = COALESCE(pinned_at, now()), "
              "    read_at   = COALESCE(read_at, now()), "
              "    deleted_at = NULL "
              "WHERE id = $1")
            (list id)))

    (define (unpin-entry! cfg id)
      ;; Manual pins (feed_id NULL) have no archive to fall back to and
      ;; would become unreachable from the UI, so unpinning a manual pin
      ;; deletes it outright. Feed-sourced pins become regular read
      ;; entries again; the next prune may soft-delete them.
      (exec cfg
            (string-append
              "DELETE FROM feed_entries "
              "WHERE id = $1 AND feed_id IS NULL")
            (list id))
      (exec cfg
            (string-append
              "UPDATE feed_entries SET pinned_at = NULL "
              "WHERE id = $1 AND feed_id IS NOT NULL")
            (list id)))

    (define (unpin-by-ids! cfg ids)
      ;; Bulk unpin a list of ids. Used by "remove all currently
      ;; displayed" — ids come from re-running the pinned-list query
      ;; with the active filter.
      (cond
        ((null? ids) #t)
        (else
         (exec cfg
               (string-append
                 "DELETE FROM feed_entries "
                 "WHERE id IN $1 AND feed_id IS NULL")
               (list (list->vector ids)))
         (exec cfg
               (string-append
                 "UPDATE feed_entries SET pinned_at = NULL "
                 "WHERE id IN $1 AND feed_id IS NOT NULL")
               (list (list->vector ids))))))

    (define (unpin-stale! cfg)
      (exec cfg
            (string-append
              "DELETE FROM feed_entries "
              "WHERE pinned_at IS NOT NULL "
              "AND link_status = 'stale' AND feed_id IS NULL"))
      (exec cfg
            (string-append
              "UPDATE feed_entries SET pinned_at = NULL "
              "WHERE pinned_at IS NOT NULL "
              "AND link_status = 'stale' AND feed_id IS NOT NULL")))

    ;; --- pinned listing ------------------------------------------------

    (define (build-pinned-filter q category feed-id since until stale-only?)
      ;; All clauses ANDed together. Empty values are skipped. Feed
      ;; category/label come from the feeds row when present, otherwise
      ;; from the manual_* columns on feed_entries.
      (let ((clauses (list "fe.pinned_at IS NOT NULL")))
        (when stale-only?
          (set! clauses (cons "fe.link_status = 'stale'" clauses)))
        (when (and category (not (string=? category "")))
          (set! clauses
                (cons (string-append
                        "COALESCE(f.category, fe.manual_category, 'misc') = "
                        (pg-quote-literal category))
                      clauses)))
        (when (and feed-id (not (string=? feed-id "")))
          (set! clauses
                (cons (string-append
                        "fe.feed_id = " (pg-quote-literal feed-id) "::bigint")
                      clauses)))
        (when (and since (not (string=? since "")))
          (set! clauses
                (cons (string-append
                        "fe.pinned_at >= " (pg-quote-literal since) "::date")
                      clauses)))
        (when (and until (not (string=? until "")))
          (set! clauses
                (cons (string-append
                        "fe.pinned_at < (" (pg-quote-literal until)
                        "::date + INTERVAL '1 day')")
                      clauses)))
        (when (and q (> (string-length (string-trim-both q)) 0))
          (set! clauses
                (cons (string-append
                        "to_tsvector('simple', fe.title || ' ' || fe.summary) "
                        "@@ plainto_tsquery('simple', "
                        (pg-quote-literal q) ")")
                      clauses)))
        (string-join clauses " AND ")))

    (define (list-pinned-entries cfg q category feed-id since until stale-only?)
      (let ((sql (string-append
                   "SELECT fe.id::text AS id, "
                   "  COALESCE(f.label, fe.manual_label, '') AS label, "
                   "  COALESCE(f.category, fe.manual_category, 'misc') AS category, "
                   "  fe.title AS title, fe.link AS link, "
                   "  substring(fe.summary, 1, "
                   (number->string summary-preview-chars) ") AS summary, "
                   "  to_char(fe.pinned_at, 'YYYY-MM-DD HH24:MI') AS published, "
                   "  'yes' AS read, 'yes' AS pinned, "
                   "  fe.link_status AS link_status, "
                   "  CASE WHEN fe.feed_id IS NULL THEN 'yes' ELSE 'no' END AS manual "
                   "FROM feed_entries fe LEFT JOIN feeds f ON f.id = fe.feed_id "
                   "WHERE "
                   (build-pinned-filter q category feed-id since until stale-only?)
                   " ORDER BY fe.pinned_at DESC, fe.id DESC")))
        (alist-rows cfg sql)))

    (define (list-pinned-ids cfg q category feed-id since until stale-only?)
      ;; Used by "remove all currently displayed" — returns just the ids
      ;; that the same filter would render, so the bulk-unpin matches
      ;; exactly what the user saw.
      (let* ((sql (string-append
                    "SELECT fe.id::text AS id "
                    "FROM feed_entries fe LEFT JOIN feeds f ON f.id = fe.feed_id "
                    "WHERE "
                    (build-pinned-filter q category feed-id since until stale-only?)))
             (rs  (alist-rows cfg sql)))
        (filter-map (lambda (r) (string->number (cdr (assoc "id" r)))) rs)))

    (define (count-pinned cfg)
      (let ((rs (rows cfg
                  (string-append
                    "SELECT COUNT(*)::text FROM feed_entries "
                    "WHERE pinned_at IS NOT NULL"))))
        (cond ((null? rs) 0)
              (else (or (string->number (vector-ref (car rs) 0)) 0)))))

    (define (count-pinned-stale cfg)
      (let ((rs (rows cfg
                  (string-append
                    "SELECT COUNT(*)::text FROM feed_entries "
                    "WHERE pinned_at IS NOT NULL AND link_status = 'stale'"))))
        (cond ((null? rs) 0)
              (else (or (string->number (vector-ref (car rs) 0)) 0)))))

    (define (list-pinned-categories cfg)
      (map (lambda (r) (vector-ref r 0))
           (rows cfg
             (string-append
               "SELECT DISTINCT COALESCE(f.category, fe.manual_category, 'misc') "
               "FROM feed_entries fe LEFT JOIN feeds f ON f.id = fe.feed_id "
               "WHERE fe.pinned_at IS NOT NULL "
               "ORDER BY 1"))))

    (define (list-pinned-feeds cfg)
      ;; (id . title) pairs for the feed filter dropdown. Excludes the
      ;; synthetic "Manual" bucket — the user picks that via category.
      (alist-rows cfg
        (string-append
          "SELECT DISTINCT f.id::text AS id, f.title AS title "
          "FROM feed_entries fe JOIN feeds f ON f.id = fe.feed_id "
          "WHERE fe.pinned_at IS NOT NULL "
          "ORDER BY f.title")))

    ;; --- manual pin entry ---------------------------------------------

    (define (random-token n)
      ;; Used to synthesize a guid for manually added pins. n must be a
      ;; small positive integer; the result is a hex-ish string of
      ;; length 2n.
      (let ((out (open-output-string)))
        (let loop ((i 0))
          (cond
            ((= i n) (get-output-string out))
            (else
             (let ((b (modulo (* (+ i 1)
                                 (exact (round (* (current-second) 1000000))))
                              65536)))
               (write-string
                 (string-append
                   (number->string (quotient b 256) 16)
                   "-"
                   (number->string (modulo b 256) 16))
                 out)
               (loop (+ i 1))))))))

    (define (add-manual-pin! cfg link title category label summary)
      ;; Returns #t on success. Stores a manual entry that's already
      ;; marked read and pinned, with a synthetic guid so the
      ;; (feed_id, guid) constraint is happy. NB: feed_id is NULL,
      ;; and Postgres treats NULLs as distinct in unique constraints,
      ;; so the same URL can be added twice — that's intentional.
      (exec cfg
        (string-append
          "INSERT INTO feed_entries "
          "(feed_id, guid, title, link, summary, "
          " manual_category, manual_label, "
          " read_at, pinned_at) "
          "VALUES (NULL, $1, $2, $3, $4, $5, $6, now(), now())")
        (list (string-append "manual:" (random-token 8))
              title link summary category label)))

    ;; --- link health ---------------------------------------------------

    (define link-check-batch 20)
    (define link-check-interval-seconds 600)
    (define link-check-stale-threshold 2)
    (define link-check-recheck-after-hours 6)

    (define (due-link-checks cfg)
      ;; Pick pinned entries with a non-empty link whose last check is
      ;; older than the recheck window (or never checked). NULLs first
      ;; ensures fresh pins get checked promptly.
      (alist-rows cfg
        (string-append
          "SELECT id::text AS id, link AS link, "
          "       link_failure_count::text AS failures "
          "FROM feed_entries "
          "WHERE pinned_at IS NOT NULL AND link <> '' "
          "AND (link_checked_at IS NULL "
          "  OR link_checked_at < now() - make_interval(hours => $1)) "
          "ORDER BY link_checked_at NULLS FIRST "
          "LIMIT $2")
        (list link-check-recheck-after-hours link-check-batch)))

    (define (record-link-check! cfg id ok?)
      (cond
        (ok?
         (exec cfg
               (string-append
                 "UPDATE feed_entries SET link_checked_at = now(), "
                 "  link_status = 'ok', link_failure_count = 0 "
                 "WHERE id = $1")
               (list id)))
        (else
         (exec cfg
               (string-append
                 "UPDATE feed_entries SET link_checked_at = now(), "
                 "  link_failure_count = link_failure_count + 1, "
                 "  link_status = CASE "
                 "    WHEN link_failure_count + 1 >= $2 THEN 'stale' "
                 "    ELSE link_status END "
                 "WHERE id = $1")
               (list id link-check-stale-threshold)))))

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
                     (list (list->vector keys) dedup-window-days))))
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

    ;; --- link health probe (called from the scheduler) ---------------

    (define link-probe-ua
      '(("User-Agent" . "dabsite/1.0 (+https://www.damianbrunold.ch)")
        ("Accept"     . "*/*")))

    (define (probe-link-ok? url)
      ;; Returns #t if the URL responds with a non-error status. Tries
      ;; HEAD first (cheap); if that yields anything other than 2xx/3xx
      ;; we fall back to GET, because plenty of sites mis-handle HEAD
      ;; (return 405, 403, or just hang up) while a GET works fine.
      (define (ok-status? s) (and s (>= s 200) (< s 400)))
      (define (try method)
        (guard (exn (#t #f))
          (let* ((req (make-http-request method url link-probe-ua #f))
                 (resp (http-send req)))
            (http-response-status resp))))
      (let ((head-status (try "HEAD")))
        (cond
          ((ok-status? head-status) #t)
          (else
           (let ((get-status (try "GET")))
             (ok-status? get-status))))))

    (define (check-pinned-links! cfg)
      (let ((batch (due-link-checks cfg)))
        (for-each
          (lambda (row)
            (let* ((id     (string->number (cdr (assoc "id" row))))
                   (link   (cdr (assoc "link" row)))
                   (ok?    (probe-link-ok? link)))
              (guard (exn (#t (log-error "feeds"
                                "record-link-check! failed; continuing")))
                (record-link-check! cfg id ok?))))
          batch)))

    (define last-link-check-time #f)

    (define (maybe-check-pinned-links! cfg)
      (let ((now-s (exact (round (current-second)))))
        (cond
          ((or (not last-link-check-time)
               (>= (- now-s last-link-check-time)
                   link-check-interval-seconds))
           (set! last-link-check-time now-s)
           (guard (exn (#t (log-error "feeds"
                             "check-pinned-links! raised; continuing")))
             (check-pinned-links! cfg)))
          (else #t))))

    ;; Unix-seconds timestamp of the last hard-prune run, or #f if not
    ;; yet run this process. Read and written only from the scheduler
    ;; thread, so set! is safe here.
    (define last-hard-prune-time #f)
    (define hard-prune-interval-seconds 3600)

    (define (maybe-hard-prune! cfg)
      (let ((now-s (exact (round (current-second)))))
        (cond
          ((or (not last-hard-prune-time)
               (>= (- now-s last-hard-prune-time)
                   hard-prune-interval-seconds))
           (set! last-hard-prune-time now-s)
           (guard (exn (#t (log-error "feeds"
                             "hard-prune-entries! failed; continuing")))
             (hard-prune-entries! cfg)))
          (else #t))))

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
          (prune-read-entries! cfg))
        (maybe-hard-prune! cfg)
        (maybe-check-pinned-links! cfg)))

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
      ;;
      ;; The row may include "pinned" ('yes'|'no'), "link_status"
      ;; ('ok'|'stale'), and "manual" ('yes'|'no'). Pinned entries show
      ;; an active pin button that unpins on click; unpinned entries
      ;; show a hollow pin that pins. Manual pins skip the read toggle
      ;; entirely (they're always read; unpin = delete).
      (let* ((id     (row-field row "id"))
             (label  (row-field row "label"))
             (cat    (row-field row "category"))
             (title  (row-field row "title"))
             (link   (row-field row "link"))
             (sumr   (strip-html-tags (row-field row "summary")))
             (pub    (row-field row "published"))
             (read   (row-field row "read"))
             (read?  (string=? read "yes"))
             (pinned (row-field row "pinned"))
             (pinned? (string=? pinned "yes"))
             (lstat  (row-field row "link_status"))
             (stale? (string=? lstat "stale"))
             (manual (row-field row "manual"))
             (manual? (string=? manual "yes"))
             (tip   (cond
                      ((and (not (string=? pub  ""))
                            (not (string=? sumr "")))
                       (string-append pub " — " sumr))
                      ((not (string=? pub  "")) pub)
                      (else sumr))))
        `(li (@ (class ,(string-append "feed-entry"
                                       (if read?   " is-read"   "")
                                       (if pinned? " is-pinned" "")
                                       (if stale?  " is-stale"  "")))
                (data-id ,id)
                (data-cat ,cat))
             ,(cond
                (manual? `(span (@ (class "mark-spacer"))))
                (else
                 `(form (@ (method "post")
                           (action ,(string-append "/feeds/entry/" id "/"
                                                   (if read? "unread" "read")))
                           (class "mark"))
                    (button (@ (type "submit")
                               (title ,(if read? "mark unread" "mark read")))
                      ,(if read? "↩" "✓")))))
             (form (@ (method "post")
                      (action ,(string-append "/feeds/entry/" id "/"
                                              (if pinned? "unpin" "pin")))
                      (class "pin"))
                (button (@ (type "submit")
                           (title ,(cond
                                     (manual? "remove pin")
                                     (pinned? "unpin")
                                     (else    "pin"))))
                  ,(if pinned? "📌" "📍")))
             (a (@ (class "entry-link")
                   (href ,link)
                   (target "_blank")
                   (rel "noopener")
                   (title ,tip))
                ,(if (string=? label "")
                     ""
                     `(span (@ (class "label")) ,label))
                ,(if (string=? label "") "" " ")
                ,title
                ,(if stale? `(span (@ (class "stale-badge")
                                      (title "link last check failed"))
                                   " · stale")
                            "")))))

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
                     (a (@ (class "admin-link") (href "/feeds/pinned"))
                        "pinned")
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

    ;; --- pinned view ---------------------------------------------------

    (define (pinned-cat-option-sxml current c)
      `(option (@ (value ,c)
                  (selected ,(and current (string=? current c) #t))) ,c))

    (define (pinned-feed-option-sxml current f)
      (let ((id    (cdr (assoc "id" f)))
            (title (cdr (assoc "title" f))))
        `(option (@ (value ,id)
                    (selected ,(and current (string=? current id) #t)))
                 ,title)))

    (define (render-pinned-page req auth cfg q category feed-id since until
                                 stale-only? msg)
      (let* ((entries (list-pinned-entries cfg q category feed-id
                                            since until stale-only?))
             (cats    (list-pinned-categories cfg))
             (feeds   (list-pinned-feeds cfg))
             (total   (count-pinned cfg))
             (stale-n (count-pinned-stale cfg))
             ;; The "remove all currently displayed" form has to carry
             ;; every filter value so the server-side bulk action matches
             ;; what the user actually sees.
             (hidden-filters
               `(,@(if (and q (not (string=? q "")))
                       `((input (@ (type "hidden") (name "q") (value ,q))))
                       '())
                 ,@(if (and category (not (string=? category "")))
                       `((input (@ (type "hidden") (name "cat") (value ,category))))
                       '())
                 ,@(if (and feed-id (not (string=? feed-id "")))
                       `((input (@ (type "hidden") (name "feed") (value ,feed-id))))
                       '())
                 ,@(if (and since (not (string=? since "")))
                       `((input (@ (type "hidden") (name "since") (value ,since))))
                       '())
                 ,@(if (and until (not (string=? until "")))
                       `((input (@ (type "hidden") (name "until") (value ,until))))
                       '())
                 ,@(if stale-only?
                       `((input (@ (type "hidden") (name "stale") (value "1"))))
                       '())))
             (body
               `((header (@ (class "feeds-head"))
                   (h1 "Pinned "
                       (span (@ (class "qual"))
                             ,(string-append
                                (number->string (length entries)) " / "
                                (number->string total)
                                (if (> stale-n 0)
                                    (string-append "  ·  " (number->string stale-n)
                                                   " stale")
                                    ""))))
                   (a (@ (class "admin-link") (href "/feeds")) "← feeds")
                   ,(cond
                      ((and msg (not (string=? msg "")))
                       `(p (@ (class "flash")) ,msg))
                      (else ""))
                   (form (@ (method "get") (action "/feeds/pinned")
                            (class "feed-filters pinned-filters"))
                     (input (@ (type "search") (name "q")
                               (placeholder "search pinned")
                               (value ,(or q ""))))
                     (select (@ (name "cat"))
                       (option (@ (value "")) "all categories")
                       ,@(map (lambda (c) (pinned-cat-option-sxml category c))
                              cats))
                     (select (@ (name "feed"))
                       (option (@ (value "")) "all feeds")
                       ,@(map (lambda (f) (pinned-feed-option-sxml feed-id f))
                              feeds))
                     (label "from "
                       (input (@ (type "date") (name "since")
                                 (value ,(or since "")))))
                     (label "to "
                       (input (@ (type "date") (name "until")
                                 (value ,(or until "")))))
                     (label (@ (class "checkbox"))
                       (input (@ (type "checkbox") (name "stale") (value "1")
                                 (checked ,(if stale-only? #t #f))))
                       "stale only")
                     (button (@ (type "submit")) "Apply")))
                 ;; Manual add form.
                 (form (@ (method "post") (action "/feeds/pinned/add")
                          (class "pinned-add"))
                   (h2 "Add URL")
                   (label "URL "
                     (input (@ (type "url") (name "link") (required #t))))
                   (label "Title "
                     (input (@ (type "text") (name "title") (required #t))))
                   (label "Category "
                     (input (@ (type "text") (name "category") (value "misc"))))
                   (label "Label "
                     (input (@ (type "text") (name "label") (maxlength "8"))))
                   (label "Note "
                     (input (@ (type "text") (name "summary"))))
                   (button (@ (type "submit")) "Add pin"))
                 ;; Bulk-action bar.
                 (div (@ (class "pinned-bulk"))
                   (form (@ (method "post")
                            (action "/feeds/pinned/remove-stale")
                            (class "inline")
                            (data-confirm
                              "Unpin all stale entries?"))
                     (button (@ (type "submit") (class "linkish danger")
                                (disabled ,(if (= stale-n 0) #t #f)))
                       ,(string-append "remove all stale ("
                                       (number->string stale-n) ")")))
                   " "
                   (form (@ (method "post")
                            (action "/feeds/pinned/remove-current")
                            (class "inline")
                            (data-confirm
                              "Unpin all currently displayed entries?"))
                     ,@hidden-filters
                     (button (@ (type "submit") (class "linkish danger")
                                (disabled ,(if (null? entries) #t #f)))
                       ,(string-append "remove currently displayed ("
                                       (number->string (length entries)) ")"))))
                 ,(cond
                    ((null? entries)
                     `(p (@ (class "empty")) "No pinned entries match."))
                    (else
                     `(ul (@ (class "feed-entries pinned-entries"))
                       ,@(map feed-entry-sxml entries)))))))
        (html-response
          (render-page req auth
                       '((title  . "Pinned")
                         (active . feeds)
                         (body-class . "feeds-page pinned-page"))
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
      ;; Reconstructs a /feeds/pinned URL carrying the filter values
      ;; posted in `form`. Used after bulk pinned actions so the user
      ;; lands on the same view they came from. Defined up front because
      ;; internal definitions must precede all expressions in a body.
      (define (pinned-redirect form)
        (let* ((parts '())
               (add!  (lambda (k v)
                        (when (and v (not (string=? v "")))
                          (set! parts
                                (cons (string-append (percent-encode k) "="
                                                     (percent-encode v))
                                      parts))))))
          (add! "q"     (form-ref form "q"     ""))
          (add! "cat"   (form-ref form "cat"   ""))
          (add! "feed"  (form-ref form "feed"  ""))
          (add! "since" (form-ref form "since" ""))
          (add! "until" (form-ref form "until" ""))
          (add! "stale" (form-ref form "stale" ""))
          (let ((qs (string-join (reverse parts) "&")))
            (cond ((string=? qs "") "/feeds/pinned")
                  (else (string-append "/feeds/pinned?" qs))))))

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
              ;; Always redirect to the unfiltered view: the category the
              ;; user just emptied has nothing left to show, so keeping
              ;; the filter would land them on an empty page.
              (make-http-response 302
                (list (cons "Location" "/feeds")) "")))))

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

      ;; --- pinned routes ---------------------------------------------

      (router-add! router "GET" "/feeds/pinned"
        (require-auth auth
          (lambda (req params)
            (let* ((q      (param-or req params "q"     ""))
                   (cat    (param-or req params "cat"   ""))
                   (feed   (param-or req params "feed"  ""))
                   (since  (param-or req params "since" ""))
                   (until  (param-or req params "until" ""))
                   (stale  (param-or req params "stale" ""))
                   (msg    (param-or req params "msg"   "")))
              (render-pinned-page req auth cfg
                                  (cond ((string=? q "")     #f) (else q))
                                  (cond ((string=? cat "")   #f) (else cat))
                                  (cond ((string=? feed "")  #f) (else feed))
                                  (cond ((string=? since "") #f) (else since))
                                  (cond ((string=? until "") #f) (else until))
                                  (string=? stale "1")
                                  msg)))))

      (router-add! router "POST" "/feeds/entry/:id/pin"
        (require-auth auth
          (lambda (req params)
            (let ((id (string->number (params-ref params "id"))))
              (when id (pin-entry! cfg id))
              (make-http-response 302
                (list (cons "Location"
                            ;; Pin from the feeds list → stay on feeds;
                            ;; pin from pinned (rare; e.g. re-pin) →
                            ;; stay on pinned. We don't have referer
                            ;; handling, so default to /feeds.
                            "/feeds"))
                "")))))

      (router-add! router "POST" "/feeds/entry/:id/unpin"
        (require-auth auth
          (lambda (req params)
            (let ((id (string->number (params-ref params "id"))))
              (when id (unpin-entry! cfg id))
              (make-http-response 302
                (list (cons "Location" "/feeds/pinned")) "")))))

      (router-add! router "POST" "/feeds/pinned/add"
        (require-auth auth
          (lambda (req params)
            (let* ((form    (parse-www-form (or (http-request-body req) "")))
                   (link    (string-trim-both (form-ref form "link" "")))
                   (title   (string-trim-both (form-ref form "title" "")))
                   (cat     (string-trim-both (form-ref form "category" "misc")))
                   (lbl     (string-trim-both (form-ref form "label" "")))
                   (summary (form-ref form "summary" "")))
              (cond
                ((or (string=? link "") (string=? title ""))
                 (render-error 400 "URL and title are required."))
                (else
                 (add-manual-pin! cfg link title
                                  (cond ((string=? cat "") "misc") (else cat))
                                  lbl summary)
                 (make-http-response 302
                   (list (cons "Location" "/feeds/pinned")) "")))))))

      (router-add! router "POST" "/feeds/pinned/remove-stale"
        (require-auth auth
          (lambda (req params)
            (unpin-stale! cfg)
            (make-http-response 302
              (list (cons "Location" "/feeds/pinned")) ""))))

      (router-add! router "POST" "/feeds/pinned/remove-current"
        (require-auth auth
          (lambda (req params)
            (let* ((form  (parse-www-form (or (http-request-body req) "")))
                   (q     (form-ref form "q"     ""))
                   (cat   (form-ref form "cat"   ""))
                   (feed  (form-ref form "feed"  ""))
                   (since (form-ref form "since" ""))
                   (until (form-ref form "until" ""))
                   (stale (form-ref form "stale" ""))
                   (ids   (list-pinned-ids cfg
                            (cond ((string=? q "")     #f) (else q))
                            (cond ((string=? cat "")   #f) (else cat))
                            (cond ((string=? feed "")  #f) (else feed))
                            (cond ((string=? since "") #f) (else since))
                            (cond ((string=? until "") #f) (else until))
                            (string=? stale "1"))))
              (unpin-by-ids! cfg ids)
              (make-http-response 302
                (list (cons "Location" (pinned-redirect form))) "")))))

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
