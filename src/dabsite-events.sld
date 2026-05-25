(define-library (dabsite events)
  (import (scheme base)
          (scheme write)
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
          (dabsite util)
          (dabsite auth)
          (dabsite views))
  (export install-events-routes!
          ;; helpers shared with calendar overlay
          events-for-date-range
          event-kind-label
          event-kind-colour
          format-event-label)
  (begin

    ;; ============================================================
    ;; Kinds
    ;; ============================================================
    ;;
    ;; Each kind: (key  label  colour  value-mode)
    ;; value-mode: 'indicator | 'bp | 'weight
    ;; The colour is used both for the calendar overlay swatch and
    ;; the chart strokes on the report page.

    (define event-kinds
      '(("training"  "Training"  "#2e7d32" indicator)
        ("joggen"    "Joggen"    "#1565c0" indicator)
        ("wandern"   "Wandern"   "#6d4c41" indicator)
        ("spazieren" "Spazieren" "#558b2f" indicator)
        ("migraene"  "Migräne"   "#c62828" indicator)
        ("kirche"    "Kirche"    "#5e35b1" indicator)
        ("blutdruck" "Blutdruck" "#7b1fa2" bp)
        ("gewicht"   "Gewicht"   "#ef6c00" weight)))

    (define (kind-row key)
      (find (lambda (r) (string=? (car r) key)) event-kinds))

    (define (event-kind-label key)
      (let ((r (kind-row key))) (if r (cadr r) key)))

    (define (event-kind-colour key)
      (let ((r (kind-row key))) (if r (caddr r) "#888")))

    (define (event-kind-mode key)
      (let ((r (kind-row key))) (if r (cadddr r) 'indicator)))

    (define (valid-kind? key)
      (and (kind-row key) #t))

    ;; ============================================================
    ;; Pure helpers
    ;; ============================================================

    (define (parse-positive s lo hi)
      ;; Returns the value as an inexact real, or #f on failure.
      ;; Accepts comma as decimal separator. lo/hi are inclusive bounds.
      (cond
        ((not (string? s)) #f)
        (else
         (let ((s2 (string-trim-both
                    (let ((out (open-output-string)))
                      (string-for-each
                       (lambda (c) (write-char (if (char=? c #\,) #\. c) out))
                       s)
                      (get-output-string out)))))
           (cond
             ((string=? s2 "") #f)
             (else
              (let ((n (string->number s2)))
                (and n (real? n) (>= n lo) (<= n hi)
                     (inexact n)))))))))

    ;; Sanity bounds — generous enough that no realistic real-world
    ;; entry is rejected, tight enough to catch fat-finger mistakes.
    (define (parse-systolic  s) (parse-positive s 50 260))
    (define (parse-diastolic s) (parse-positive s 30 160))
    (define (parse-pulse     s) (parse-positive s 30 220))
    (define (parse-weight    s) (parse-positive s 20 300))

    ;; ============================================================
    ;; DB ops
    ;; ============================================================

    (define (nul v) (if v v 'null))

    (define (insert-event! cfg kind recorded-str v1 v2 v3 notes)
      ;; recorded-str: "YYYY-MM-DDTHH:MM" or "" for now().
      ;; v1/v2/v3: real numbers or #f (NULL).
      (let ((use-now? (or (not recorded-str) (string=? recorded-str ""))))
        (cond
          (use-now?
           (exec cfg
             "INSERT INTO events (kind, v1, v2, v3, notes) VALUES ($1, $2, $3, $4, $5)"
             (list kind (nul v1) (nul v2) (nul v3) (or notes ""))))
          (else
           (exec cfg
             (string-append
              "INSERT INTO events (kind, recorded_at, v1, v2, v3, notes) "
              "VALUES ($1, $2::timestamptz, $3, $4, $5, $6)")
             (list kind recorded-str
                   (nul v1) (nul v2) (nul v3) (or notes "")))))))

    (define (delete-event! cfg id)
      (exec cfg "DELETE FROM events WHERE id = $1" (list id)))

    (define (recent-events cfg limit)
      (alist-rows cfg
        (string-append
         "SELECT id::text AS id, kind, "
         "       to_char(recorded_at, 'YYYY-MM-DD HH24:MI') AS recorded, "
         "       COALESCE(v1::text,'') AS v1, "
         "       COALESCE(v2::text,'') AS v2, "
         "       COALESCE(v3::text,'') AS v3, "
         "       notes "
         "FROM events "
         "ORDER BY recorded_at DESC, id DESC "
         "LIMIT " (number->string limit))))

    (define (report-rows cfg kind from-d to-d)
      (let ((clauses (list "kind = $1"))
            (params  (list kind))
            (n 1))
        (define (add v)
          (set! n (+ n 1))
          (set! params (append params (list v)))
          (string-append "$" (number->string n)))
        (when (non-empty-string? from-d)
          (set! clauses
                (cons (string-append "recorded_at >= " (add from-d) "::timestamptz")
                      clauses)))
        (when (non-empty-string? to-d)
          (set! clauses
                (cons (string-append
                       "recorded_at < (" (add to-d) "::date + 1)::timestamptz")
                      clauses)))
        (alist-rows cfg
          (string-append
           "SELECT id::text AS id, "
           "       to_char(recorded_at, 'YYYY-MM-DD HH24:MI') AS recorded, "
           "       to_char(recorded_at, 'YYYY-MM-DD') AS day, "
           "       COALESCE(v1::text,'') AS v1, "
           "       COALESCE(v2::text,'') AS v2, "
           "       COALESCE(v3::text,'') AS v3 "
           "FROM events "
           "WHERE " (string-join clauses " AND ") " "
           "ORDER BY recorded_at ASC, id ASC")
          params)))

    ;; --- For calendar overlay ---
    ;;
    ;; Returns alist rows with day (YYYY-MM-DD) and kind, distinct
    ;; per (day, kind) — one synthetic all-day chip per kind per day.

    (define (events-for-date-range cfg from-date-str to-date-str)
      ;; One row per (day, kind) — for kinds with values, takes the
      ;; LAST reading of the day. v1/v2/v3 are empty strings for
      ;; indicator kinds.
      (alist-rows cfg
        (string-append
         "SELECT DISTINCT ON (day, kind) "
         "       to_char(recorded_at, 'YYYY-MM-DD') AS day, "
         "       kind, "
         "       COALESCE(v1::text, '') AS v1, "
         "       COALESCE(v2::text, '') AS v2, "
         "       COALESCE(v3::text, '') AS v3 "
         "FROM events "
         "WHERE recorded_at >= $1::date "
         "  AND recorded_at < ($2::date + 1) "
         "ORDER BY day, kind, recorded_at DESC")
        (list from-date-str to-date-str)))

    (define (trim-trailing-zeros s)
      ;; "75.0" -> "75", "75.50" -> "75.5". Leaves non-decimal strings alone.
      (cond
        ((not (string-index s #\.)) s)
        (else
         (let loop ((i (string-length s)))
           (cond
             ((= i 0) s)
             ((char=? (string-ref s (- i 1)) #\0)
              (loop (- i 1)))
             ((char=? (string-ref s (- i 1)) #\.)
              (substring s 0 (- i 1)))
             (else (substring s 0 i)))))))

    (define (format-event-label kind v1 v2 v3)
      ;; The synthetic-occurrence title shown on calendar chips.
      (let ((base (event-kind-label kind)))
        (cond
          ((string=? kind "blutdruck")
           (cond
             ((or (string=? v1 "") (string=? v2 "")) base)
             (else
              (string-append base " "
                             (trim-trailing-zeros v1) "/"
                             (trim-trailing-zeros v2)
                             (if (string=? v3 "")
                                 ""
                                 (string-append "/" (trim-trailing-zeros v3)))))))
          ((string=? kind "gewicht")
           (cond
             ((string=? v1 "") base)
             (else
              (string-append base " " (trim-trailing-zeros v1) " kg"))))
          (else base))))

    ;; ============================================================
    ;; Views
    ;; ============================================================

    (define (page req auth title body)
      (html-response
        (render-page req auth
                     (list (cons 'title title)
                           (cons 'active 'events)
                           (cons 'body-class "events-page"))
                     (html->string body))))

    (define (assq-ref alist key)
      (let ((p (assq key alist))) (cond (p (cdr p)) (else #f))))

    (define (indicator-button kind)
      (let ((key    (car  kind))
            (label  (cadr kind)))
        `(form (@ (method "post") (action "/events") (class "ev-quick"))
           (input (@ (type "hidden") (name "kind") (value ,key)))
           (button (@ (type "submit")
                      (class ,(string-append "ev-tap ev-k-" key)))
             (span (@ (class "ev-tap-label")) ,label)))))

    (define (bp-form)
      `(form (@ (method "post") (action "/events")
                (class "ev-value-form ev-k-blutdruck"))
         (input (@ (type "hidden") (name "kind") (value "blutdruck")))
         (h3 (@ (class "ev-form-title")) "Blutdruck")
         (div (@ (class "ev-bp-row"))
           (label "Sys"
             (input (@ (type "number") (name "v1") (required #t)
                       (inputmode "numeric") (min "50") (max "260")
                       (placeholder "120"))))
           (label "Dia"
             (input (@ (type "number") (name "v2") (required #t)
                       (inputmode "numeric") (min "30") (max "160")
                       (placeholder "80"))))
           (label "Puls"
             (input (@ (type "number") (name "v3")
                       (inputmode "numeric") (min "30") (max "220")
                       (placeholder "70")))))
         (button (@ (type "submit") (class "ev-record")) "Aufzeichnen")))

    (define (weight-form)
      `(form (@ (method "post") (action "/events")
                (class "ev-value-form ev-k-gewicht"))
         (input (@ (type "hidden") (name "kind") (value "gewicht")))
         (h3 (@ (class "ev-form-title")) "Gewicht")
         (div (@ (class "ev-w-row"))
           (label "kg"
             (input (@ (type "number") (name "v1") (required #t)
                       (inputmode "decimal") (step "0.1")
                       (min "20") (max "300")
                       (placeholder "75.0")))))
         (button (@ (type "submit") (class "ev-record")) "Aufzeichnen")))

    (define (when-fieldset)
      `(details (@ (class "ev-when"))
         (summary "Anderer Zeitpunkt")
         (label "Aufzeichnungszeit "
           (input (@ (type "datetime-local") (name "recorded")
                     (form "ev-when-form"))))
         (p (@ (class "ev-hint"))
            "Leer = jetzt. Beim Setzen gilt der Zeitpunkt für die nächste "
            "Aufzeichnung — Knopf drücken nach Wahl der Zeit.")))

    ;; We avoid wiring the "when" picker across the per-button forms
    ;; (would need JS to copy the value); instead we render one extra
    ;; explicit form below the buttons for the rare backfill case.

    (define (backfill-form indicator-kinds)
      `(form (@ (method "post") (action "/events") (class "ev-backfill"))
         (h3 "Manuell hinzufügen")
         (label "Art "
           (select (@ (name "kind") (required #t))
             ,@(map (lambda (k)
                      `(option (@ (value ,(car k))) ,(cadr k)))
                    event-kinds)))
         (label "Wann "
           (input (@ (type "datetime-local") (name "recorded"))))
         (div (@ (class "ev-bf-vals"))
           (label "v1 "
             (input (@ (type "text") (name "v1") (placeholder "Sys / kg"))))
           (label "v2 "
             (input (@ (type "text") (name "v2") (placeholder "Dia"))))
           (label "v3 "
             (input (@ (type "text") (name "v3") (placeholder "Puls")))))
         (label "Notiz "
           (input (@ (type "text") (name "notes") (maxlength "500"))))
         (button (@ (type "submit")) "Speichern")))

    (define (event-value-summary e)
      (let* ((kind (row-field e "kind"))
             (mode (event-kind-mode kind))
             (v1   (row-field e "v1"))
             (v2   (row-field e "v2"))
             (v3   (row-field e "v3")))
        (case mode
          ((bp)
           (string-append
            (if (string=? v1 "") "?" v1) "/"
            (if (string=? v2 "") "?" v2)
            (if (string=? v3 "") "" (string-append " (P " v3 ")"))))
          ((weight)
           (cond ((string=? v1 "") "—") (else (string-append v1 " kg"))))
          (else ""))))

    (define (event-row-sxml e)
      (let* ((id     (row-field e "id"))
             (kind   (row-field e "kind"))
             (label  (event-kind-label kind))
             (colour (event-kind-colour kind))
             (when-s (row-field e "recorded"))
             (val    (event-value-summary e)))
        `(tr
           (td (@ (class "ev-when-c")) ,when-s)
           (td (@ (class "ev-kind"))
             (span (@ (class ,(string-append "ev-swatch ev-k-" kind))))
             " " ,label)
           (td (@ (class "ev-val")) ,val)
           (td (@ (class "acts"))
               (form (@ (method "post")
                        (action ,(string-append "/events/" id "/delete"))
                        (class "inline")
                        (data-confirm "Eintrag löschen?"))
                 (button (@ (class "linkish danger")) "del"))))))

    (define (recent-list-sxml events)
      (cond
        ((null? events) `(p (@ (class "empty")) "Noch nichts aufgezeichnet."))
        (else
         `(table (@ (class "ev-recent mobile-cards"))
            (thead (tr (th "wann") (th "art") (th "wert") (th)))
            (tbody ,@(map event-row-sxml events))))))

    (define (render-main req auth cfg)
      (let* ((events (recent-events cfg 50))
             (indicators (filter (lambda (k) (eq? (cadddr k) 'indicator))
                                 event-kinds))
             (body
               `((header (@ (class "feeds-head"))
                   (h1 "Events")
                   (a (@ (class "admin-link")
                         (href "/events/report?kind=blutdruck"))
                      "Blutdruck-Report")
                   " "
                   (a (@ (class "admin-link")
                         (href "/events/report?kind=gewicht"))
                      "Gewichts-Report"))
                 (section (@ (class "ev-quickbar"))
                   ,@(map indicator-button indicators))
                 (section (@ (class "ev-value-forms"))
                   ,(bp-form)
                   ,(weight-form))
                 (section (@ (class "ev-backfill-wrap"))
                   ,(backfill-form indicators))
                 (section (@ (class "ev-recent-wrap"))
                   (h2 "Zuletzt")
                   ,(recent-list-sxml events)))))
        (page req auth "Events" body)))

    ;; ---- report ----

    (define default-report-days 90)

    (define (default-from-date cfg days)
      (let ((rs (rows cfg
                  (string-append
                   "SELECT to_char(now() - interval '"
                   (number->string days) " days', 'YYYY-MM-DD')"))))
        (if (pair? rs) (vector-ref (car rs) 0) "")))

    (define (default-to-date cfg)
      (let ((rs (rows cfg "SELECT to_char(now(), 'YYYY-MM-DD')")))
        (if (pair? rs) (vector-ref (car rs) 0) "")))

    (define (chart-svg series x-count colour width height)
      ;; series: list of (label colour values), values is list of
      ;; numbers (#f for gaps). Renders a single SVG with all series.
      (let* ((w width) (h height)
             (pad-l 36) (pad-r 8) (pad-t 8) (pad-b 22)
             (inner-w (- w pad-l pad-r))
             (inner-h (- h pad-t pad-b))
             (all-nums
              (filter (lambda (n) (and n (real? n)))
                      (apply append (map caddr series))))
             (ymin (if (null? all-nums) 0
                       (apply min all-nums)))
             (ymax (if (null? all-nums) 1
                       (apply max all-nums)))
             (yrange (max 1 (- ymax ymin)))
             (ymin-p (- ymin (* 0.05 yrange)))
             (ymax-p (+ ymax (* 0.05 yrange)))
             (yspan  (max 1 (- ymax-p ymin-p)))
             (n      (max 1 x-count))
             (step-x (if (= n 1) 0 (/ inner-w (- n 1)))))
        (define (x-at i)
          (+ pad-l (if (= n 1) (/ inner-w 2) (* step-x i))))
        (define (y-at v)
          (+ pad-t (- inner-h (* inner-h (/ (- v ymin-p) yspan)))))
        (define (num->str n)
          (let* ((r (* (round (* n 10)) 0.1)))
            (number->string r)))
        (define (path-of values)
          (let loop ((i 0) (vs values) (started? #f) (out '()))
            (cond
              ((null? vs) (string-join (reverse out) " "))
              ((not (car vs))
               (loop (+ i 1) (cdr vs) #f out))
              (else
               (let* ((cmd (if started? "L" "M"))
                      (xs (num->str (x-at i)))
                      (ys (num->str (y-at (car vs)))))
                 (loop (+ i 1) (cdr vs) #t
                       (cons (string-append cmd xs "," ys) out)))))))
        (define (points-of values colour)
          (let loop ((i 0) (vs values) (out '()))
            (cond
              ((null? vs) (reverse out))
              ((not (car vs)) (loop (+ i 1) (cdr vs) out))
              (else
               (loop (+ i 1) (cdr vs)
                     (cons `(circle (@ (cx ,(num->str (x-at i)))
                                       (cy ,(num->str (y-at (car vs))))
                                       (r "2.5")
                                       (fill ,colour)))
                           out))))))
        `(svg (@ (class "ev-chart")
                 (viewBox ,(string-append "0 0 "
                                          (number->string w) " "
                                          (number->string h)))
                 (width "100%")
                 (preserveAspectRatio "none")
                 (xmlns "http://www.w3.org/2000/svg"))
           ;; axes
           (line (@ (x1 ,(num->str pad-l)) (y1 ,(num->str pad-t))
                    (x2 ,(num->str pad-l)) (y2 ,(num->str (+ pad-t inner-h)))
                    (stroke "currentColor") (stroke-opacity "0.3")))
           (line (@ (x1 ,(num->str pad-l))
                    (y1 ,(num->str (+ pad-t inner-h)))
                    (x2 ,(num->str (+ pad-l inner-w)))
                    (y2 ,(num->str (+ pad-t inner-h)))
                    (stroke "currentColor") (stroke-opacity "0.3")))
           ;; y labels
           (text (@ (x "4") (y ,(num->str (+ pad-t 4)))
                    (font-size "10") (fill "currentColor"))
                 ,(num->str ymax-p))
           (text (@ (x "4") (y ,(num->str (+ pad-t inner-h)))
                    (font-size "10") (fill "currentColor"))
                 ,(num->str ymin-p))
           ;; series paths + points
           ,@(append-map
              (lambda (s)
                (let ((label  (car s))
                      (col    (cadr s))
                      (values (caddr s)))
                  (cons
                   `(path (@ (d ,(path-of values))
                             (fill "none")
                             (stroke ,col)
                             (stroke-width "1.5")))
                   (points-of values col))))
              series))))

    (define (running-average values window)
      ;; values: list of numbers or #f (gaps). Returns list of running
      ;; averages with the same length; #f where insufficient data.
      (let* ((vec (list->vector values))
             (n   (vector-length vec)))
        (let loop ((i 0) (out '()))
          (cond
            ((= i n) (reverse out))
            (else
             (let inner ((j (max 0 (- i (- window 1))))
                         (sum 0) (count 0))
               (cond
                 ((> j i)
                  (loop (+ i 1)
                        (cons (if (= count 0) #f (/ sum count)) out)))
                 (else
                  (let ((v (vector-ref vec j)))
                    (inner (+ j 1)
                           (if v (+ sum v) sum)
                           (if v (+ count 1) count)))))))))))

    (define (one-per-day rows kind)
      ;; rows: list of report-rows alists, ascending. For each day,
      ;; keeps the LAST entry. Returns list of (day . row).
      (let loop ((in rows) (acc '()))
        (cond
          ((null? in) (reverse acc))
          (else
           (let* ((r (car in))
                  (d (row-field r "day"))
                  (p (assoc d acc)))
             (cond
               (p (set-cdr! p r) (loop (cdr in) acc))
               (else (loop (cdr in) (cons (cons d r) acc)))))))))

    (define (extract-numbers daily field)
      (map (lambda (p)
             (let ((s (row-field (cdr p) field)))
               (and (non-empty-string? s)
                    (let ((n (string->number s)))
                      (and n (real? n) (inexact n))))))
           daily))

    (define (render-report req auth cfg kind from-d to-d view avg-window)
      (let* ((from*  (if (non-empty-string? from-d) from-d
                         (default-from-date cfg default-report-days)))
             (to*    (if (non-empty-string? to-d) to-d
                         (default-to-date cfg)))
             (rows   (report-rows cfg kind from* to*))
             (daily  (one-per-day rows kind))
             (label  (event-kind-label kind))
             (mode   (event-kind-mode kind))
             (avg-w  (max 0 (or avg-window 0)))
             (link   (lambda (params extra)
                       (string-append
                        "/events/report?"
                        (string-join
                         (filter (lambda (s) (not (string=? s "")))
                                 (list
                                  (string-append "kind=" kind)
                                  (if (non-empty-string? from*)
                                      (string-append "from=" (percent-encode from*))
                                      "")
                                  (if (non-empty-string? to*)
                                      (string-append "to=" (percent-encode to*))
                                      "")
                                  extra))
                         "&"))))
             (body
               `((header (@ (class "feeds-head"))
                   (h1 ,(string-append label " Report"))
                   (a (@ (class "admin-link") (href "/events"))
                      ,(raw "← Events")))
                 (form (@ (method "get") (action "/events/report")
                          (class "feed-filters"))
                   (input (@ (type "hidden") (name "kind") (value ,kind)))
                   (label "von "
                     (input (@ (type "date") (name "from") (value ,from*))))
                   (label "bis "
                     (input (@ (type "date") (name "to")   (value ,to*))))
                   (label "Ansicht "
                     (select (@ (name "view"))
                       (option (@ (value "table")
                                  (selected ,(and (string=? view "table") #t)))
                               "Tabelle")
                       (option (@ (value "graph")
                                  (selected ,(and (string=? view "graph") #t)))
                               "Grafik")))
                   (label "Mittel "
                     (input (@ (type "number") (name "avg")
                               (min "0") (max "60") (step "1")
                               (value ,(number->string avg-w))
                               (placeholder "0"))))
                   (button (@ (type "submit")) "Anwenden"))
                 ,(cond
                    ((null? daily)
                     `(p (@ (class "empty")) "Keine Daten im Zeitraum."))
                    ((string=? view "graph")
                     (report-graph-sxml daily mode kind avg-w))
                    (else
                     (report-table-sxml daily mode rows))))))
        (page req auth (string-append label " Report") body)))

    (define (report-graph-sxml daily mode kind avg-w)
      (let* ((n     (length daily))
             (col   (event-kind-colour kind))
             (xlabs (map car daily))
             (series
              (case mode
                ((bp)
                 (let ((sys (extract-numbers daily "v1"))
                       (dia (extract-numbers daily "v2"))
                       (pul (extract-numbers daily "v3")))
                   (let ((base (list
                                (list "Sys"  "#c62828" sys)
                                (list "Dia"  "#1565c0" dia)
                                (list "Puls" "#6a1b9a" pul))))
                     (cond
                       ((> avg-w 1)
                        (append base
                                (list (list "Sys-Ø" "#c6282880"
                                            (running-average sys avg-w))
                                      (list "Dia-Ø" "#1565c080"
                                            (running-average dia avg-w))
                                      (list "Puls-Ø" "#6a1b9a80"
                                            (running-average pul avg-w)))))
                       (else base)))))
                ((weight)
                 (let ((w (extract-numbers daily "v1")))
                   (cond
                     ((> avg-w 1)
                      (list (list "kg" col w)
                            (list "Ø"  (string-append col "80")
                                  (running-average w avg-w))))
                     (else (list (list "kg" col w))))))
                (else '())))
             (legend
              `(ul (@ (class "ev-legend"))
                ,@(map (lambda (s)
                         `(li (svg (@ (class "ev-swatch")
                                      (width "10") (height "10")
                                      (viewBox "0 0 10 10")
                                      (xmlns "http://www.w3.org/2000/svg"))
                                (rect (@ (width "10") (height "10")
                                         (fill ,(cadr s)))))
                              " " ,(car s)))
                       series))))
        `(div (@ (class "ev-chart-wrap"))
           ,(chart-svg series n col 720 280)
           ,legend
           (p (@ (class "ev-chart-x"))
              ,(if (pair? xlabs) (car xlabs) "")
              " — "
              ,(if (pair? xlabs)
                   (car (reverse xlabs))
                   "")))))

    (define (report-table-sxml daily mode all-rows)
      (case mode
        ((bp)
         `(table (@ (class "ev-report mobile-cards"))
            (thead (tr (th "Tag") (th "Sys") (th "Dia") (th "Puls")))
            (tbody
             ,@(map
                (lambda (p)
                  (let ((r (cdr p)))
                    `(tr (td ,(car p))
                         (td ,(row-field r "v1"))
                         (td ,(row-field r "v2"))
                         (td ,(row-field r "v3")))))
                daily))))
        ((weight)
         `(table (@ (class "ev-report mobile-cards"))
            (thead (tr (th "Tag") (th "kg")))
            (tbody
             ,@(map
                (lambda (p)
                  (let ((r (cdr p)))
                    `(tr (td ,(car p))
                         (td ,(row-field r "v1")))))
                daily))))
        (else `(p "Kein Tabellenformat für diese Art."))))

    ;; ============================================================
    ;; Routes
    ;; ============================================================

    (define (param-or req name default)
      (let ((p (assoc name (url-query-params (http-request-url req)))))
        (cond
          ((and p (string? (cdr p))) (percent-decode (cdr p)))
          (else default))))

    (define (install-events-routes! router cfg auth)

      (router-add! router "GET" "/events"
        (require-auth auth
          (lambda (req params)
            (render-main req auth cfg))))

      (router-add! router "POST" "/events"
        (require-auth auth
          (lambda (req params)
            (let* ((form  (parse-www-form (or (http-request-body req) "")))
                   (kind  (string-trim-both (form-ref form "kind" "")))
                   (when-s (string-trim-both (form-ref form "recorded" "")))
                   (v1-raw (string-trim-both (form-ref form "v1" "")))
                   (v2-raw (string-trim-both (form-ref form "v2" "")))
                   (v3-raw (string-trim-both (form-ref form "v3" "")))
                   (notes  (string-trim-both (form-ref form "notes" ""))))
              (cond
                ((not (valid-kind? kind))
                 (render-error 400 "Ungültige Art."))
                (else
                 (let ((mode (event-kind-mode kind)))
                   (case mode
                     ((indicator)
                      (insert-event! cfg kind when-s #f #f #f notes)
                      (redirect-to "/events"))
                     ((bp)
                      (let ((sys (parse-systolic v1-raw))
                            (dia (parse-diastolic v2-raw))
                            (pul (and (non-empty-string? v3-raw)
                                      (parse-pulse v3-raw))))
                        (cond
                          ((or (not sys) (not dia))
                           (render-error 400
                             "Blutdruck: Sys/Dia erforderlich (Plausibilität geprüft)."))
                          ((and (non-empty-string? v3-raw) (not pul))
                           (render-error 400 "Puls ausserhalb erwartetem Bereich."))
                          (else
                           (insert-event! cfg kind when-s sys dia
                                          (or pul #f) notes)
                           (redirect-to "/events")))))
                     ((weight)
                      (let ((w (parse-weight v1-raw)))
                        (cond
                          ((not w)
                           (render-error 400 "Gewicht: Wert erforderlich (kg)."))
                          (else
                           (insert-event! cfg kind when-s w #f #f notes)
                           (redirect-to "/events")))))))))))))

      (router-add! router "POST" "/events/:id/delete"
        (require-auth auth
          (lambda (req params)
            (let ((id (string->number (params-ref params "id"))))
              (when id (delete-event! cfg id))
              (redirect-to "/events")))))

      (router-add! router "GET" "/events/report"
        (require-auth auth
          (lambda (req params)
            (let* ((kind (param-or req "kind" "blutdruck"))
                   (from-d (param-or req "from" ""))
                   (to-d   (param-or req "to" ""))
                   (view   (let ((v (param-or req "view" "graph")))
                             (if (or (string=? v "table")
                                     (string=? v "graph"))
                                 v "graph")))
                   (avg    (string->number (param-or req "avg" "0"))))
              (cond
                ((not (valid-kind? kind))
                 (render-error 400 "Unbekannte Art."))
                ((not (memv (event-kind-mode kind) '(bp weight)))
                 (render-error 400 "Für diese Art gibt es keinen Report."))
                (else
                 (render-report req auth cfg kind from-d to-d view
                                (and avg (exact (max 0 (round avg))))))))))))

    (define (redirect-to path)
      (make-http-response 302 (list (cons "Location" path)) ""))

))
