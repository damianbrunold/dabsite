(define-library (dabsite calendar)
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
          (dabsite util)
          (dabsite auth)
          (dabsite views))
  (export install-calendar-routes!
          ;; pure helpers exposed for tests
          parse-quick-add
          date->jdn
          jdn->date
          date+days
          weekday-of           ; 1=Mon ... 7=Sun (ISO)
          format-date
          format-time
          format-datetime
          parse-rrule
          expand-recurrence
          last-day-of-month
          weekday-name->iso
          pad2)
  (begin

    ;; ============================================================
    ;; Date arithmetic (proleptic Gregorian, Julian Day Number).
    ;; All times handled here are wall-clock; we let postgres deal
    ;; with timezones via timestamptz casts.
    ;; ============================================================

    (define (pad2 n)
      (let ((s (number->string n)))
        (if (< n 10) (string-append "0" s) s)))

    (define (date->jdn y m d)
      (let* ((a  (quotient (- 14 m) 12))
             (y2 (- (+ y 4800) a))
             (m2 (- (+ m (* 12 a)) 3)))
        (+ d
           (quotient (+ (* 153 m2) 2) 5)
           (* 365 y2)
           (quotient y2 4)
           (- (quotient y2 100))
           (quotient y2 400)
           -32045)))

    (define (jdn->date jdn)
      ;; Returns (year month day).
      (let* ((a (+ jdn 32044))
             (b (quotient (+ (* 4 a) 3) 146097))
             (c (- a (quotient (* 146097 b) 4)))
             (d (quotient (+ (* 4 c) 3) 1461))
             (e (- c (quotient (* 1461 d) 4)))
             (m (quotient (+ (* 5 e) 2) 153))
             (day   (+ 1 (- e (quotient (+ (* 153 m) 2) 5))))
             (month (- (+ m 3) (* 12 (quotient m 10))))
             (year  (+ (* 100 b) d -4800 (quotient m 10))))
        (list year month day)))

    (define (date+days date n)
      (jdn->date (+ (apply date->jdn date) n)))

    (define (weekday-of date)
      ;; ISO weekday: 1=Mon ... 7=Sun. JDN 0 is a Monday in the
      ;; proleptic Julian-day system; (JDN+1) mod 7 gives 0=Sun,
      ;; 1=Mon, ..., 6=Sat, which we remap to ISO.
      (let* ((jdn (apply date->jdn date))
             (w   (modulo (+ jdn 1) 7)))
        (if (= w 0) 7 w)))

    (define (leap-year? y)
      (and (= 0 (modulo y 4))
           (or (not (= 0 (modulo y 100)))
               (= 0 (modulo y 400)))))

    (define (last-day-of-month y m)
      (cond
        ((memv m '(1 3 5 7 8 10 12)) 31)
        ((memv m '(4 6 9 11))        30)
        ((leap-year? y)              29)
        (else                        28)))

    (define (date+months date n)
      ;; Clamps to the last valid day of the target month so
      ;; (date+months '(2026 1 31) 1) => '(2026 2 28).
      (let* ((y (car date)) (m (cadr date)) (d (caddr date))
             (total (+ (- m 1) n))
             (ny    (+ y (quotient total 12)))
             (nm    (+ 1 (modulo total 12)))
             (last  (last-day-of-month ny nm)))
        (list ny nm (min d last))))

    (define (date+years date n)
      (let* ((y (car date)) (m (cadr date)) (d (caddr date))
             (ny (+ y n))
             (last (last-day-of-month ny m)))
        (list ny m (min d last))))

    ;; ============================================================
    ;; Formatters
    ;; ============================================================

    (define (format-date d)
      ;; "YYYY-MM-DD"
      (string-append (number->string (car d))
                     "-" (pad2 (cadr d))
                     "-" (pad2 (caddr d))))

    (define (format-time t)
      ;; "HH:MM"
      (string-append (pad2 (car t)) ":" (pad2 (cadr t))))

    (define (format-datetime d t)
      ;; "YYYY-MM-DDTHH:MM"
      (string-append (format-date d) "T" (format-time t)))

    ;; ============================================================
    ;; Quick-add parser
    ;; ============================================================
    ;;
    ;; Input: text (string), today = (year month day weekday) where
    ;;        weekday is 1..7 (Mon..Sun); 'now-hour' is current hour.
    ;;
    ;; Returns an alist with keys (any may be absent or empty):
    ;;   title       string
    ;;   date        (y m d)  or #f
    ;;   time        (h mi)   or #f
    ;;   end-time    (h mi)   or #f         ; for "14:00-15:30"
    ;;   duration    integer minutes        or #f
    ;;   all-day     boolean
    ;;   rrule       string                 ; "" if none
    ;;   location    string                 ; "" if none
    ;;   category    string                 ; "" if none
    ;;   error       string                 ; "" if none

    (define weekday-table
      ;; lowercase token -> ISO weekday number.
      '(("mon" . 1) ("monday" . 1) ("mo" . 1) ("montag" . 1)
        ("tue" . 2) ("tuesday" . 2) ("tu" . 2) ("di" . 2) ("dienstag" . 2)
        ("wed" . 3) ("wednesday" . 3) ("we" . 3) ("mi" . 3) ("mittwoch" . 3)
        ("thu" . 4) ("thursday" . 4) ("th" . 4) ("do" . 4) ("donnerstag" . 4)
        ("fri" . 5) ("friday" . 5) ("fr" . 5) ("freitag" . 5)
        ("sat" . 6) ("saturday" . 6) ("sa" . 6) ("samstag" . 6) ("sonnabend" . 6)
        ("sun" . 7) ("sunday" . 7) ("su" . 7) ("so" . 7) ("sonntag" . 7)))

    (define (weekday-name->iso s)
      (let ((p (assoc (string-downcase s) weekday-table)))
        (and p (cdr p))))

    (define (next-weekday-on-or-after today iso-wd)
      (let* ((cur (weekday-of today))
             (delta (modulo (- iso-wd cur) 7)))
        (date+days today delta)))

    (define (next-weekday-strict-after today iso-wd)
      ;; Always > today.
      (let* ((cur (weekday-of today))
             (delta (modulo (- iso-wd cur) 7)))
        (date+days today (if (= delta 0) 7 delta))))

    (define daily-words
      '("daily" "täglich" "taglich"))
    (define weekly-words
      '("weekly" "wöchentlich" "wochentlich"))
    (define monthly-words
      '("monthly" "monatlich"))
    (define yearly-words
      '("yearly" "annually" "jährlich" "jahrlich"))
    (define allday-words
      '("allday" "all-day" "ganztägig" "ganztagig"))
    (define every-words
      '("every" "jeden" "jede"))
    (define for-words
      '("for" "für" "fur"))
    (define today-words
      '("today" "heute"))
    (define tomorrow-words
      '("tomorrow" "morgen"))
    (define dayaftertomorrow-words
      '("übermorgen" "uebermorgen"))
    (define tonight-words
      '("tonight" "heute-abend"))
    (define yesterday-words
      '("yesterday" "gestern"))
    (define in-words
      '("in"))
    (define unit-day-words
      '("day" "days" "tag" "tage" "tagen"))
    (define unit-week-words
      '("week" "weeks" "woche" "wochen"))
    (define unit-month-words
      '("month" "months" "monat" "monate" "monaten"))
    (define unit-year-words
      '("year" "years" "jahr" "jahre" "jahren"))

    (define (member-ci? s lst)
      (let ((lo (string-downcase s)))
        (any (lambda (x) (string=? x lo)) lst)))

    (define (split-whitespace s)
      (let ((n (string-length s)))
        (let loop ((i 0) (start 0) (acc '()) (in-word? #f))
          (cond
            ((= i n)
             (reverse (if in-word? (cons (substring s start n) acc) acc)))
            ((char-whitespace? (string-ref s i))
             (loop (+ i 1) (+ i 1)
                   (if in-word? (cons (substring s start i) acc) acc)
                   #f))
            (else
             (loop (+ i 1) (if in-word? start i) acc #t))))))

    (define (all-digits? s)
      (and (> (string-length s) 0)
           (string-every char-numeric? s)))

    (define (split-on s ch)
      (let ((n (string-length s)))
        (let loop ((i 0) (start 0) (acc '()))
          (cond
            ((= i n)
             (reverse (cons (substring s start n) acc)))
            ((char=? (string-ref s i) ch)
             (loop (+ i 1) (+ i 1) (cons (substring s start i) acc)))
            (else (loop (+ i 1) start acc))))))

    ;; ---- token classifiers (return alist of fields or #f) ----

    (define (try-time-range t)
      (let ((parts (split-on t #\-)))
        (and (= 2 (length parts))
             (let ((a (parse-clock (car parts)))
                   (b (parse-clock (cadr parts))))
               (and a b
                    (list (cons 'time a) (cons 'end-time b)))))))

    (define (try-hhmm t)
      (and (= 4 (string-length t)) (all-digits? t)
           (let ((h (string->number (substring t 0 2)))
                 (m (string->number (substring t 2 4))))
             (and (<= 0 h 23) (<= 0 m 59)
                  (list (cons 'time (list h m)))))))

    (define (try-time-token t)
      ;; "HH:MM", "HH:MM-HH:MM", "HHMM" (4 digits). Returns alist with
      ;; 'time and optional 'end-time, or #f.
      (or (try-time-range t)
          (cond ((parse-clock t)
                 => (lambda (hm) (list (cons 'time hm))))
                (else #f))
          (try-hhmm t)))

    (define (parse-clock s)
      ;; "HH:MM" -> (h m); returns #f otherwise.
      (let ((parts (split-on s #\:)))
        (and (= 2 (length parts))
             (all-digits? (car parts))
             (all-digits? (cadr parts))
             (let ((h (string->number (car parts)))
                   (m (string->number (cadr parts))))
               (and (<= 0 h 23) (<= 0 m 59) (list h m))))))

    (define (parse-hours-head head)
      ;; head is the part before 'h'. Accepts "1", "1.5", "1:30".
      (let ((p (split-on head #\:)))
        (cond
          ((= 2 (length p))
           (and (all-digits? (car p)) (all-digits? (cadr p))
                (let ((h (string->number (car p)))
                      (m (string->number (cadr p))))
                  (and (>= h 0) (<= 0 m 59) (+ (* h 60) m)))))
          (else
           (let ((v (string->number head)))
             (and v (real? v) (>= v 0)
                  (exact (round (* v 60)))))))))

    (define (try-duration-token t)
      ;; "1h", "90m", "1:30h" -> minutes. Returns alist with 'duration
      ;; or #f.
      (let ((n (string-length t)))
        (cond
          ((< n 2) #f)
          ((char=? (string-ref t (- n 1)) #\h)
           (let ((mins (parse-hours-head (substring t 0 (- n 1)))))
             (and mins (list (cons 'duration mins)))))
          ((char=? (string-ref t (- n 1)) #\m)
           (let ((v (string->number (substring t 0 (- n 1)))))
             (and v (integer? v) (>= v 0)
                  (list (cons 'duration (exact v))))))
          (else #f))))

    (define (try-date-keyword t today)
      ;; Single-token date keywords. Returns alist or #f.
      (cond
        ((member-ci? t today-words)
         (list (cons 'date today)))
        ((member-ci? t tomorrow-words)
         (list (cons 'date (date+days today 1))))
        ((member-ci? t dayaftertomorrow-words)
         (list (cons 'date (date+days today 2))))
        ((member-ci? t yesterday-words)
         (list (cons 'date (date+days today -1))))
        ((member-ci? t tonight-words)
         (list (cons 'date today) (cons 'time '(20 0))))
        ((weekday-name->iso t)
         => (lambda (wd)
              (list (cons 'date (next-weekday-on-or-after today wd)))))
        (else #f)))

    (define (try-iso-date t)
      ;; YYYY-MM-DD
      (let ((parts (split-on t #\-)))
        (and (= 3 (length parts))
             (all-digits? (car parts))
             (all-digits? (cadr parts))
             (all-digits? (caddr parts))
             (= 4 (string-length (car parts)))
             (let ((y (string->number (car parts)))
                   (m (string->number (cadr parts)))
                   (d (string->number (caddr parts))))
               (and (<= 1 m 12)
                    (<= 1 d (last-day-of-month y m))
                    (list (cons 'date (list y m d))))))))

    (define (try-dotted-or-slashed-date t today)
      ;; "16.5." / "16.5.2026" / "16/5" / "16/5/2026"
      (define (try sep)
        (let ((parts (filter (lambda (p) (not (string=? p "")))
                             (split-on t sep))))
          (and (or (= 2 (length parts)) (= 3 (length parts)))
               (every all-digits? parts)
               (let* ((d (string->number (car parts)))
                      (m (string->number (cadr parts)))
                      (y (if (= 3 (length parts))
                             (string->number (caddr parts))
                             (car today))))
                 (and (<= 1 m 12)
                      (<= 1 d (last-day-of-month y m))
                      (list (cons 'date (list y m d))))))))
      (or (and (string-index t #\.) (try #\.))
          (and (string-index t #\/) (try #\/))))

    (define (try-recurrence-word t)
      ;; daily/weekly/monthly/yearly + DE equivalents.
      (cond
        ((member-ci? t daily-words)
         (list (cons 'rrule "FREQ=DAILY")))
        ((member-ci? t weekly-words)
         (list (cons 'rrule "FREQ=WEEKLY")))
        ((member-ci? t monthly-words)
         (list (cons 'rrule "FREQ=MONTHLY")))
        ((member-ci? t yearly-words)
         (list (cons 'rrule "FREQ=YEARLY")))
        (else #f)))

    (define (try-every-clause tokens i today)
      ;; (every|jeden) <weekday>     -> WEEKLY BYDAY
      ;; (every|jeden) <N> <unit>    -> FREQ=...;INTERVAL=N
      ;; (every|jeden) <unit-sing>   -> daily/weekly/...
      ;; Returns (alist . consumed-count) or #f. Consumed-count is the
      ;; number of tokens used starting at i.
      (let* ((t  (list-ref tokens i))
             (t2 (and (< (+ i 1) (length tokens)) (list-ref tokens (+ i 1))))
             (t3 (and (< (+ i 2) (length tokens)) (list-ref tokens (+ i 2)))))
        (cond
          ((not (member-ci? t every-words)) #f)
          ;; every <weekday>
          ((and t2 (weekday-name->iso t2))
           => (lambda (wd)
                (cons (list (cons 'rrule
                                  (string-append "FREQ=WEEKLY;BYDAY="
                                                 (byday-code wd)))
                            (cons 'date
                                  (next-weekday-on-or-after today wd)))
                      2)))
          ;; every <N> <unit>
          ((and t2 t3 (all-digits? t2))
           (let ((n (string->number t2))
                 (freq (unit->freq t3)))
             (and n freq (> n 0)
                  (cons (list (cons 'rrule
                                    (string-append "FREQ=" freq
                                                   ";INTERVAL=" (number->string n))))
                        3))))
          ;; every <unit-sing>: every day/week/month/year
          ((and t2 (unit->freq t2))
           => (lambda (freq)
                (cons (list (cons 'rrule (string-append "FREQ=" freq))) 2)))
          (else #f))))

    (define (byday-code wd)
      (vector-ref #("MO" "TU" "WE" "TH" "FR" "SA" "SU") (- wd 1)))

    (define (unit->freq t)
      (cond
        ((member-ci? t unit-day-words)   "DAILY")
        ((member-ci? t unit-week-words)  "WEEKLY")
        ((member-ci? t unit-month-words) "MONTHLY")
        ((member-ci? t unit-year-words)  "YEARLY")
        (else #f)))

    (define (try-in-clause tokens i today)
      ;; "in N <unit>" — shifts date by N days/weeks/months/years.
      (let* ((t  (list-ref tokens i))
             (t2 (and (< (+ i 1) (length tokens)) (list-ref tokens (+ i 1))))
             (t3 (and (< (+ i 2) (length tokens)) (list-ref tokens (+ i 2)))))
        (cond
          ((not (member-ci? t in-words)) #f)
          ((and t2 t3 (all-digits? t2))
           (let ((n (string->number t2)))
             (cond
               ((member-ci? t3 unit-day-words)
                (cons (list (cons 'date (date+days today n))) 3))
               ((member-ci? t3 unit-week-words)
                (cons (list (cons 'date (date+days today (* 7 n)))) 3))
               ((member-ci? t3 unit-month-words)
                (cons (list (cons 'date (date+months today n))) 3))
               ((member-ci? t3 unit-year-words)
                (cons (list (cons 'date (date+years today n))) 3))
               (else #f))))
          (else #f))))

    (define (try-for-clause tokens i)
      ;; "for|für N..." consumes two tokens.
      (let* ((t  (list-ref tokens i))
             (t2 (and (< (+ i 1) (length tokens)) (list-ref tokens (+ i 1)))))
        (cond
          ((not (member-ci? t for-words)) #f)
          ((and t2 (try-duration-token t2))
           => (lambda (d) (cons d 2)))
          (else #f))))

    (define (try-next-clause tokens i today)
      ;; "next <weekday>" / "nächsten <weekday>"
      (let* ((t  (list-ref tokens i))
             (t2 (and (< (+ i 1) (length tokens)) (list-ref tokens (+ i 1))))
             (next? (or (string-ci=? t "next")
                        (string-ci=? t "nächsten")
                        (string-ci=? t "naechsten")
                        (string-ci=? t "naechste")
                        (string-ci=? t "nächste"))))
        (cond
          ((not next?) #f)
          ((and t2 (weekday-name->iso t2))
           => (lambda (wd)
                (cons (list (cons 'date (next-weekday-strict-after today wd)))
                      2)))
          (else #f))))

    ;; ---- main parser ----

    (define (merge-result acc add)
      ;; Adds keys from `add` to `acc` unless already present.
      (let loop ((add add) (acc acc))
        (cond
          ((null? add) acc)
          ((assq (car (car add)) acc)
           (loop (cdr add) acc))
          (else
           (loop (cdr add) (cons (car add) acc))))))

    (define (parse-quick-add text today)
      ;; today = (y m d). Returns alist.
      (let* ((text (or text ""))
             ;; Extract location: from first '@' to end of string.
             (at-idx (string-index text #\@))
             (location (if at-idx
                           (string-trim-both (substring text (+ at-idx 1)
                                                       (string-length text)))
                           ""))
             (rest (if at-idx
                       (substring text 0 at-idx)
                       text))
             (tokens (split-whitespace rest))
             (n (length tokens)))
        (let loop ((i 0)
                   (acc '())
                   (title-tokens '()))
          (cond
            ((= i n)
             (let* ((title (string-join (reverse title-tokens) " "))
                    (acc (if (string=? location "")
                             acc
                             (cons (cons 'location location) acc)))
                    (acc (cons (cons 'title (string-trim-both title)) acc))
                    ;; Default all-day to #f.
                    (acc (if (assq 'all-day acc)
                             acc
                             (cons (cons 'all-day #f) acc))))
               acc))
            (else
             (let ((t (list-ref tokens i)))
               (cond
                 ;; #category
                 ((and (> (string-length t) 0) (char=? (string-ref t 0) #\#))
                  (let ((cat (substring t 1 (string-length t))))
                    (loop (+ i 1)
                          (if (assq 'category acc)
                              acc
                              (cons (cons 'category cat) acc))
                          title-tokens)))
                 ;; all-day keyword
                 ((member-ci? t allday-words)
                  (loop (+ i 1) (cons (cons 'all-day #t) acc) title-tokens))
                 ;; multi-token clauses
                 ((try-every-clause tokens i today)
                  => (lambda (r)
                       (loop (+ i (cdr r))
                             (merge-result acc (car r))
                             title-tokens)))
                 ((try-in-clause tokens i today)
                  => (lambda (r)
                       (loop (+ i (cdr r))
                             (merge-result acc (car r))
                             title-tokens)))
                 ((try-for-clause tokens i)
                  => (lambda (r)
                       (loop (+ i (cdr r))
                             (merge-result acc (car r))
                             title-tokens)))
                 ((try-next-clause tokens i today)
                  => (lambda (r)
                       (loop (+ i (cdr r))
                             (merge-result acc (car r))
                             title-tokens)))
                 ;; single-token classifiers
                 ((try-recurrence-word t)
                  => (lambda (r)
                       (loop (+ i 1) (merge-result acc r) title-tokens)))
                 ((try-date-keyword t today)
                  => (lambda (r)
                       (loop (+ i 1) (merge-result acc r) title-tokens)))
                 ((try-iso-date t)
                  => (lambda (r)
                       (loop (+ i 1) (merge-result acc r) title-tokens)))
                 ((try-dotted-or-slashed-date t today)
                  => (lambda (r)
                       (loop (+ i 1) (merge-result acc r) title-tokens)))
                 ((try-time-token t)
                  => (lambda (r)
                       (loop (+ i 1) (merge-result acc r) title-tokens)))
                 ((try-duration-token t)
                  => (lambda (r)
                       (loop (+ i 1) (merge-result acc r) title-tokens)))
                 (else
                  (loop (+ i 1) acc (cons t title-tokens))))))))))

    ;; ============================================================
    ;; Recurrence (minimal RRULE subset)
    ;; ============================================================
    ;;
    ;; parse-rrule takes the string and returns alist with keys:
    ;;   freq      symbol: 'daily|'weekly|'monthly|'yearly
    ;;   interval  positive integer (default 1)
    ;;   byday     list of ISO weekday numbers (weekly only)
    ;;   count     integer or #f
    ;;   until     (y m d) or #f
    ;; Returns #f on parse failure.

    (define (parse-rrule s)
      (cond
        ((or (not (string? s)) (string=? s "")) #f)
        (else
         (let* ((parts (split-on s #\;))
                (kvs (map (lambda (p)
                            (let ((eq (string-index p #\=)))
                              (and eq
                                   (cons (string-upcase (substring p 0 eq))
                                         (substring p (+ eq 1)
                                                    (string-length p))))))
                          parts))
                (get (lambda (k) (let ((p (assoc k kvs))) (and p (cdr p)))))
                (freq-str (get "FREQ")))
           (and freq-str
                (let ((freq (cond
                              ((string-ci=? freq-str "DAILY") 'daily)
                              ((string-ci=? freq-str "WEEKLY") 'weekly)
                              ((string-ci=? freq-str "MONTHLY") 'monthly)
                              ((string-ci=? freq-str "YEARLY") 'yearly)
                              (else #f))))
                  (and freq
                       (let ((iv (or (and (get "INTERVAL")
                                          (string->number (get "INTERVAL")))
                                     1))
                             (byday (cond
                                      ((get "BYDAY")
                                       => (lambda (s)
                                            (filter-map byday->iso
                                                        (split-on s #\,))))
                                      (else '())))
                             (count (and (get "COUNT")
                                         (string->number (get "COUNT"))))
                             (until (and (get "UNTIL")
                                         (parse-rrule-until (get "UNTIL")))))
                         (and (integer? iv) (> iv 0)
                              (list (cons 'freq freq)
                                    (cons 'interval iv)
                                    (cons 'byday byday)
                                    (cons 'count count)
                                    (cons 'until until)))))))))))

    (define (byday->iso s)
      (let ((p (assoc (string-upcase s)
                      '(("MO" . 1) ("TU" . 2) ("WE" . 3) ("TH" . 4)
                        ("FR" . 5) ("SA" . 6) ("SU" . 7)))))
        (and p (cdr p))))

    (define (parse-rrule-until s)
      ;; Accepts YYYYMMDD or YYYYMMDDTHHMMSSZ. Returns date list.
      (and (>= (string-length s) 8)
           (let ((y (string->number (substring s 0 4)))
                 (m (string->number (substring s 4 6)))
                 (d (string->number (substring s 6 8))))
             (and y m d (list y m d)))))

    (define (date<=? a b)
      (<= (apply date->jdn a) (apply date->jdn b)))

    (define (date<? a b)
      (< (apply date->jdn a) (apply date->jdn b)))

    (define (expand-recurrence start-date rrule from-date to-date exdates limit)
      ;; Returns list of dates (occurrences) of start-date's series
      ;; that fall in [from-date, to-date] (inclusive), excluding any
      ;; in exdates (list of dates). limit caps how many to produce.
      (let ((r (parse-rrule rrule)))
        (cond
          ((not r)
           ;; Single occurrence.
           (if (and (date<=? from-date start-date)
                    (date<=? start-date to-date)
                    (not (member start-date exdates equal?)))
               (list start-date)
               '()))
          (else
           (let ((freq     (cdr (assq 'freq r)))
                 (interval (cdr (assq 'interval r)))
                 (byday    (cdr (assq 'byday r)))
                 (count    (cdr (assq 'count r)))
                 (until    (cdr (assq 'until r))))
             (expand-loop start-date freq interval byday count until
                          from-date to-date exdates limit))))))

    (define (nth-occurrence start freq interval n)
      (case freq
        ((daily)   (date+days   start (* interval n)))
        ((weekly)  (date+days   start (* 7 interval n)))
        ((monthly) (date+months start (* interval n)))
        ((yearly)  (date+years  start (* interval n)))
        (else      start)))

    (define (expand-loop start freq interval byday count until from to exdates limit)
      (cond
        ((and (eq? freq 'weekly) (pair? byday))
         ;; Day-by-day from start; emit when weekday matches BYDAY and
         ;; we're inside the right week-block (relative to start).
         (let loop ((cur start) (produced 0) (kept '()))
           (cond
             ((>= produced limit) (reverse kept))
             ((and count (>= produced count)) (reverse kept))
             ((and until (date<? until cur)) (reverse kept))
             ((date<? to cur) (reverse kept))
             (else
              (let* ((days-from-start (- (apply date->jdn cur)
                                          (apply date->jdn start)))
                     (week-block (quotient days-from-start 7))
                     (match? (and (= 0 (modulo week-block interval))
                                  (memv (weekday-of cur) byday)))
                     (emit?  (and match?
                                  (date<=? from cur)
                                  (not (member cur exdates equal?))))
                     (kept2     (if emit? (cons cur kept) kept))
                     (produced2 (if match? (+ produced 1) produced)))
                (loop (date+days cur 1) produced2 kept2))))))
        (else
         (let loop ((n 0) (kept '()))
           (let ((cur (nth-occurrence start freq interval n)))
             (cond
               ((>= n limit) (reverse kept))
               ((and count (>= n count)) (reverse kept))
               ((and until (date<? until cur)) (reverse kept))
               ((date<? to cur) (reverse kept))
               ((and (date<=? from cur)
                     (not (member cur exdates equal?)))
                (loop (+ n 1) (cons cur kept)))
               (else
                (loop (+ n 1) kept))))))))

    ;; ============================================================
    ;; Combine parser output into starts_at / ends_at ISO strings.
    ;; ============================================================
    ;;
    ;; Inputs from parse-quick-add:
    ;;   date (y m d) | #f, time (h m) | #f, end-time (h m) | #f,
    ;;   duration mins | #f, all-day bool
    ;; Defaults: no date => today; date but no time => all-day.
    ;; Returns alist with starts-iso, ends-iso, all-day.

    (define (resolve-times parsed today)
      (let* ((date     (or (assq-ref parsed 'date) today))
             (time     (assq-ref parsed 'time))
             (end-time (assq-ref parsed 'end-time))
             (duration (assq-ref parsed 'duration))
             (all-day  (or (assq-ref parsed 'all-day)
                           (not time))))
        (cond
          (all-day
           (list (cons 'starts-iso (format-datetime date '(0 0)))
                 (cons 'ends-iso #f)
                 (cons 'all-day #t)))
          (else
           (let* ((starts-iso (format-datetime date time))
                  (ends-iso
                    (cond
                      (end-time (format-datetime date end-time))
                      (duration
                       (datetime+minutes-iso date time duration))
                      (else #f))))
             (list (cons 'starts-iso starts-iso)
                   (cons 'ends-iso ends-iso)
                   (cons 'all-day #f)))))))

    (define (assq-ref alist key)
      (let ((p (assq key alist))) (and p (cdr p))))

    (define (datetime+minutes-iso date time mins)
      (let* ((h (car time)) (mi (cadr time))
             (total (+ (* h 60) mi mins))
             (extra-days (quotient total (* 24 60)))
             (rem (modulo total (* 24 60)))
             (new-h (quotient rem 60))
             (new-m (modulo rem 60))
             (new-date (date+days date extra-days)))
        (format-datetime new-date (list new-h new-m))))

    ;; ============================================================
    ;; DB layer
    ;; ============================================================

    (define (split-exdates s)
      (cond
        ((or (not (string? s)) (string=? s "")) '())
        (else
         (filter-map parse-iso-date
                     (filter (lambda (x) (not (string=? x "")))
                             (map string-trim-both (split-on s #\,)))))))

    (define (parse-iso-date s)
      ;; "YYYY-MM-DD" -> (y m d), else #f.
      (and (= 10 (string-length s))
           (let ((y (string->number (substring s 0 4)))
                 (m (string->number (substring s 5 7)))
                 (d (string->number (substring s 8 10))))
             (and y m d (list y m d)))))

    (define (join-exdates dates)
      (string-join (map format-date dates) ","))

    (define (event-select-cols)
      ;; All columns formatted as strings, with starts_at/ends_at
      ;; rendered as "YYYY-MM-DDTHH:MI" for direct use in views and
      ;; <input type=datetime-local>.
      (string-append
        "id::text AS id, title, notes, "
        "to_char(starts_at, 'YYYY-MM-DD\"T\"HH24:MI') AS starts_iso, "
        "to_char(starts_at, 'YYYY-MM-DD')              AS starts_date, "
        "to_char(starts_at, 'HH24:MI')                  AS starts_time, "
        "CASE WHEN ends_at IS NULL THEN '' "
        "     ELSE to_char(ends_at, 'YYYY-MM-DD\"T\"HH24:MI') END AS ends_iso, "
        "CASE WHEN ends_at IS NULL THEN '' "
        "     ELSE to_char(ends_at, 'HH24:MI') END AS ends_time, "
        "CASE WHEN all_day THEN 'yes' ELSE 'no' END AS all_day, "
        "location, category, rrule, exdates"))

    (define (list-events-in-range cfg from-date to-date)
      ;; Returns alist rows. We fetch:
      ;;   - non-recurring events whose [starts_at, COALESCE(ends_at,starts_at)]
      ;;     overlaps [from, to+1day)
      ;;   - all recurring events whose starts_at <= to (expansion
      ;;     happens in Scheme).
      (let ((from-s (format-date from-date))
            (to-s   (format-date to-date)))
        (alist-rows cfg
          (string-append
            "SELECT " (event-select-cols) " FROM calendar_events "
            "WHERE (rrule = '' "
            "       AND starts_at < ($2::date + 1)::timestamptz "
            "       AND COALESCE(ends_at, starts_at) >= $1::timestamptz) "
            "   OR (rrule <> '' AND starts_at < ($2::date + 1)::timestamptz) "
            "ORDER BY starts_at, id")
          (list from-s to-s))))

    (define (find-event cfg id)
      (let ((rs (alist-rows cfg
                  (string-append
                    "SELECT " (event-select-cols)
                    " FROM calendar_events WHERE id = $1")
                  (list id))))
        (and (pair? rs) (car rs))))

    (define (create-event! cfg title notes starts-iso ends-iso all-day
                           location category rrule)
      (with-db cfg
        (lambda (c)
          (let ((res (pg-query c
                       (string-append
                         "INSERT INTO calendar_events "
                         "(title, notes, starts_at, ends_at, all_day, "
                         " location, category, rrule) "
                         "VALUES ($1, $2, $3::timestamptz, "
                         "        NULLIF($4, '')::timestamptz, "
                         "        $5, $6, $7, $8) RETURNING id")
                       title notes starts-iso (or ends-iso "")
                       (if all-day "t" "f")
                       location category rrule)))
            (string->number (vector-ref (car (pg-result-rows res)) 0))))))

    (define (update-event! cfg id title notes starts-iso ends-iso all-day
                           location category rrule)
      (exec cfg
        (string-append
          "UPDATE calendar_events SET "
          "  title = $2, notes = $3, "
          "  starts_at = $4::timestamptz, "
          "  ends_at   = NULLIF($5, '')::timestamptz, "
          "  all_day = $6, location = $7, category = $8, rrule = $9, "
          "  updated_at = now() "
          "WHERE id = $1")
        (list id title notes starts-iso (or ends-iso "")
              (if all-day "t" "f") location category rrule)))

    (define (delete-event! cfg id)
      (exec cfg "DELETE FROM calendar_events WHERE id = $1" (list id)))

    (define (add-exdate! cfg id date)
      (let* ((row (find-event cfg id))
             (cur (and row (split-exdates (row-field row "exdates"))))
             (new (and cur (cons date (filter (lambda (d) (not (equal? d date))) cur)))))
        (when new
          (exec cfg "UPDATE calendar_events SET exdates = $2 WHERE id = $1"
                (list id (join-exdates new))))))

    (define (truncate-recurrence-until! cfg id until-date)
      ;; Set UNTIL on the existing rrule to until-date - 1 day, so the
      ;; series ends before that occurrence.
      (let ((row (find-event cfg id)))
        (when row
          (let* ((rrule (row-field row "rrule"))
                 (new-until (date+days until-date -1))
                 (new-rrule (replace-or-add-until rrule new-until)))
            (exec cfg "UPDATE calendar_events SET rrule = $2 WHERE id = $1"
                  (list id new-rrule))))))

    (define (replace-or-add-until rrule until-date)
      ;; Remove any existing UNTIL=...; append UNTIL=YYYYMMDD.
      (let* ((parts (filter (lambda (p)
                              (not (string-prefix? "UNTIL=" (string-upcase p))))
                            (split-on rrule #\;)))
             (date-str (string-append
                         (number->string (car until-date))
                         (pad2 (cadr until-date))
                         (pad2 (caddr until-date))))
             (clean (filter (lambda (p) (not (string=? p ""))) parts)))
        (string-join (append clean (list (string-append "UNTIL=" date-str)))
                     ";")))

    ;; ---- categories admin ----

    (define (list-categories cfg)
      (alist-rows cfg
        "SELECT id::text AS id, name, colour FROM calendar_categories ORDER BY lower(name)"))

    (define (create-category! cfg name colour)
      (exec cfg
        (string-append
          "INSERT INTO calendar_categories (name, colour) VALUES ($1, $2) "
          "ON CONFLICT (name) DO NOTHING")
        (list name colour)))

    (define (update-category! cfg id name colour)
      (exec cfg
        "UPDATE calendar_categories SET name = $2, colour = $3 WHERE id = $1"
        (list id name colour)))

    (define (delete-category! cfg id)
      (exec cfg "DELETE FROM calendar_categories WHERE id = $1" (list id)))

    (define (category-colour cats name)
      (let* ((lo (string-downcase name))
             (m (find (lambda (c) (string=? (string-downcase (row-field c "name")) lo))
                      cats)))
        (and m (row-field m "colour"))))

    ;; ============================================================
    ;; Building occurrences for a date range
    ;; ============================================================
    ;;
    ;; Returns list of occurrence records (alists) — one per visible
    ;; instance of a series within [from, to]. Each occurrence has:
    ;;   id, title, location, category, colour, all-day (bool),
    ;;   date (y m d), starts-time (string "HH:MM" or ""),
    ;;   ends-time (string), recurring? (bool).

    (define (occurrences-in-range cfg from-date to-date)
      (let* ((rows (list-events-in-range cfg from-date to-date))
             (cats (list-categories cfg)))
        (sort-occurrences
          (append-map (lambda (r) (event->occurrences r cats from-date to-date))
                      rows))))

    (define (event->occurrences row cats from-date to-date)
      (let* ((id        (row-field row "id"))
             (title     (row-field row "title"))
             (location  (row-field row "location"))
             (category  (row-field row "category"))
             (rrule     (row-field row "rrule"))
             (all-day?  (string=? (row-field row "all_day") "yes"))
             (starts-d  (parse-iso-date (row-field row "starts_date")))
             (starts-t  (row-field row "starts_time"))
             (ends-t    (row-field row "ends_time"))
             (exdates   (split-exdates (row-field row "exdates")))
             (recurring? (not (string=? rrule "")))
             (colour    (or (category-colour cats category) "")))
        (cond
          ((not starts-d) '())
          ((not recurring?)
           (cond
             ((and (date<=? from-date starts-d) (date<=? starts-d to-date))
              (list (make-occurrence id title location category colour all-day?
                                     starts-d starts-t ends-t #f)))
             (else '())))
          (else
           (let ((dates (expand-recurrence starts-d rrule
                                           from-date to-date exdates 366)))
             (map (lambda (d)
                    (make-occurrence id title location category colour all-day?
                                     d starts-t ends-t #t))
                  dates))))))

    (define (make-occurrence id title location category colour all-day?
                             date starts-time ends-time recurring?)
      (list (cons 'id id) (cons 'title title)
            (cons 'location location) (cons 'category category)
            (cons 'colour colour) (cons 'all-day all-day?)
            (cons 'date date) (cons 'starts-time starts-time)
            (cons 'ends-time ends-time) (cons 'recurring recurring?)))

    (define (sort-occurrences occs)
      (list-sort
        (lambda (a b)
          (let ((da (cdr (assq 'date a))) (db (cdr (assq 'date b))))
            (cond
              ((date<? da db) #t)
              ((date<? db da) #f)
              (else
               ;; all-day first, then by start time
               (let ((aa? (cdr (assq 'all-day a)))
                     (ba? (cdr (assq 'all-day b))))
                 (cond
                   ((and aa? (not ba?)) #t)
                   ((and ba? (not aa?)) #f)
                   (else
                    (string<? (cdr (assq 'starts-time a))
                              (cdr (assq 'starts-time b))))))))))
        occs))

    (define (list-sort cmp lst)
      ;; Simple insertion sort; lists are short.
      (let loop ((in lst) (out '()))
        (cond
          ((null? in) out)
          (else
           (loop (cdr in) (insert-sorted cmp (car in) out))))))

    (define (insert-sorted cmp x lst)
      (cond
        ((null? lst) (list x))
        ((cmp x (car lst)) (cons x lst))
        (else (cons (car lst) (insert-sorted cmp x (cdr lst))))))

    ;; ============================================================
    ;; Views
    ;; ============================================================

    (define (page req auth title body)
      (html-response
        (render-page req auth
                     (list (cons 'title title)
                           (cons 'active 'calendar)
                           (cons 'body-class "feeds-page calendar-page"))
                     (html->string body))))

    (define (today-date)
      ;; Reads the current date from the postgres clock so it matches
      ;; the server tz used everywhere else. We don't actually need DB
      ;; here at startup, so just use the host system clock via the
      ;; (scheme time) library wouldn't help portably; use a side-effect-
      ;; free approximation: parse from a postgres call inside routes
      ;; instead. This helper is overridden by request-time helpers.
      '(2026 1 1))

    (define (db-today cfg)
      (let ((rs (rows cfg "SELECT to_char(now()::date, 'YYYY-MM-DD')")))
        (or (and (pair? rs) (parse-iso-date (vector-ref (car rs) 0)))
            '(2026 1 1))))

    ;; ---- shared bits ----

    (define (view-switcher current anchor)
      `(nav (@ (class "cal-views"))
            ,(view-link "month"   "Month"  current anchor)
            ,(view-link "week"    "Week"   current anchor)
            ,(view-link "agenda"  "Agenda" current anchor)))

    (define (view-link key label current anchor)
      `(a (@ (href ,(string-append "/calendar?view=" key
                                   "&d=" (format-date anchor)))
             (class ,(if (string=? key current) "active" #f)))
          ,label))

    (define (nav-controls anchor view step-fn)
      ;; step-fn: prev/next deltas in days
      (let ((prev (date+days anchor (- (step-fn))))
            (next (date+days anchor (step-fn)))
            (tdy  '()))
        `(nav (@ (class "cal-nav"))
              (a (@ (href ,(string-append "/calendar?view=" view
                                          "&d=" (format-date prev))))
                 ,(raw "← prev"))
              (a (@ (href ,(string-append "/calendar?view=" view))) "today")
              (a (@ (href ,(string-append "/calendar?view=" view
                                          "&d=" (format-date next))))
                 ,(raw "next →")))))

    ;; ---- quick-add form ----

    (define (quick-add-sxml prefill cats)
      (let ((pf-text (or (and prefill (assq-ref prefill 'text)) "")))
        `(form (@ (method "post") (action "/calendar")
                  (class "feed-new cal-add") (data-cal-add #t))
           (h2 "Quick add")
           (label (@ (class "cal-quick-label")) "Event"
             (input (@ (type "text") (name "text") (required #t)
                       (maxlength "500") (autofocus #t)
                       (placeholder "Dentist tomorrow 14:00 1h #health @Dr. Müller")
                       (value ,pf-text)
                       (data-cal-input #t))))
           (div (@ (class "cal-preview") (data-cal-preview #t)
                   (aria-live "polite")) "")
           (details (@ (class "cal-fallback"))
             (summary "Details")
             (label "Date "
               (input (@ (type "date") (name "date"))))
             (label "Start "
               (input (@ (type "time") (name "time"))))
             (label "End "
               (input (@ (type "time") (name "end_time"))))
             (label "Duration "
               (input (@ (type "text") (name "duration")
                         (placeholder "1h, 90m, 1:30h"))))
             (label (@ (class "cb")) "All day "
               (input (@ (type "checkbox") (name "all_day") (value "1"))))
             (label "Location "
               (input (@ (type "text") (name "location"))))
             (label "Category "
               (input (@ (type "text") (name "category")
                         (list "cal-cats-list"))))
             (datalist (@ (id "cal-cats-list"))
               ,@(map (lambda (c)
                        `(option (@ (value ,(row-field c "name")))))
                      cats))
             (label "Repeats "
               (select (@ (name "rrule"))
                 (option (@ (value ""))         "(no)")
                 (option (@ (value "FREQ=DAILY")) "daily")
                 (option (@ (value "FREQ=WEEKLY")) "weekly")
                 (option (@ (value "FREQ=MONTHLY")) "monthly")
                 (option (@ (value "FREQ=YEARLY")) "yearly"))))
           (button (@ (type "submit")) "Add"))))

    ;; ---- agenda view ----

    (define (group-by-date occs)
      ;; Returns list of (date . list-of-occs).
      (let loop ((in occs) (cur-date #f) (cur-acc '()) (out '()))
        (cond
          ((null? in)
           (reverse (if cur-date (cons (cons cur-date (reverse cur-acc)) out) out)))
          (else
           (let* ((occ (car in))
                  (d   (cdr (assq 'date occ))))
             (cond
               ((and cur-date (equal? d cur-date))
                (loop (cdr in) cur-date (cons occ cur-acc) out))
               (else
                (loop (cdr in) d (list occ)
                      (if cur-date (cons (cons cur-date (reverse cur-acc)) out)
                          out)))))))))

    (define iso-weekday-names
      #("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))

    (define (weekday-label date)
      (vector-ref iso-weekday-names (- (weekday-of date) 1)))

    (define (occurrence-row-sxml occ)
      (let* ((id        (cdr (assq 'id occ)))
             (title     (cdr (assq 'title occ)))
             (location  (cdr (assq 'location occ)))
             (category  (cdr (assq 'category occ)))
             (colour    (cdr (assq 'colour occ)))
             (all-day?  (cdr (assq 'all-day occ)))
             (starts-t  (cdr (assq 'starts-time occ)))
             (ends-t    (cdr (assq 'ends-time occ)))
             (recurring? (cdr (assq 'recurring occ)))
             (time-str (cond
                         (all-day? "all day")
                         ((and (not (string=? ends-t ""))
                               (not (string=? starts-t "")))
                          (string-append starts-t "–" ends-t))
                         (else starts-t))))
        `(li (@ (class "cal-agenda-item"))
           (a (@ (href ,(string-append "/calendar/" id)))
              ,@(if (string=? colour "")
                    '()
                    `((span (@ (class "cal-swatch")
                               (style ,(string-append "background:" colour)))
                            "")))
              (span (@ (class "cal-time")) ,time-str)
              (span (@ (class "cal-title")) ,title
                    ,@(if recurring?
                          '((span (@ (class "cal-rec")) " ↻"))
                          '())
                    ,@(if (string=? location "")
                          '()
                          `((span (@ (class "cal-loc")) " @ " ,location)))
                    ,@(if (string=? category "")
                          '()
                          `((span (@ (class "cal-cat")) " #" ,category))))))))

    (define (agenda-day-sxml date occs)
      `(section (@ (class "cal-agenda-day"))
         (h3 (@ (class "cal-agenda-date"))
             (span (@ (class "cal-wd")) ,(weekday-label date))
             " "
             ,(format-date date))
         (ul (@ (class "cal-agenda"))
             ,@(map occurrence-row-sxml occs))))

    (define agenda-window-days 30)

    (define (render-agenda req auth cfg anchor cats prefill)
      (let* ((from anchor)
             (to   (date+days anchor (- agenda-window-days 1)))
             (occs (occurrences-in-range cfg from to))
             (groups (group-by-date occs))
             (body
               `((header (@ (class "feeds-head"))
                   (h1 "Calendar")
                   (a (@ (class "admin-link") (href "/calendar/categories"))
                      "categories")
                   " "
                   (a (@ (class "admin-link") (href "/calendar.ics"))
                      "export ICS"))
                 ,(view-switcher "agenda" anchor)
                 ,(nav-controls anchor "agenda"
                                (lambda () agenda-window-days))
                 ,(quick-add-sxml prefill cats)
                 ,(if (null? groups)
                      `(p (@ (class "empty")) "Nothing in this window.")
                      `(div (@ (class "cal-agenda-list"))
                            ,@(map (lambda (g)
                                     (agenda-day-sxml (car g) (cdr g)))
                                   groups))))))
        (page req auth "Calendar" body)))

    ;; ---- month view ----

    (define (month-start anchor)
      (list (car anchor) (cadr anchor) 1))

    (define (month-grid-start anchor)
      ;; First grid cell: Monday on/before the 1st of the anchor month.
      (let* ((first (month-start anchor))
             (wd    (weekday-of first)))
        (date+days first (- (- wd 1)))))

    (define (render-month req auth cfg anchor cats prefill)
      (let* ((grid-start (month-grid-start anchor))
             (grid-end   (date+days grid-start (- (* 6 7) 1)))
             (occs       (occurrences-in-range cfg grid-start grid-end))
             (by-day     (group-occs-by-day occs))
             (cells      (build-month-cells grid-start anchor by-day))
             (month-name (string-append (number->string (car anchor))
                                        "-" (pad2 (cadr anchor))))
             (body
               `((header (@ (class "feeds-head"))
                   (h1 "Calendar")
                   (span (@ (class "cal-month-label")) ,month-name)
                   (a (@ (class "admin-link") (href "/calendar/categories"))
                      "categories")
                   " "
                   (a (@ (class "admin-link") (href "/calendar.ics"))
                      "export ICS"))
                 ,(view-switcher "month" anchor)
                 ,(nav-controls anchor "month" (lambda () 30))
                 ,(quick-add-sxml prefill cats)
                 (table (@ (class "cal-month"))
                   (thead (tr ,@(map (lambda (w) `(th ,w))
                                     '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))))
                   (tbody ,@(map (lambda (row)
                                   `(tr ,@(map month-cell-sxml row)))
                                 (chunk-of cells 7)))))))
        (page req auth "Calendar" body)))

    (define (group-occs-by-day occs)
      ;; alist date -> list of occs
      (let loop ((in occs) (acc '()))
        (cond
          ((null? in) acc)
          (else
           (let* ((occ (car in))
                  (d   (cdr (assq 'date occ)))
                  (p   (assoc d acc)))
             (cond
               (p (set-cdr! p (cons occ (cdr p)))
                  (loop (cdr in) acc))
               (else (loop (cdr in) (cons (list d occ) acc)))))))))

    (define (build-month-cells grid-start anchor by-day)
      (let loop ((i 0) (acc '()))
        (cond
          ((= i 42) (reverse acc))
          (else
           (let* ((d (date+days grid-start i))
                  (in-month? (= (cadr d) (cadr anchor)))
                  (entry (assoc d by-day))
                  (occs (if entry (reverse (cdr entry)) '())))
             (loop (+ i 1)
                   (cons (list (cons 'date d)
                               (cons 'in-month in-month?)
                               (cons 'occs occs))
                         acc)))))))

    (define (chunk-of lst n)
      (cond
        ((null? lst) '())
        (else (cons (take-up-to lst n) (chunk-of (drop-up-to lst n) n)))))

    (define (take-up-to lst n)
      (cond ((or (null? lst) (= n 0)) '())
            (else (cons (car lst) (take-up-to (cdr lst) (- n 1))))))
    (define (drop-up-to lst n)
      (cond ((or (null? lst) (= n 0)) lst)
            (else (drop-up-to (cdr lst) (- n 1)))))

    (define max-month-chips 3)

    (define (month-cell-sxml cell)
      (let* ((d         (cdr (assq 'date cell)))
             (in-month? (cdr (assq 'in-month cell)))
             (occs      (cdr (assq 'occs cell)))
             (n         (length occs))
             (visible   (take-up-to occs max-month-chips))
             (more      (- n max-month-chips))
             (today?    #f))
        `(td (@ (class ,(string-append "cal-cell"
                                       (if in-month? "" " out")
                                       (if today? " today" "")))
                (data-date ,(format-date d)))
           (div (@ (class "cal-cell-date")) ,(number->string (caddr d)))
           ,@(map month-chip-sxml visible)
           ,@(if (> more 0)
                 `((div (@ (class "cal-more"))
                        "+" ,(number->string more) " more"))
                 '())
           (a (@ (class "cal-add-here")
                 (href ,(string-append "/calendar?view=agenda&d="
                                       (format-date d)
                                       "&prefill_date=" (format-date d))))
              "+"))))

    (define (month-chip-sxml occ)
      (let ((id     (cdr (assq 'id occ)))
            (title  (cdr (assq 'title occ)))
            (colour (cdr (assq 'colour occ)))
            (all-day? (cdr (assq 'all-day occ)))
            (starts-t (cdr (assq 'starts-time occ))))
        `(a (@ (class "cal-chip")
               (href ,(string-append "/calendar/" id))
               (style ,(if (string=? colour "")
                           #f
                           (string-append "border-left-color:" colour))))
            ,@(if all-day? '() `((span (@ (class "cal-chip-time")) ,starts-t " ")))
            (span (@ (class "cal-chip-title")) ,title))))

    ;; ---- week view ----

    (define (week-grid-start anchor)
      (let ((wd (weekday-of anchor)))
        (date+days anchor (- (- wd 1)))))

    (define (render-week req auth cfg anchor cats prefill)
      (let* ((from (week-grid-start anchor))
             (to   (date+days from 6))
             (occs (occurrences-in-range cfg from to))
             (by-day (group-occs-by-day occs))
             (body
               `((header (@ (class "feeds-head"))
                   (h1 "Calendar")
                   (a (@ (class "admin-link") (href "/calendar/categories"))
                      "categories")
                   " "
                   (a (@ (class "admin-link") (href "/calendar.ics"))
                      "export ICS"))
                 ,(view-switcher "week" anchor)
                 ,(nav-controls anchor "week" (lambda () 7))
                 ,(quick-add-sxml prefill cats)
                 (div (@ (class "cal-week"))
                   ,@(map (lambda (i)
                            (let* ((d (date+days from i))
                                   (entry (assoc d by-day))
                                   (occs (if entry (reverse (cdr entry)) '())))
                              `(section (@ (class "cal-week-day"))
                                 (h3 (span (@ (class "cal-wd"))
                                           ,(weekday-label d))
                                     " " ,(format-date d))
                                 ,(if (null? occs)
                                      `(p (@ (class "empty")) "—")
                                      `(ul (@ (class "cal-agenda"))
                                           ,@(map occurrence-row-sxml occs))))))
                          (iota 7))))))
        (page req auth "Calendar" body)))

    ;; ---- detail / edit ----

    (define (render-detail req auth cfg id cats)
      (let ((row (find-event cfg id)))
        (cond
          ((not row) (render-error 404 "Event not found."))
          (else
           (let* ((id       (number->string id))
                  (title    (row-field row "title"))
                  (notes    (row-field row "notes"))
                  (starts-iso (row-field row "starts_iso"))
                  (ends-iso   (row-field row "ends_iso"))
                  (all-day?   (string=? (row-field row "all_day") "yes"))
                  (location   (row-field row "location"))
                  (category   (row-field row "category"))
                  (rrule      (row-field row "rrule"))
                  (recurring? (not (string=? rrule "")))
                  (body
                    `((header (@ (class "feeds-head"))
                        (h1 ,title)
                        " "
                        (a (@ (class "admin-link") (href "/calendar"))
                           ,(raw "← back")))
                      (section (@ (class "cal-detail-meta"))
                        (p "When: " ,(if all-day?
                                         (string-append "all day, "
                                                        (substring starts-iso 0 10))
                                         (if (string=? ends-iso "")
                                             starts-iso
                                             (string-append starts-iso " → " ends-iso))))
                        ,@(if (string=? location "")
                              '() `((p "Where: " ,location)))
                        ,@(if (string=? category "")
                              '() `((p "Category: " ,category)))
                        ,@(if recurring?
                              `((p "Repeats: " ,rrule))
                              '())
                        ,@(if (string=? notes "")
                              '() `((p (@ (class "cal-notes")) ,notes))))
                      (h2 "Edit")
                      (form (@ (method "post")
                               (action ,(string-append "/calendar/" id "/edit"))
                               (class "feed-new cal-edit"))
                        (label "Title "
                          (input (@ (type "text") (name "title") (required #t)
                                    (maxlength "500") (value ,title))))
                        (label "Notes "
                          (textarea (@ (name "notes") (rows "3")) ,notes))
                        (label "Starts "
                          (input (@ (type "datetime-local") (name "starts_at")
                                    (required #t) (value ,starts-iso))))
                        (label "Ends "
                          (input (@ (type "datetime-local") (name "ends_at")
                                    (value ,ends-iso))))
                        (label (@ (class "cb")) "All day "
                          (input (@ (type "checkbox") (name "all_day")
                                    (value "1")
                                    (checked ,(and all-day? #t)))))
                        (label "Location "
                          (input (@ (type "text") (name "location")
                                    (value ,location))))
                        (label "Category "
                          (input (@ (type "text") (name "category")
                                    (list "cal-cats-list")
                                    (value ,category))))
                        (datalist (@ (id "cal-cats-list"))
                          ,@(map (lambda (c)
                                   `(option (@ (value ,(row-field c "name")))))
                                 cats))
                        (label "Repeats (RRULE) "
                          (input (@ (type "text") (name "rrule")
                                    (placeholder "FREQ=WEEKLY;BYDAY=MO")
                                    (value ,rrule))))
                        (button (@ (type "submit")) "Save"))
                      (h2 "Delete")
                      ,@(if recurring?
                            `((p "This is a recurring event. Choose scope:")
                              (form (@ (method "post")
                                       (action ,(string-append "/calendar/" id "/delete"))
                                       (class "inline")
                                       (data-confirm "Delete this entire series?"))
                                (input (@ (type "hidden") (name "scope") (value "all")))
                                (button (@ (class "linkish danger")) "all")))
                            `((form (@ (method "post")
                                       (action ,(string-append "/calendar/" id "/delete"))
                                       (class "inline")
                                       (data-confirm "Delete this event?"))
                                (button (@ (class "linkish danger")) "delete"))))
                      ;; Per-occurrence skip is only meaningful for series.
                      ,@(if recurring?
                            `((h2 "Skip a single occurrence")
                              (form (@ (method "post")
                                       (action ,(string-append "/calendar/" id "/skip"))
                                       (class "inline"))
                                (input (@ (type "date") (name "date") (required #t)))
                                (button (@ (type "submit")) "Skip"))
                              (h2 "End series before")
                              (form (@ (method "post")
                                       (action ,(string-append "/calendar/" id "/until"))
                                       (class "inline"))
                                (input (@ (type "date") (name "date") (required #t)))
                                (button (@ (type "submit")) "End before")))
                            '()))))
             (page req auth title body))))))

    ;; ---- categories admin ----

    (define (render-categories req auth cfg)
      (let* ((cats (list-categories cfg))
             (body
               `((header (@ (class "feeds-head"))
                   (h1 "Calendar categories")
                   (a (@ (href "/calendar")) ,(raw "← back")))
                 (form (@ (method "post") (action "/calendar/categories")
                          (class "skip-new inline"))
                   (input (@ (type "text") (name "name") (required #t)
                             (placeholder "new category")))
                   (input (@ (type "color") (name "colour") (value "#888888")))
                   (button (@ (type "submit")) "Add"))
                 ,(if (null? cats)
                      `(p (@ (class "empty")) "No categories yet.")
                      `(table (@ (class "feed-table"))
                         (thead (tr (th "name") (th "colour") (th)))
                         (tbody
                           ,@(map
                              (lambda (c)
                                (let ((id (row-field c "id"))
                                      (name (row-field c "name"))
                                      (colour (row-field c "colour")))
                                  `(tr
                                     (td (form (@ (method "post")
                                                  (action ,(string-append
                                                             "/calendar/categories/" id "/edit"))
                                                  (class "inline"))
                                           (input (@ (type "text") (name "name")
                                                     (value ,name)))
                                           (input (@ (type "color") (name "colour")
                                                     (value ,colour)))
                                           (button "Save")))
                                     (td (span (@ (class "cal-swatch")
                                                  (style ,(string-append
                                                            "background:" colour)))
                                               "")
                                         " " ,colour)
                                     (td (form (@ (method "post")
                                                  (action ,(string-append
                                                             "/calendar/categories/" id "/delete"))
                                                  (class "inline")
                                                  (data-confirm "Delete this category?"))
                                           (button (@ (class "linkish danger"))
                                             "delete"))))))
                              cats)))))))
        (page req auth "Calendar categories" body)))

    ;; ============================================================
    ;; ICS export
    ;; ============================================================

    (define (ics-escape s)
      (let* ((out (open-output-string))
             (n (string-length s)))
        (let loop ((i 0))
          (cond
            ((= i n) (get-output-string out))
            (else
             (let ((c (string-ref s i)))
               (cond
                 ((char=? c #\\) (write-string "\\\\" out))
                 ((char=? c #\,) (write-string "\\," out))
                 ((char=? c #\;) (write-string "\\;" out))
                 ((char=? c #\newline) (write-string "\\n" out))
                 (else (write-char c out)))
               (loop (+ i 1))))))))

    (define (ics-format-dt iso all-day?)
      ;; iso is "YYYY-MM-DDTHH:MM". RFC 5545 floating local times use
      ;; "YYYYMMDDTHHMMSS" with no Z.
      (cond
        (all-day?
         (string-append (substring iso 0 4)
                        (substring iso 5 7)
                        (substring iso 8 10)))
        (else
         (string-append (substring iso 0 4)
                        (substring iso 5 7)
                        (substring iso 8 10)
                        "T"
                        (substring iso 11 13)
                        (substring iso 14 16)
                        "00"))))

    (define (event->ics row)
      (let* ((id    (row-field row "id"))
             (title (row-field row "title"))
             (notes (row-field row "notes"))
             (loc   (row-field row "location"))
             (rrule (row-field row "rrule"))
             (all-day? (string=? (row-field row "all_day") "yes"))
             (starts (ics-format-dt (row-field row "starts_iso") all-day?))
             (ends-iso (row-field row "ends_iso"))
             (out (open-output-string)))
        (write-string "BEGIN:VEVENT\r\n" out)
        (write-string (string-append "UID:" id "@dabsite\r\n") out)
        (write-string (string-append "SUMMARY:" (ics-escape title) "\r\n") out)
        (cond
          (all-day?
           (write-string (string-append "DTSTART;VALUE=DATE:" starts "\r\n") out))
          (else
           (write-string (string-append "DTSTART:" starts "\r\n") out)))
        (unless (string=? ends-iso "")
          (let ((e (ics-format-dt ends-iso all-day?)))
            (cond
              (all-day?
               (write-string (string-append "DTEND;VALUE=DATE:" e "\r\n") out))
              (else
               (write-string (string-append "DTEND:" e "\r\n") out)))))
        (unless (string=? loc "")
          (write-string (string-append "LOCATION:" (ics-escape loc) "\r\n") out))
        (unless (string=? notes "")
          (write-string (string-append "DESCRIPTION:" (ics-escape notes) "\r\n") out))
        (unless (string=? rrule "")
          (write-string (string-append "RRULE:" rrule "\r\n") out))
        (write-string "END:VEVENT\r\n" out)
        (get-output-string out)))

    (define (render-ics cfg)
      (let* ((rows (alist-rows cfg
                    (string-append
                      "SELECT " (event-select-cols)
                      " FROM calendar_events ORDER BY starts_at, id")))
             (out (open-output-string)))
        (write-string "BEGIN:VCALENDAR\r\nVERSION:2.0\r\nPRODID:-//dabsite//calendar//EN\r\n"
                      out)
        (for-each (lambda (r) (write-string (event->ics r) out)) rows)
        (write-string "END:VCALENDAR\r\n" out)
        (make-http-response 200
          (list (cons "Content-Type" "text/calendar; charset=utf-8")
                (cons "Content-Disposition" "attachment; filename=\"calendar.ics\""))
          (get-output-string out))))

    ;; ============================================================
    ;; Routes
    ;; ============================================================

    (define (redirect to)
      (make-http-response 302 (list (cons "Location" to)) ""))

    (define (param-or req name default)
      (let ((p (assoc name (url-query-params (http-request-url req)))))
        (cond
          ((and p (string? (cdr p))) (percent-decode (cdr p)))
          (else default))))

    (define (form-or form name default)
      (let ((v (form-ref form name "")))
        (if (string=? v "") default v)))

    (define (anchor-of-request req cfg)
      (let ((d (param-or req "d" "")))
        (or (parse-iso-date d)
            (db-today cfg))))

    (define (install-calendar-routes! router cfg auth)

      ;; -- GET /calendar (with ?view=, ?d=) --
      (router-add! router "GET" "/calendar"
        (require-auth auth
          (lambda (req params)
            (let* ((anchor  (anchor-of-request req cfg))
                   (view    (param-or req "view" "agenda"))
                   (cats    (list-categories cfg))
                   (prefill (let ((d (param-or req "prefill_date" "")))
                              (and (non-empty-string? d)
                                   (list (cons 'text d))))))
              (cond
                ((string=? view "month")  (render-month  req auth cfg anchor cats prefill))
                ((string=? view "week")   (render-week   req auth cfg anchor cats prefill))
                (else                     (render-agenda req auth cfg anchor cats prefill)))))))

      ;; -- POST /calendar (quick add) --
      (router-add! router "POST" "/calendar"
        (require-auth auth
          (lambda (req params)
            (let* ((form  (parse-www-form (or (http-request-body req) "")))
                   (text  (string-trim-both (form-ref form "text" "")))
                   (today (db-today cfg)))
              (cond
                ((string=? text "")
                 (render-error 400 "Please describe the event."))
                (else
                 (let* ((parsed (parse-quick-add text today))
                        ;; Form fallback fields override parsed values
                        ;; if the user filled the explicit inputs.
                        (parsed (apply-form-overrides parsed form today))
                        (resolved (resolve-times parsed today))
                        (title    (assq-ref parsed 'title))
                        (location (or (assq-ref parsed 'location) ""))
                        (category (or (assq-ref parsed 'category) ""))
                        (rrule    (or (assq-ref parsed 'rrule) ""))
                        (rrule    (or (form-or form "rrule" #f) rrule))
                        (starts   (assq-ref resolved 'starts-iso))
                        (ends     (assq-ref resolved 'ends-iso))
                        (all-day  (assq-ref resolved 'all-day)))
                   (cond
                     ((or (not title) (string=? title ""))
                      (render-error 400 "Title is required."))
                     ((not starts)
                      (render-error 400 "Could not work out a start time."))
                     (else
                      (create-event! cfg title "" starts ends all-day
                                     location category rrule)
                      (redirect "/calendar"))))))))))

      ;; -- GET /calendar/:id --
      (router-add! router "GET" "/calendar/:id"
        (require-auth auth
          (lambda (req params)
            (let ((id (string->number (params-ref params "id"))))
              (cond
                ((not id) (render-error 404 "Event not found."))
                (else (render-detail req auth cfg id (list-categories cfg))))))))

      ;; -- POST /calendar/:id/edit --
      (router-add! router "POST" "/calendar/:id/edit"
        (require-auth auth
          (lambda (req params)
            (let* ((id   (string->number (params-ref params "id")))
                   (form (parse-www-form (or (http-request-body req) "")))
                   (title (string-trim-both (form-ref form "title" "")))
                   (notes (form-ref form "notes" ""))
                   (starts (form-ref form "starts_at" ""))
                   (ends   (form-ref form "ends_at" ""))
                   (all-day (string=? (form-ref form "all_day" "") "1"))
                   (location (form-ref form "location" ""))
                   (category (form-ref form "category" ""))
                   (rrule    (form-ref form "rrule" "")))
              (cond
                ((or (not id) (string=? title "") (string=? starts ""))
                 (render-error 400 "Title and start time are required."))
                (else
                 (update-event! cfg id title notes starts
                                (if (string=? ends "") #f ends)
                                all-day location category rrule)
                 (redirect (string-append "/calendar/" (number->string id)))))))))

      ;; -- POST /calendar/:id/delete --
      (router-add! router "POST" "/calendar/:id/delete"
        (require-auth auth
          (lambda (req params)
            (let ((id (string->number (params-ref params "id"))))
              (when id (delete-event! cfg id))
              (redirect "/calendar")))))

      ;; -- POST /calendar/:id/skip (single occurrence) --
      (router-add! router "POST" "/calendar/:id/skip"
        (require-auth auth
          (lambda (req params)
            (let* ((id (string->number (params-ref params "id")))
                   (form (parse-www-form (or (http-request-body req) "")))
                   (d  (parse-iso-date (form-ref form "date" ""))))
              (when (and id d) (add-exdate! cfg id d))
              (redirect (string-append "/calendar/" (number->string id)))))))

      ;; -- POST /calendar/:id/until (end series before date) --
      (router-add! router "POST" "/calendar/:id/until"
        (require-auth auth
          (lambda (req params)
            (let* ((id (string->number (params-ref params "id")))
                   (form (parse-www-form (or (http-request-body req) "")))
                   (d  (parse-iso-date (form-ref form "date" ""))))
              (when (and id d) (truncate-recurrence-until! cfg id d))
              (redirect (string-append "/calendar/" (number->string id)))))))

      ;; -- GET /calendar.ics --
      (router-add! router "GET" "/calendar.ics"
        (require-auth auth
          (lambda (req params) (render-ics cfg))))

      ;; -- Categories admin --
      (router-add! router "GET" "/calendar/categories"
        (require-auth auth
          (lambda (req params) (render-categories req auth cfg))))

      (router-add! router "POST" "/calendar/categories"
        (require-auth auth
          (lambda (req params)
            (let* ((form (parse-www-form (or (http-request-body req) "")))
                   (name (string-trim-both (form-ref form "name" "")))
                   (colour (form-or form "colour" "#888888")))
              (unless (string=? name "") (create-category! cfg name colour))
              (redirect "/calendar/categories")))))

      (router-add! router "POST" "/calendar/categories/:id/edit"
        (require-auth auth
          (lambda (req params)
            (let* ((id (string->number (params-ref params "id")))
                   (form (parse-www-form (or (http-request-body req) "")))
                   (name (string-trim-both (form-ref form "name" "")))
                   (colour (form-or form "colour" "#888888")))
              (when (and id (not (string=? name "")))
                (update-category! cfg id name colour))
              (redirect "/calendar/categories")))))

      (router-add! router "POST" "/calendar/categories/:id/delete"
        (require-auth auth
          (lambda (req params)
            (let ((id (string->number (params-ref params "id"))))
              (when id (delete-category! cfg id))
              (redirect "/calendar/categories"))))))

    (define (apply-form-overrides parsed form today)
      ;; If the user filled in explicit fields in the fallback details
      ;; pane, they override the parsed text values.
      (let* ((d  (form-ref form "date" ""))
             (t  (form-ref form "time" ""))
             (et (form-ref form "end_time" ""))
             (du (form-ref form "duration" ""))
             (ad (form-ref form "all_day" ""))
             (lo (form-ref form "location" ""))
             (ca (form-ref form "category" ""))
             (set (lambda (acc k v)
                    (cond
                      ((not v) acc)
                      ((assq k acc)
                       (map (lambda (p)
                              (if (eq? (car p) k) (cons k v) p))
                            acc))
                      (else (cons (cons k v) acc))))))
        (let* ((p parsed)
               (p (if (string=? d "") p (set p 'date (parse-iso-date d))))
               (p (if (string=? t "") p (set p 'time (parse-clock t))))
               (p (if (string=? et "") p (set p 'end-time (parse-clock et))))
               (p (if (string=? du "") p
                      (let ((mins (parse-hours-head
                                    (if (or (string-suffix? "h" du)
                                            (string-suffix? "m" du))
                                        (substring du 0 (- (string-length du) 1))
                                        du))))
                        (if mins (set p 'duration mins) p))))
               (p (if (string=? ad "") p (set p 'all-day #t)))
               (p (if (string=? lo "") p (set p 'location lo)))
               (p (if (string=? ca "") p (set p 'category ca))))
          p)))

))
