;; Unit tests for (dabsite calendar) — pure helpers only.

(import (scheme base) (scheme write) (scheme process-context) (scm module))
(module-search-path! (cons "src" (module-search-path)))

(import (dabsite calendar) (scm test) (srfi 64) (srfi 1))

(test-runner-factory scm-test-runner)
(test-begin "calendar")

;; A fixed "today": Sunday 2026-05-17 (ISO weekday 7).
(define today '(2026 5 17))

;; ---- date math ----

(test-group "date->jdn / jdn->date"
  (test-equal '(2026 5 17) (jdn->date (date->jdn 2026 5 17)))
  (test-equal '(2000 1 1)  (jdn->date (date->jdn 2000 1 1)))
  (test-equal '(1999 12 31) (jdn->date (date->jdn 1999 12 31))))

(test-group "date+days"
  (test-equal '(2026 5 18) (date+days '(2026 5 17) 1))
  (test-equal '(2026 6 1)  (date+days '(2026 5 31) 1))
  (test-equal '(2026 1 1)  (date+days '(2025 12 31) 1))
  (test-equal '(2026 5 10) (date+days '(2026 5 17) -7)))

(test-group "weekday-of"
  (test-eqv 7 (weekday-of '(2026 5 17)))   ; Sunday
  (test-eqv 1 (weekday-of '(2026 5 18)))   ; Monday
  (test-eqv 4 (weekday-of '(2026 1 1))))   ; Thursday

(test-group "last-day-of-month"
  (test-eqv 31 (last-day-of-month 2026 1))
  (test-eqv 28 (last-day-of-month 2026 2))
  (test-eqv 29 (last-day-of-month 2024 2))
  (test-eqv 30 (last-day-of-month 2026 4)))

(test-group "format-date / format-time / format-datetime"
  (test-equal "2026-05-17" (format-date '(2026 5 17)))
  (test-equal "09:05"      (format-time '(9 5)))
  (test-equal "2026-05-17T09:05"
              (format-datetime '(2026 5 17) '(9 5))))

;; ---- quick-add parser ----

(define (field r k) (let ((p (assq k r))) (and p (cdr p))))

(test-group "parse-quick-add: title only"
  (let ((r (parse-quick-add "Buy milk" today)))
    (test-equal "Buy milk" (field r 'title))
    (test-eqv   #f (field r 'date))
    (test-eqv   #f (field r 'time))
    (test-eqv   #f (field r 'all-day))))

(test-group "parse-quick-add: today/tomorrow + time"
  (let ((r (parse-quick-add "Dentist tomorrow 14:00" today)))
    (test-equal "Dentist"   (field r 'title))
    (test-equal '(2026 5 18) (field r 'date))
    (test-equal '(14 0)      (field r 'time)))
  (let ((r (parse-quick-add "Tea today 09:30" today)))
    (test-equal '(2026 5 17) (field r 'date))
    (test-equal '(9 30)      (field r 'time))))

(test-group "parse-quick-add: time range"
  (let ((r (parse-quick-add "Standup 09:00-09:30" today)))
    (test-equal '(9 0)  (field r 'time))
    (test-equal '(9 30) (field r 'end-time))
    (test-equal "Standup" (field r 'title))))

(test-group "parse-quick-add: ISO date"
  (let ((r (parse-quick-add "Trip 2026-06-16" today)))
    (test-equal '(2026 6 16) (field r 'date))
    (test-equal "Trip" (field r 'title))))

(test-group "parse-quick-add: dotted German date"
  (let ((r (parse-quick-add "Termin 16.6. 10:00" today)))
    (test-equal '(2026 6 16) (field r 'date))
    (test-equal '(10 0)      (field r 'time))
    (test-equal "Termin"     (field r 'title)))
  (let ((r (parse-quick-add "X 16.6.2027" today)))
    (test-equal '(2027 6 16) (field r 'date))))

(test-group "parse-quick-add: weekday upcoming"
  (let ((r (parse-quick-add "Yoga mon 18:00" today)))
    ;; today is Sun 17; "mon" => 18.
    (test-equal '(2026 5 18) (field r 'date))
    (test-equal '(18 0)      (field r 'time)))
  (let ((r (parse-quick-add "Lunch next mon" today)))
    ;; "next mon" is strict after today => May 18 (Mon, 1 day away).
    (test-equal '(2026 5 18) (field r 'date))
    (test-equal "Lunch" (field r 'title))))

(test-group "parse-quick-add: duration"
  (let ((r (parse-quick-add "Run 17:00 1h" today)))
    (test-equal '(17 0) (field r 'time))
    (test-eqv   60     (field r 'duration)))
  (let ((r (parse-quick-add "Meeting 14:00 for 90m" today)))
    (test-eqv 90 (field r 'duration)))
  (let ((r (parse-quick-add "Workshop 09:00 1:30h" today)))
    (test-eqv 90 (field r 'duration))))

(test-group "parse-quick-add: all-day"
  (let ((r (parse-quick-add "Holiday tomorrow allday" today)))
    (test-eqv #t (field r 'all-day))
    (test-equal '(2026 5 18) (field r 'date))))

(test-group "parse-quick-add: recurrence words"
  (let ((r (parse-quick-add "Standup weekly 09:00" today)))
    (test-equal "FREQ=WEEKLY" (field r 'rrule)))
  (let ((r (parse-quick-add "Birthday yearly 1980-06-16" today)))
    (test-equal "FREQ=YEARLY" (field r 'rrule))
    (test-equal '(1980 6 16) (field r 'date))))

(test-group "parse-quick-add: every <weekday>"
  (let ((r (parse-quick-add "Standup every mon 09:00" today)))
    (test-equal "FREQ=WEEKLY;BYDAY=MO" (field r 'rrule))
    (test-equal '(2026 5 18) (field r 'date))
    (test-equal '(9 0)       (field r 'time))))

(test-group "parse-quick-add: every <N> <unit>"
  (let ((r (parse-quick-add "Pay rent every 1 month" today)))
    (test-equal "FREQ=MONTHLY;INTERVAL=1" (field r 'rrule)))
  (let ((r (parse-quick-add "Standup every 2 weeks" today)))
    (test-equal "FREQ=WEEKLY;INTERVAL=2" (field r 'rrule))))

(test-group "parse-quick-add: in N days/weeks"
  (let ((r (parse-quick-add "Reminder in 3 days" today)))
    (test-equal '(2026 5 20) (field r 'date)))
  (let ((r (parse-quick-add "Followup in 2 weeks" today)))
    (test-equal '(2026 5 31) (field r 'date))))

(test-group "parse-quick-add: location and category"
  (let ((r (parse-quick-add "Coffee 14:00 #social @Café Schober" today)))
    (test-equal "Coffee"       (field r 'title))
    (test-equal "social"       (field r 'category))
    (test-equal "Café Schober" (field r 'location))
    (test-equal '(14 0)        (field r 'time))))

(test-group "parse-quick-add: tonight"
  (let ((r (parse-quick-add "Movie tonight" today)))
    (test-equal '(2026 5 17) (field r 'date))
    (test-equal '(20 0)      (field r 'time))))

(test-group "parse-quick-add: German keywords"
  (let ((r (parse-quick-add "Zahnarzt morgen 14:00" today)))
    (test-equal "Zahnarzt"    (field r 'title))
    (test-equal '(2026 5 18)  (field r 'date))
    (test-equal '(14 0)       (field r 'time)))
  (let ((r (parse-quick-add "Stand-up täglich 09:00" today)))
    (test-equal "FREQ=DAILY" (field r 'rrule)))
  (let ((r (parse-quick-add "Team jeden Mi 10:00" today)))
    (test-equal "FREQ=WEEKLY;BYDAY=WE" (field r 'rrule))))

;; ---- rrule parsing ----

(test-group "parse-rrule basics"
  (let ((r (parse-rrule "FREQ=DAILY")))
    (test-equal 'daily (cdr (assq 'freq r)))
    (test-eqv   1      (cdr (assq 'interval r))))
  (let ((r (parse-rrule "FREQ=WEEKLY;INTERVAL=2;BYDAY=MO,WE")))
    (test-equal 'weekly (cdr (assq 'freq r)))
    (test-eqv   2       (cdr (assq 'interval r)))
    (test-equal '(1 3)  (cdr (assq 'byday r))))
  (let ((r (parse-rrule "FREQ=YEARLY;UNTIL=20301231")))
    (test-equal 'yearly (cdr (assq 'freq r)))
    (test-equal '(2030 12 31) (cdr (assq 'until r))))
  (test-eqv #f (parse-rrule ""))
  (test-eqv #f (parse-rrule "garbage")))

;; ---- recurrence expansion ----

(test-group "expand-recurrence: single (no rrule)"
  (test-equal '((2026 5 17))
              (expand-recurrence '(2026 5 17) ""
                                 '(2026 5 1) '(2026 5 31) '() 50))
  (test-equal '()
              (expand-recurrence '(2026 5 17) ""
                                 '(2026 6 1) '(2026 6 30) '() 50)))

(test-group "expand-recurrence: daily"
  (let ((r (expand-recurrence '(2026 5 17) "FREQ=DAILY"
                              '(2026 5 17) '(2026 5 20) '() 50)))
    (test-equal 4 (length r))
    (test-equal '(2026 5 17) (car r))
    (test-equal '(2026 5 20) (list-ref r 3))))

(test-group "expand-recurrence: weekly with BYDAY"
  (let ((r (expand-recurrence '(2026 5 18)
                              "FREQ=WEEKLY;BYDAY=MO,WE"
                              '(2026 5 18) '(2026 5 31) '() 50)))
    ;; Mondays/Wednesdays in [May 18, May 31]:
    ;; 18(Mon) 20(Wed) 25(Mon) 27(Wed) — 4 occurrences.
    (test-equal 4 (length r))))

(test-group "expand-recurrence: monthly with month-end clamp"
  (let ((r (expand-recurrence '(2026 1 31) "FREQ=MONTHLY"
                              '(2026 1 1) '(2026 4 30) '() 50)))
    (test-equal '((2026 1 31) (2026 2 28) (2026 3 31) (2026 4 30))
                r)))

(test-group "expand-recurrence: COUNT cap"
  (let ((r (expand-recurrence '(2026 5 17) "FREQ=DAILY;COUNT=3"
                              '(2026 5 17) '(2026 12 31) '() 50)))
    (test-equal 3 (length r))))

(test-group "expand-recurrence: UNTIL cap"
  (let ((r (expand-recurrence '(2026 5 17) "FREQ=DAILY;UNTIL=20260519"
                              '(2026 5 17) '(2026 12 31) '() 50)))
    (test-equal 3 (length r))))

(test-group "expand-recurrence: exdates skipped"
  (let ((r (expand-recurrence '(2026 5 17) "FREQ=DAILY"
                              '(2026 5 17) '(2026 5 20)
                              '((2026 5 18)) 50)))
    (test-equal '((2026 5 17) (2026 5 19) (2026 5 20)) r)))

(test-end "calendar")

(exit (if (= 0 (last-run-failed-tests)) 0 1))
