;; Tiny multi-threaded load tester.
;;
;; Usage: scm bin/load-test.scm [URL [requests-per-thread [num-threads]]]
;;   defaults:           http://127.0.0.1:8080/  20  4
;;
;; Each thread issues N GETs back-to-back with a random short pause
;; between them. We record (status . duration-microseconds) for every
;; request and, when all threads have joined, print a small report:
;;
;;   * status-code breakdown
;;   * wall-clock time and throughput
;;   * latency: min / p50 / p90 / p99 / max / mean
;;
;; This is a sanity check, not a benchmark — it exercises real network
;; and DB code from the same machine, so absolute numbers are noisy.

(import (scheme base)
        (scheme write)
        (scheme process-context)
        (scheme time)
        (srfi 1)
        (srfi 13)
        (srfi 18)
        (scm crypto)
        (scm net http client)
        (scm net http response))

;; ----- argument parsing -----

(define (arg-or args i default)
  (cond ((>= i (length args)) default)
        (else (list-ref args i))))

(define (parse-int s default)
  (let ((n (string->number s)))
    (cond ((and n (integer? n) (> n 0)) (exact n))
          (else default))))

(define argv (cdr (command-line)))
(define url           (arg-or argv 0 "http://127.0.0.1:8080/"))
(define reqs-per-thr  (parse-int (arg-or argv 1 "20") 20))
(define n-threads     (parse-int (arg-or argv 2 "4")  4))
(define total         (* reqs-per-thr n-threads))

;; ----- timing helpers -----

(define (now-us) (current-jiffy))
(define (elapsed-us t0) (- (now-us) t0))

;; ----- per-thread result lists -----
;;
;; Each thread returns its own list of (status . duration-us) entries
;; via thread-join!'s return value. We deliberately avoid sharing a
;; mutable accumulator across threads: scm's set! on top-level defines
;; is not visible across threads, so per-thread return values are the
;; only portable way to collect work done by workers.

;; ----- random short pause -----
;;
;; 30..170 ms — enough jitter to overlap requests across threads without
;; turning into a DoS.

(define (random-pause-seconds)
  (let ((b (bytevector-u8-ref (random-bytes 1) 0)))
    ;; b ∈ 0..255; (30 + b * 140 / 255) ms → 30..170 ms; convert to s
    (/ (+ 30 (quotient (* b 140) 255)) 1000.0)))

;; ----- worker -----

(define (do-one)
  ;; Returns (status . duration-us). #f status → request threw.
  (let ((t0 (now-us)))
    (guard (exn (#t (cons #f (elapsed-us t0))))
      (let ((resp (http-get url '())))
        (cons (http-response-status resp) (elapsed-us t0))))))

(define (worker tid)
  (lambda ()
    (let loop ((i 0) (acc '()))
      (cond
        ((= i reqs-per-thr) (reverse acc))
        (else
         (let ((entry (do-one)))
           (thread-sleep! (random-pause-seconds))
           (loop (+ i 1) (cons entry acc))))))))

;; ----- statistics -----

(define (status-breakdown rs)
  (let ((tab '()))
    (for-each
     (lambda (r)
       (let* ((s (car r))
              (p (assv s tab)))
         (cond
           (p (set-cdr! p (+ 1 (cdr p))))
           (else (set! tab (cons (cons s 1) tab))))))
     rs)
    ;; Sort by status code; #f (error) goes last.
    (list-sort
      (lambda (a b)
        (cond ((not (car a)) #f)
              ((not (car b)) #t)
              (else (< (car a) (car b)))))
      tab)))

(define (list-sort lt xs)
  ;; Plain merge sort — keeps this script free of SRFI 132 churn.
  (cond
    ((null? xs) '())
    ((null? (cdr xs)) xs)
    (else
     (let* ((half (quotient (length xs) 2))
            (a    (take xs half))
            (b    (drop xs half)))
       (merge lt (list-sort lt a) (list-sort lt b))))))

(define (merge lt a b)
  (cond
    ((null? a) b)
    ((null? b) a)
    ((lt (car a) (car b)) (cons (car a) (merge lt (cdr a) b)))
    (else                 (cons (car b) (merge lt a (cdr b))))))

(define (percentile sorted-vec p)
  ;; sorted-vec is a vector of integers (durations). p is 0..100.
  (let* ((n (vector-length sorted-vec))
         (idx (min (- n 1)
                   (max 0 (exact (round (* (- n 1) (/ p 100.0))))))))
    (vector-ref sorted-vec idx)))

(define (us->ms us) (/ us 1000.0))

(define (display-fixed n digits)
  ;; n is a real, digits is the count of decimal places. Cheap rounding
  ;; without depending on (scheme inexact)'s number->string formatting.
  (let* ((mult (expt 10 digits))
         (r    (round (* n mult)))
         (i    (exact r))
         (whole (quotient i mult))
         (frac  (modulo  i mult))
         (frac-str (number->string frac))
         (pad   (- digits (string-length frac-str))))
    (string-append (number->string whole)
                   "."
                   (make-string (max 0 pad) #\0)
                   frac-str)))

;; ----- run -----

(display "load-test: ") (display total) (display " requests, ")
(display n-threads) (display " threads → ") (display url) (newline)

(define wall-start (now-us))

(define threads
  (map (lambda (tid)
         (let ((t (make-thread (worker tid) (number->string tid))))
           (thread-start! t)
           t))
       (iota n-threads)))

;; thread-join! returns the worker's value — collect every thread's
;; result list and concatenate.
(define rs (apply append (map thread-join! threads)))

(define wall-us (elapsed-us wall-start))

;; ----- report -----
(define ok-rs (filter (lambda (r) (and (car r) (= (car r) 200))) rs))
(define durations
  (let ((v (list->vector (map cdr rs))))
    ;; Insert into a vector then sort. For convenience we sort the list
    ;; and rebuild the vector.
    (list->vector (list-sort < (vector->list v)))))

(newline)
(display "=== status codes ===") (newline)
(for-each
 (lambda (p)
   (display "  ")
   (display (cond ((not (car p)) "ERROR") (else (car p))))
   (display ": ") (display (cdr p)) (newline))
 (status-breakdown rs))

(newline)
(display "=== wall clock ===") (newline)
(display "  total requests: ") (display (length rs)) (newline)
(display "  wall time:      ") (display (display-fixed (/ (us->ms wall-us) 1000) 3))
(display " s") (newline)
(display "  throughput:     ")
(display (display-fixed (/ (length rs) (/ wall-us 1000000.0)) 1))
(display " req/s") (newline)

(when (> (vector-length durations) 0)
  (newline)
  (display "=== latency (ms) ===") (newline)
  (let ((min-us  (vector-ref durations 0))
        (max-us  (vector-ref durations (- (vector-length durations) 1)))
        (mean-us (/ (apply + (map cdr rs)) (length rs)))
        (p50     (percentile durations 50))
        (p90     (percentile durations 90))
        (p99     (percentile durations 99)))
    (display "  min  ") (display (display-fixed (us->ms min-us)  2)) (newline)
    (display "  p50  ") (display (display-fixed (us->ms p50)     2)) (newline)
    (display "  p90  ") (display (display-fixed (us->ms p90)     2)) (newline)
    (display "  p99  ") (display (display-fixed (us->ms p99)     2)) (newline)
    (display "  max  ") (display (display-fixed (us->ms max-us)  2)) (newline)
    (display "  mean ") (display (display-fixed (us->ms mean-us) 2)) (newline)))

(when (< (length ok-rs) (length rs))
  (newline)
  (display "WARNING: ")
  (display (- (length rs) (length ok-rs)))
  (display " non-200 responses (or errors).") (newline))
