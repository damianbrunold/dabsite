(define-library (dabsite util)
  (import (scheme base)
          (scheme write)
          (srfi 13))
  (export sql-quote-literal
          sql-quote-int)
  (begin

    ;; ============================================================
    ;; SQL quoting
    ;;
    ;; These remain dabsite-local until (scm database postgres) gains
    ;; first-class parameter binding — at which point all SQL in dabsite
    ;; should switch from string concatenation to parameterised queries
    ;; and these helpers can be retired.
    ;; ============================================================

    (define (sql-quote-literal s)
      ;; Doubles single quotes. Wraps in single quotes. PostgreSQL ships
      ;; with standard_conforming_strings=on (default since 9.1) so a
      ;; backslash inside '...' is a literal backslash — doubling it
      ;; would corrupt real data without adding any safety. CALLERS MUST
      ;; use this for any user-controlled string in SQL.
      (let* ((n (string-length s))
             (out (open-output-string)))
        (write-char #\' out)
        (let loop ((i 0))
          (cond
            ((= i n)
             (write-char #\' out)
             (get-output-string out))
            (else
             (let ((c (string-ref s i)))
               (cond ((char=? c #\') (write-string "''" out))
                     (else           (write-char c out)))
               (loop (+ i 1))))))))

    (define (sql-quote-int n)
      ;; Accepts an integer; returns its decimal representation. Use for
      ;; numeric parameters so callers can't slip in SQL via string concat.
      (cond
        ((integer? n) (number->string n))
        ((string? n)
         (let ((parsed (string->number n)))
           (cond ((and parsed (integer? parsed)) (number->string parsed))
                 (else (error "sql-quote-int: not an integer string" n)))))
        (else (error "sql-quote-int: not an integer" n))))
))
