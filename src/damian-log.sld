(define-library (damian log)
  (import (scheme base)
          (scheme write)
          (srfi 19))
  (export log-info
          log-warn
          log-error
          log-access)
  (begin

    ;; ============================================================
    ;; Structured logging to stderr. systemd captures stderr via
    ;; journald, where retention is controlled by journald.conf
    ;; (SystemMaxUse, MaxRetentionSec). No file-based rotation is
    ;; needed at the app level.
    ;;
    ;; Line format:
    ;;   YYYY-MM-DD HH:MM:SS LEVEL [module] message
    ;;
    ;; Access lines additionally carry:
    ;;   method url -> status (Nms)
    ;; ============================================================

    (define (pad2 n)
      (let ((s (number->string n)))
        (cond ((< n 10) (string-append "0" s)) (else s))))

    (define (timestamp)
      (let ((d (current-date)))
        (string-append
          (number->string (date-year d)) "-"
          (pad2 (date-month d)) "-"
          (pad2 (date-day d)) " "
          (pad2 (date-hour d)) ":"
          (pad2 (date-minute d)) ":"
          (pad2 (date-second d)))))

    (define (emit level module msg)
      (let ((out (current-error-port)))
        (write-string (timestamp) out)
        (write-string " " out)
        (write-string level out)
        (write-string " [" out)
        (write-string module out)
        (write-string "] " out)
        (write-string msg out)
        (newline out)))

    (define (log-info  module msg) (emit "INFO " module msg))
    (define (log-warn  module msg) (emit "WARN " module msg))
    (define (log-error module msg) (emit "ERROR" module msg))

    (define (log-access method url status duration-ms)
      ;; Single-line access record. status and duration-ms are integers.
      (emit "INFO " "http"
            (string-append method " " url
                           " -> " (number->string status)
                           " (" (number->string duration-ms) "ms)")))

))
