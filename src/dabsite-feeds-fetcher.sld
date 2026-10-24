(define-library (dabsite feeds-fetcher)
  (import (scheme base)
          (scm net http client)
          (scm net http response)
          (scm feed))
  (export fetch-feed
          fetch-result?
          fetch-result-ok?
          fetch-result-error
          fetch-result-title
          fetch-result-entries)
  (begin

    ;; --------------------------------------------------------------
    ;; Fetches one feed URL and parses it. Either ok? is #t and the
    ;; title + entries are populated, or ok? is #f and error holds a
    ;; short human-readable message. Never throws.
    ;; --------------------------------------------------------------
    (define-record-type fetch-result
      (make-fetch-result ok? error title entries)
      fetch-result?
      (ok?     fetch-result-ok?)
      (error   fetch-result-error)
      (title   fetch-result-title)
      (entries fetch-result-entries))

    (define (err msg) (make-fetch-result #f msg #f '()))
    (define (ok title entries) (make-fetch-result #t #f title entries))

    (define ua-header
      '(("User-Agent" . "dabsite/1.0 (+https://www.damianbrunold.ch)")
        ("Accept"     . "application/atom+xml, application/rss+xml, application/xml;q=0.9, */*;q=0.8")))

    ;; A hanging site must not stall the feed scheduler.
    (define fetch-timeout-seconds 30)

    (define (fetch-feed url)
      (guard (exn (#t (err (string-append "fetch error: "
                                          (cond ((string? exn) exn)
                                                ((error-object? exn)
                                                 (error-object-message exn))
                                                (else "exception"))))))
        ;; The body stays bytes so the XML parser can honour the encoding
        ;; the feed declares (a text decode assumed UTF-8).
        (let ((resp (http-get-bytevector url ua-header fetch-timeout-seconds)))
          (cond
            ((not (= 200 (http-response-status resp)))
             (err (string-append "HTTP "
                                 (number->string (http-response-status resp)))))
            (else
             (let ((parsed (guard (parse-exn (#t #f))
                             (parse-feed-bytevector (http-response-body resp)))))
               (cond
                 ((not parsed) (err "parse error"))
                 (else (ok (car parsed) (cdr parsed))))))))))
))
