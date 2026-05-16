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

    (define (fetch-feed url)
      (guard (exn (#t (err (string-append "fetch error: "
                                          (cond ((string? exn) exn)
                                                (else "exception"))))))
        (let ((resp (http-get url ua-header)))
          (cond
            ((not (= 200 (http-response-status resp)))
             (err (string-append "HTTP "
                                 (number->string (http-response-status resp)))))
            (else
             (let* ((body  (http-response-body resp))
                    ;; (scm feed) accepts either; pass through whatever the
                    ;; client gave us. The XML parser honours the declared
                    ;; encoding for bytevector input.
                    (parsed
                      (guard (parse-exn (#t #f))
                        (cond
                          ((bytevector? body) (parse-feed-bytevector body))
                          ((string? body)     (parse-feed-string body))
                          (else               #f)))))
               (cond
                 ((not parsed) (err "parse error"))
                 (else (ok (car parsed) (cdr parsed))))))))))
))
