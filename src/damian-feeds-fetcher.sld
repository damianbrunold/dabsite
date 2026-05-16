(define-library (damian feeds-fetcher)
  (import (scheme base)
          (scheme write)
          (scheme file)
          (srfi 13)
          (scm crypto)
          (scm fs)
          (scm net http client)
          (scm net http response)
          (damian feeds-parser)
          (damian util))
  (export fetch-feed
          fetch-result?
          fetch-result-ok?
          fetch-result-error
          fetch-result-title
          fetch-result-entries)
  (begin

    ;; --------------------------------------------------------------
    ;; Result of fetching one feed URL. Either ok? is #t and the title +
    ;; entries are populated, or ok? is #f and error holds a short
    ;; human-readable message. Never throws.
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
      '(("User-Agent" . "damian_www/1.0 (+https://www.damianbrunold.ch)")
        ("Accept"     . "application/atom+xml, application/rss+xml, application/xml;q=0.9, */*;q=0.8")))

    ;; The .NET HttpClient decodes the body to a .NET string for us, so by
    ;; the time we see it the bytes are normalised UTF-16. When we write
    ;; them back to disk as UTF-8 the bytes match what's in the file, but
    ;; the embedded XML declaration may still claim a different encoding
    ;; (e.g. ISO-8859-1) — which would then cause XmlReader to misinterpret
    ;; non-ASCII characters. Rewriting the declaration sidesteps that.
    (define (rewrite-encoding body)
      (cond
        ((and (>= (string-length body) 5)
              (string=? (substring body 0 5) "<?xml"))
         (let* ((end (string-index body #\>)))
           (cond
             ((not end) body)
             (else
              (string-append "<?xml version=\"1.0\" encoding=\"utf-8\"?>"
                             (substring body (+ end 1)
                                        (string-length body)))))))
        (else body)))

    (define (random-suffix)
      (bytevector->hex (random-bytes 8)))

    (define (tmp-path)
      (string-append "/tmp/damian-feed-" (random-suffix) ".xml"))

    (define (write-string-to-file path s)
      (call-with-output-file path
        (lambda (port) (write-string s port))))

    (define (try-delete-file path)
      (guard (e (#t #f))
        (when (file-exists? path) (delete-file path))))

    (define (fetch-feed url)
      (guard (exn (#t (err (string-append "fetch error: "
                                          (cond ((string? exn) exn)
                                                (else "exception"))))))
        (let* ((resp (http-get url ua-header)))
          (cond
            ((not (= 200 (http-response-status resp)))
             (err (string-append "HTTP "
                                 (number->string (http-response-status resp)))))
            (else
             (let* ((body  (http-response-body resp))
                    (body  (cond ((string? body) body)
                                 ((bytevector? body) (utf8->string body))
                                 (else "")))
                    (path  (tmp-path)))
               (guard (parse-exn (#t
                                  (try-delete-file path)
                                  (err "parse error")))
                 (write-string-to-file path (rewrite-encoding body))
                 (let* ((parsed (parse-feed-file path)))
                   (try-delete-file path)
                   (ok (car parsed) (cdr parsed))))))))))

))
