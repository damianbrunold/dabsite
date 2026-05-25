(define-library (dabsite app)
  (import (scheme base)
          (scheme write)
          (scheme file)
          (scheme time)
          (scm net http request)
          (scm net http response)
          (scm net http route)
          (scm net http server)
          (scm fs)
          (srfi 1)
          (srfi 13)
          (dabsite db)
          (dabsite util)
          (dabsite auth)
          (dabsite landing)
          (dabsite notepad)
          (dabsite feeds)
          (dabsite shortener)
          (dabsite polls)
          (dabsite tracker)
          (dabsite files)
          (dabsite grocery)
          (dabsite calendar)
          (dabsite events)
          (scm log))
  (export build-router
          build-static-router
          serve)
  (begin

    (define (text-response body)
      (make-http-response 200
                          '(("Content-Type" . "text/plain; charset=utf-8"))
                          body))

    ;; ---------------- core / health ----------------

    (define (install-core-routes! router)
      (router-add! router "GET" "/healthz"
        (lambda (req params) (text-response "ok\n"))))

    ;; ---------------- static assets ----------------

    (define (read-file-bytes path)
      (let* ((size (file-size path))
             (port (open-binary-input-file path))
             (bv   (read-bytevector size port)))
        (close-input-port port)
        (if (eof-object? bv) (bytevector) bv)))

    (define (path-safe? rel)
      (and (> (string-length rel) 0)
           (not (char=? (string-ref rel 0) #\/))
           (not (string-contains rel ".."))))

    (define (make-static-handler root)
      (lambda (req params)
        (let ((rel (params-ref params "*")))
          (if (or (not rel) (not (path-safe? rel)))
              (make-http-response 403 '() "Forbidden")
              (let ((full (string-append root "/" rel)))
                (cond
                  ((not (file-exists? full))     (http-not-found))
                  ((directory-exists? full)
                   (make-http-response 403 '() "Forbidden"))
                  (else
                   (make-http-response 200
                     (list (cons "Content-Type" (mime-from-path full)))
                     (read-file-bytes full)))))))))

    (define (well-known-static-handler static-dir filename ctype)
      ;; Serves one specific file from static-dir at a well-known
      ;; top-level path (e.g. /robots.txt, /favicon.ico).
      (lambda (req params)
        (let ((full (string-append static-dir "/" filename)))
          (cond
            ((not (file-exists? full)) (http-not-found))
            (else
             (make-http-response 200
               (list (cons "Content-Type" ctype)
                     (cons "Cache-Control" "public, max-age=3600"))
               (read-file-bytes full)))))))

    (define (install-static-routes! router static-dir)
      (router-add! router "GET" "/static/*"
        (make-static-handler static-dir))
      ;; Well-known root paths the browser asks for unprompted.
      (router-add! router "GET" "/robots.txt"
        (well-known-static-handler static-dir "robots.txt"
                                   "text/plain; charset=utf-8"))
      (router-add! router "GET" "/favicon.ico"
        (well-known-static-handler static-dir "icon.png" "image/png")))

    ;; ---------------- router composition ----------------

    (define (build-router static-dir files-dir db-cfg auth secure-cookies?)
      (let ((router (make-router)))
        (install-core-routes! router)
        (install-auth-routes!     router auth secure-cookies?)
        (install-landing-routes!  router db-cfg auth)
        (install-notepad-routes!  router db-cfg auth)
        (install-feed-routes!      router db-cfg auth)
        (install-shortener-routes! router db-cfg auth)
        (install-poll-routes!      router db-cfg auth secure-cookies?)
        (install-tracker-routes!   router db-cfg auth)
        (install-files-routes!     router db-cfg auth files-dir)
        (install-grocery-routes!   router db-cfg auth)
        (install-calendar-routes!  router db-cfg auth)
        (install-events-routes!    router db-cfg auth)
        ;; static is registered last so the dynamic routes match first.
        (install-static-routes! router static-dir)
        router))

    ;; Smaller router for tests and degraded modes: just /healthz + static.
    ;; Useful when starting without a configured DB or auth.
    (define (build-static-router static-dir)
      (let ((router (make-router)))
        (install-core-routes! router)
        (install-static-routes! router static-dir)
        router))

    ;; ---------------- entry point ----------------

    ;; --- security headers ---
    ;; Added to every dynamic response. CSP locks scripts and styles to
    ;; this origin and forbids inline scripts; the theme bootstrap that
    ;; used to live inline in <head> moved to site.js for this reason.
    (define security-headers
      '(("X-Content-Type-Options" . "nosniff")
        ("X-Frame-Options"        . "DENY")
        ("Referrer-Policy"        . "same-origin")
        ("Content-Security-Policy"
         . "default-src 'self'; img-src 'self' data:; style-src 'self'; script-src 'self'; base-uri 'none'; frame-ancestors 'none'")))

    (define (with-security-headers handler)
      (lambda (req)
        (let ((resp (handler req)))
          (if (http-response? resp)
              (let* ((existing (http-response-headers resp))
                     (taken    (map (lambda (h) (string-downcase (car h)))
                                    existing))
                     (extra    (filter
                                 (lambda (h)
                                   (not (member (string-downcase (car h))
                                                taken string=?)))
                                 security-headers)))
                (make-http-response
                  (http-response-status resp)
                  (append existing extra)
                  (http-response-body resp)))
              resp))))

    (define (with-access-log handler)
      ;; Wraps the router dispatcher: logs one line per request.
      (lambda (req)
        (let ((start (current-jiffy)))
          (guard
              (exn
               (#t
                (let ((elapsed (quotient (* 1000 (- (current-jiffy) start))
                                         (jiffies-per-second))))
                  (log-error "http"
                    (string-append
                      (http-request-method req) " "
                      (http-request-url req)
                      " raised: "
                      (if (error-object? exn)
                          (error-object-message exn)
                          (let ((p (open-output-string)))
                            (display exn p)
                            (get-output-string p)))))
                  (log-access (http-request-method req)
                              (http-request-url req)
                              500 elapsed)
                  (raise exn))))
            (let* ((resp    (handler req))
                   (elapsed (quotient (* 1000 (- (current-jiffy) start))
                                       (jiffies-per-second)))
                   (status  (if (http-response? resp)
                                (http-response-status resp)
                                0)))
              (log-access (http-request-method req)
                          (http-request-url req)
                          status elapsed)
              resp)))))

    (define (serve port host static-dir files-dir
                   db-cfg auth secure-cookies?)
      (let ((router (build-router static-dir files-dir
                                  db-cfg auth secure-cookies?)))
        (start-feed-scheduler! db-cfg)
        (log-info "boot"
                  (string-append "listening on http://" host ":"
                                 (number->string port)))
        ;; Raise max-body-bytes above the 4 MB default so the files
        ;; upload (capped at 25 MB by the app) can complete. Read
        ;; timeout is generous because large multipart uploads over a
        ;; slow link can take a while to land.
        (serve-forever port
                       (with-access-log
                         (with-security-headers
                           (lambda (req) (router-dispatch router req))))
                       0
                       host
                       300000       ; read-timeout-ms (5 min, room for slow uploads)
                       (* 26 1024 1024))))  ; max-body-bytes (26 MB)

))
