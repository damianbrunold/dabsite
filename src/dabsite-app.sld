(define-library (dabsite app)
  (import (scheme base)
          (scm net http request)
          (scm net http response)
          (scm net http route)
          (scm net http server)
          (scm net http static)
          (scm net http middleware)
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
          (dabsite views)
          (scm log))
  (export build-router
          build-static-router
          serve)
  (begin

    ;; ---------------- core / health ----------------

    (define (install-core-routes! router)
      (router-add! router "GET" "/healthz"
        (lambda (req params) (text-response "ok\n"))))

    ;; ---------------- static assets ----------------

    (define (well-known-static-handler static-dir filename)
      ;; Serves one specific file from static-dir at a well-known
      ;; top-level path (e.g. /robots.txt, /favicon.ico).
      (lambda (req params)
        (file-response (string-append static-dir "/" filename)
                       `((request . ,req)
                         (cache-control . "public, max-age=3600")))))

    (define (install-static-routes! router static-dir)
      ;; (scm net http static) refuses traversal and dot-files, and
      ;; caches the ?v= URLs of static-url for good.
      (set-static-dir! static-dir)
      (router-add! router "GET" "/static/*"
        (make-static-handler static-dir))
      ;; Well-known root paths the browser asks for unprompted.
      (router-add! router "GET" "/robots.txt"
        (well-known-static-handler static-dir "robots.txt"))
      (router-add! router "GET" "/favicon.ico"
        (well-known-static-handler static-dir "icon.png")))

    ;; ---------------- router composition ----------------

    (define (build-router static-dir files-dir db-cfg auth secure-cookies?)
      (let ((router (make-router)))
        (install-core-routes! router)
        (install-auth-routes!     router auth)
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
        ;; (scm net http middleware): one access-log line per request
        ;; (path only: query strings carry search terms), errors logged
        ;; and answered with an error page that never shows their text,
        ;; and the security headers incl. a CSP without inline scripts
        ;; or styles.
        (serve-forever port
                       (with-access-log
                         (with-security-headers
                           (with-error-handler (router->handler router)
                             (lambda (req)
                               (render-error 500 "Something went wrong.")))))
                       0
                       host
                       300000       ; read-timeout-ms (5 min, room for slow uploads)
                       (* 26 1024 1024))))  ; max-body-bytes (26 MB)

))
