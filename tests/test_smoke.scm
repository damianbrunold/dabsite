;; Stage 1 smoke test.
;;
;; Boots the router in a background thread on an ephemeral port and verifies
;; the basic routes work. Postgres is NOT required for this test — we wire
;; the router directly via (dabsite app)'s build-router/serve and skip
;; run-migrations!.
;;
;; Run from the project root: scm tests/test_smoke.scm

(import (scheme base)
        (scheme write)
        (scheme process-context)
        (scm module))

(module-search-path! (cons "src" (module-search-path)))

(import (dabsite app)
        (scm test)
        (scm net http server)
        (scm net http route)
        (scm net http request)
        (scm net http response)
        (scm net http client)
        (srfi 13)
        (srfi 18)
        (srfi 64))

(define port 18099)
(define base (string-append "http://127.0.0.1:" (number->string port)))

(define router (build-static-router "static"))

(define server-handle
  (tcp-http-serve port
                  (lambda (req)
                    (let ((resp ((lambda (r)
                                   (guard (exn (#t (make-http-response
                                                     500
                                                     '(("Content-Type" . "text/plain"))
                                                     "test server error")))
                                     (router-dispatch router r)))
                                 req)))
                      resp))
                  0
                  "127.0.0.1"))

;; Give the listener a moment.
(thread-sleep! 0.2)

(test-runner-factory scm-test-runner)

(test-begin "stage1-smoke")

(test-group "healthz"
  (let ((r (http-get (string-append base "/healthz") '())))
    (test-eqv "200" 200 (http-response-status r))))

(test-group "static asset"
  (let ((r (http-get (string-append base "/static/site.css") '())))
    (test-eqv "200" 200 (http-response-status r))
    (test-assert "body contains css rule"
                 (let* ((body (http-response-body r))
                        (s    (if (string? body) body (utf8->string body))))
                   (not (not (string-contains s "body")))))))

(test-group "unknown route"
  (let ((r (http-get (string-append base "/no-such-path") '())))
    (test-eqv "404" 404 (http-response-status r))))

(test-group "path traversal"
  (let ((r (http-get (string-append base "/static/../config.example.scm") '())))
    (test-assert "rejected" (not (= 200 (http-response-status r))))))

(test-end "stage1-smoke")

(server-stop server-handle)

(exit (if (= 0 (last-run-failed-tests)) 0 1))
