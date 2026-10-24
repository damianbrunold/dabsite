;; Unit tests for (dabsite auth). No DB, no network.

(import (scheme base) (scheme write) (scheme process-context) (scm module))
(module-search-path! (cons "src" (module-search-path)))

(import (dabsite auth)
        (scm crypto)
        (scm crypto password)
        (scm net http request)
        (scm test)
        (srfi 13)
        (srfi 64))

(test-runner-factory scm-test-runner)

;; --- helpers ---

(define secret (base64-encode (string->utf8 "thirty-two-byte-secret-for-test!")))

(define (test-hash pw)
  (parameterize ((password-pbkdf2-iterations 1000))
    (password-hash pw)))

(define (make-test-auth pw . maybe-secret)
  (make-auth "damian_auth"
             (if (pair? maybe-secret) (car maybe-secret) secret)
             3600 (test-hash pw) #f))

;; "damian_auth=<token>; Path=/; ..." -> a request carrying that cookie
(define (request-with set-cookie)
  (let ((v (cdr set-cookie)))
    (cookie-request (substring v 0 (string-index v #\;)))))

(define (cookie-request value)
  (make-http-request "GET" "/notes" (list (cons "Cookie" value)) #f))

(test-begin "auth")

(test-group "passphrase verification"
  (let ((a (make-test-auth "secret")))
    (test-assert "correct passphrase"     (verify-passphrase a "secret"))
    (test-assert "wrong passphrase"   (not (verify-passphrase a "wrong")))
    (test-assert "empty rejected"     (not (verify-passphrase a "")))
    (test-assert "trailing space rejected"
                 (not (verify-passphrase a "secret ")))))

(test-group "session cookie"
  (let* ((a    (make-test-auth "secret"))
         (sc   (login-cookie a))
         (good (request-with sc)))
    (test-equal "Set-Cookie header" "Set-Cookie" (car sc))
    (test-assert "HttpOnly" (string-contains (cdr sc) "HttpOnly"))
    (test-assert "good cookie" (authed? a good))
    (test-assert "no cookie"
                 (not (authed? a (make-http-request "GET" "/notes" '() #f))))
    (test-assert "garbage cookie"
                 (not (authed? a (cookie-request "damian_auth=garbage.deadbeef"))))
    (test-assert "tampered token"
                 (not (authed? a (cookie-request
                                   (string-append
                                     (substring (cdr sc) 0 (string-index (cdr sc) #\;))
                                     "x")))))
    (test-assert "wrong cookie name"
                 (not (authed? a (cookie-request "session=irrelevant"))))
    (test-assert "wrong secret rejected"
                 (not (authed? (make-test-auth "secret"
                                 (base64-encode (string->utf8 "OTHER-thirty-two-byte-secret!!!!")))
                               good)))
    ;; A new passphrase hash in config.scm ends the existing sessions.
    (test-assert "old session after passphrase change"
                 (not (authed? (make-test-auth "new") good)))))

(test-end "auth")

(exit (if (= 0 (last-run-failed-tests)) 0 1))
