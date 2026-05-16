;; Unit tests for (damian auth). No DB, no network.

(import (scheme base) (scheme write) (scheme process-context) (scm module))
(module-search-path! (cons "src" (module-search-path)))

(import (damian auth)
        (damian util)
        (scm crypto)
        (scm net http request)
        (scm test)
        (srfi 64))

(test-runner-factory scm-test-runner)

;; --- helpers ---

(define (make-test-auth pw)
  (let* ((salt (string->utf8 "saltsaltsaltsalt"))
         (iter 1000)
         (hash (pbkdf2-sha256 (string->utf8 pw) salt iter 32))
         (hash-str (string-append "pbkdf2$"
                                  (number->string iter) "$"
                                  (base64-encode salt) "$"
                                  (base64-encode hash)))
         (secret  (base64-encode (string->utf8 "thirty-two-byte-secret-for-test!"))))
    (make-auth "damian_auth" secret 3600 hash-str)))

(test-begin "auth")

(test-group "passphrase verification"
  (let ((a (make-test-auth "secret")))
    (test-assert "correct passphrase"     (verify-passphrase a "secret"))
    (test-assert "wrong passphrase"   (not (verify-passphrase a "wrong")))
    (test-assert "empty rejected"     (not (verify-passphrase a "")))
    (test-assert "trailing space rejected"
                 (not (verify-passphrase a "secret ")))))

(test-group "token signing and verification"
  (let* ((a (make-test-auth "secret"))
         (t (sign-token a)))
    (test-assert "verifies own token" (verify-token a t))
    (test-assert "tampered MAC rejected"
                 (not (verify-token a (string-append t "x"))))
    (test-assert "empty rejected" (not (verify-token a "")))
    (test-assert "garbage rejected" (not (verify-token a "abc.def")))
    (test-assert "no dot rejected" (not (verify-token a "nodot")))
    ;; Token created by a different secret must not verify.
    (let ((other (make-auth "damian_auth"
                            (base64-encode (string->utf8 "OTHER-thirty-two-byte-secret!!!!"))
                            3600
                            (string-append
                              "pbkdf2$1000$"
                              (base64-encode (string->utf8 "saltsaltsaltsalt")) "$"
                              (base64-encode (pbkdf2-sha256 (string->utf8 "x")
                                                            (string->utf8 "saltsaltsaltsalt")
                                                            1000 32))))))
      (test-assert "wrong secret rejected" (not (verify-token other t))))))

(test-group "authed? via request cookies"
  (let* ((a (make-test-auth "secret"))
         (t (sign-token a))
         (good (make-http-request "GET" "/notes"
                  (list (cons "Cookie" (string-append "damian_auth=" t)))
                  #f))
         (no-cookie (make-http-request "GET" "/notes" '() #f))
         (bad       (make-http-request "GET" "/notes"
                  (list (cons "Cookie" "damian_auth=garbage.deadbeef"))
                  #f))
         (other-cookie (make-http-request "GET" "/notes"
                  (list (cons "Cookie" "session=irrelevant"))
                  #f)))
    (test-assert "good cookie"     (authed? a good))
    (test-assert "no cookie"   (not (authed? a no-cookie)))
    (test-assert "bad mac"     (not (authed? a bad)))
    (test-assert "wrong name"  (not (authed? a other-cookie)))))

(test-end "auth")

(exit (if (= 0 (last-run-failed-tests)) 0 1))
