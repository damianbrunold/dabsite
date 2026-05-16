(define-library (damian auth)
  (import (scheme base)
          (scheme write)
          (scheme time)
          (scheme char)
          (scheme cxr)
          (srfi 13)
          (srfi 18)
          (scm crypto)
          (scm net http request)
          (scm net http response)
          (scm net http route)
          (damian util))
  (export make-auth
          auth?
          authed?
          verify-passphrase
          sign-token
          verify-token
          install-auth-routes!
          require-auth)
  (begin

    ;; auth holds everything needed to verify credentials and cookies. The
    ;; passphrase hash is parsed once into (iter . (salt-bv . hash-bv)).
    (define-record-type auth
      (make-auth-record cookie-name cookie-secret cookie-max-age
                        pbkdf2-iter pbkdf2-salt pbkdf2-hash)
      auth?
      (cookie-name    auth-cookie-name)
      (cookie-secret  auth-cookie-secret)
      (cookie-max-age auth-cookie-max-age)
      (pbkdf2-iter    auth-pbkdf2-iter)
      (pbkdf2-salt    auth-pbkdf2-salt)
      (pbkdf2-hash    auth-pbkdf2-hash))

    ;; --- helpers ---

    (define (split-on-dollar s)
      (let* ((n (string-length s))
             (acc '()))
        (let loop ((i 0) (start 0))
          (cond
            ((= i n) (reverse (cons (substring s start n) acc)))
            ((char=? (string-ref s i) #\$)
             (set! acc (cons (substring s start i) acc))
             (loop (+ i 1) (+ i 1)))
            (else (loop (+ i 1) start))))))

    (define (parse-pbkdf2-hash s)
      ;; Format: pbkdf2$<iter>$<salt-b64>$<hash-b64>
      ;; Returns three values via a 3-list: (iter salt-bv hash-bv).
      (let ((parts (split-on-dollar s)))
        (when (not (= 4 (length parts)))
          (error "auth: malformed pbkdf2 hash"))
        (when (not (string=? (car parts) "pbkdf2"))
          (error "auth: not a pbkdf2 hash"))
        (let ((iter (string->number (cadr parts)))
              (salt (base64-decode (caddr parts)))
              (hash (base64-decode (cadddr parts))))
          (when (not iter) (error "auth: bad iteration count"))
          (list iter salt hash))))

    (define (make-auth cookie-name cookie-secret cookie-max-age
                       passphrase-hash)
      (let ((parsed (parse-pbkdf2-hash passphrase-hash)))
        (make-auth-record cookie-name
                          (base64-decode cookie-secret)
                          cookie-max-age
                          (car parsed)
                          (cadr parsed)
                          (caddr parsed))))

    ;; --- passphrase ---

    (define (verify-passphrase auth pw)
      (let ((computed (pbkdf2-sha256 (string->utf8 pw)
                                     (auth-pbkdf2-salt auth)
                                     (auth-pbkdf2-iter auth)
                                     (bytevector-length (auth-pbkdf2-hash auth)))))
        (constant-time-bv-equal? computed (auth-pbkdf2-hash auth))))

    ;; --- cookie tokens ---
    ;;
    ;; A token is "<base64(payload)>.<hex(hmac-sha256 secret payload-bytes)>".
    ;; The payload is "v1|<issued-at>". We don't enforce expiry here — the
    ;; browser drops the cookie when max-age passes — but having issued-at
    ;; in the payload makes it possible to invalidate by rotating the
    ;; cookie-secret without changing format.

    (define (payload-now)
      (string-append "v1|" (number->string (exact (round (current-second))))))

    (define (sign-bytes secret bv)
      (hmac-sha256 secret bv))

    (define (sign-token auth)
      (let* ((payload  (payload-now))
             (pl-bytes (string->utf8 payload))
             (b64      (base64-encode pl-bytes))
             (mac      (sign-bytes (auth-cookie-secret auth) pl-bytes)))
        (string-append b64 "." (bytevector->hex mac))))

    (define (verify-token auth token)
      (cond
        ((or (not token) (string=? token "")) #f)
        (else
         (let ((dot (string-index token #\.)))
           (cond
             ((not dot) #f)
             (else
              (guard (exn (#t #f))
                (let* ((b64  (substring token 0 dot))
                       (mac  (substring token (+ dot 1)
                                        (string-length token)))
                       (pl   (base64-decode b64))
                       (calc (sign-bytes (auth-cookie-secret auth) pl))
                       (got  (hex->bytevector mac)))
                  (constant-time-bv-equal? calc got)))))))))

    ;; --- request inspection ---

    (define (request-cookies req)
      (parse-cookie-header (http-request-header req "Cookie")))

    (define (authed? auth req)
      (let* ((cookies (request-cookies req))
             (token   (cookie-ref cookies (auth-cookie-name auth))))
        (verify-token auth token)))

    ;; --- responses ---

    (define (redirect to . set-cookie)
      (let ((headers (cons (cons "Location" to)
                           (if (pair? set-cookie)
                               (list (cons "Set-Cookie" (car set-cookie)))
                               '()))))
        (make-http-response 302 headers "")))

    (define (login-page-html error?)
      (let ((out (open-output-string)))
        (write-string "<!doctype html><html lang=\"en\"><head>" out)
        (write-string "<meta charset=\"utf-8\"><title>Login</title>" out)
        (write-string "<link rel=\"stylesheet\" href=\"/static/site.css\">" out)
        (write-string "</head><body><main class=\"login\">" out)
        (write-string "<h1>Login</h1>" out)
        (when error?
          (write-string "<p class=\"error\">Wrong passphrase.</p>" out))
        (write-string "<form method=\"post\" action=\"/login\">" out)
        (write-string "<label>Passphrase " out)
        (write-string
          (string-append "<input type=\"password\" name=\"passphrase\" "
                         "autocomplete=\"current-password\" autofocus required>")
          out)
        (write-string "</label>" out)
        (write-string "<button type=\"submit\">Sign in</button>" out)
        (write-string "</form></main></body></html>" out)
        (get-output-string out)))

    (define (html-ok body)
      (make-http-response 200
        '(("Content-Type" . "text/html; charset=utf-8"))
        body))

    ;; --- routes ---

    (define (install-auth-routes! router auth secure-cookies?)
      (router-add! router "GET" "/login"
        (lambda (req params)
          (cond
            ((authed? auth req) (redirect "/"))
            (else (html-ok (login-page-html #f))))))

      (router-add! router "POST" "/login"
        (lambda (req params)
          (let* ((form (parse-www-form (or (http-request-body req) "")))
                 (pw   (form-ref form "passphrase" "")))
            (cond
              ((verify-passphrase auth pw)
               (let* ((token (sign-token auth))
                      (sc    (if secure-cookies?
                                 (format-set-cookie (auth-cookie-name auth)
                                                    token
                                                    (auth-cookie-max-age auth)
                                                    "/"
                                                    'secure)
                                 (format-set-cookie (auth-cookie-name auth)
                                                    token
                                                    (auth-cookie-max-age auth)
                                                    "/"
                                                    'no-secure))))
                 (redirect "/" sc)))
              (else
               ;; Constant-ish delay on failure caps the rate at ~2
               ;; attempts/sec/connection, taking online brute-force
               ;; from "feasible" to "needs a botnet". PBKDF2 already
               ;; runs ~hundreds of ms, but a wrong-format password
               ;; could short-circuit it.
               (thread-sleep! 0.5)
               (html-ok (login-page-html #t)))))))

      (router-add! router "POST" "/logout"
        (lambda (req params)
          (let ((clear (if secure-cookies?
                           (format-set-cookie (auth-cookie-name auth) ""
                                              0 "/" 'secure)
                           (format-set-cookie (auth-cookie-name auth) ""
                                              0 "/" 'no-secure))))
            (redirect "/" clear)))))

    ;; --- middleware ---

    (define (require-auth auth handler)
      (lambda (req params)
        (cond
          ((authed? auth req) (handler req params))
          (else (redirect "/login")))))

))
