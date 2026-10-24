(define-library (dabsite auth)
  (import (scheme base)
          (srfi 18)
          (scm crypto)
          (scm crypto password)
          (scm net http request)
          (scm net http response)
          (scm net http route)
          (scm net http forms)
          (scm net http session)
          (scm net http static)
          (scm html builder))
  (export make-auth
          auth?
          authed?
          verify-passphrase
          login-cookie
          install-auth-routes!
          require-auth
          site-lang
          set-site-lang!
          set-static-dir!
          static-url)
  (begin

    ;; Site-wide default for <html lang="…">. server.scm sets this from
    ;; config at startup, before any request is served, so the set! is
    ;; race-free. Pages can override per-render via the 'lang opt.
    (define %site-lang "en")
    (define (site-lang) %site-lang)
    (define (set-site-lang! lang) (set! %site-lang lang))

    ;; Asset URLs with a ?v=<mtime> cache-buster, so the static handler
    ;; can let browsers cache them for good. build-router sets the
    ;; directory; until then (tests) URLs carry no version.
    (define %static-url (lambda (rel) (string-append "/static/" rel)))
    (define (set-static-dir! dir)
      (set! %static-url (make-static-url dir "/static")))
    (define (static-url rel) (%static-url rel))

    ;; auth holds the session cookie config and the passphrase hash.
    (define-record-type auth
      (make-auth-record session passphrase-hash)
      auth?
      (session         auth-session)
      (passphrase-hash auth-passphrase-hash))

    ;; cookie-secret: base64, or a list of them (newest first) while
    ;; rotating keys; a short or malformed one raises here, at startup.
    (define (make-auth cookie-name cookie-secret cookie-max-age
                       passphrase-hash secure-cookies?)
      (make-auth-record
        (make-session-config cookie-name cookie-secret cookie-max-age
                             `((secure? . ,secure-cookies?)))
        passphrase-hash))

    ;; --- passphrase ---
    ;;
    ;; (scm crypto password): "pbkdf2$<iter>$<salt-b64>$<hash-b64>".

    (define (verify-passphrase auth pw)
      (password-verify pw (auth-passphrase-hash auth)))

    ;; --- session cookie ---
    ;;
    ;; The signed cookie (scm net http session) carries a stamp of the
    ;; passphrase hash: putting a new hash into config.scm ends every
    ;; existing session. Its lifetime is enforced by the token itself,
    ;; not only by the browser.

    (define (passphrase-stamp auth)
      (base64-encode
        (bytevector-copy
          (sha256-hash (string->utf8 (auth-passphrase-hash auth))) 0 12)))

    (define (login-cookie auth)
      (session-set-cookie (auth-session auth)
                          `((pw . ,(passphrase-stamp auth)))))

    (define (authed? auth req)
      (let* ((s  (session-ref (auth-session auth) req))
             (pw (and (pair? s) (assq 'pw s))))
        (and pw (equal? (cdr pw) (passphrase-stamp auth)))))

    ;; --- responses ---

    (define (redirect to . headers)
      (make-http-response 302 (cons (cons "Location" to) headers) ""))

    (define (login-page-html error?)
      (html->string
        (html5
          `(@ (lang ,(site-lang)))
          `(head (meta (@ (charset "utf-8")))
                 (title "Login")
                 (link (@ (rel "stylesheet") (href ,(static-url "site.css")))))
          `(body (main (@ (class "login"))
                       (h1 "Login")
                       ,@(if error?
                             `((p (@ (class "error")) "Wrong passphrase."))
                             '())
                       (form (@ (method "post") (action "/login"))
                         (label "Passphrase "
                           (input (@ (type "password") (name "passphrase")
                                     (autocomplete "current-password")
                                     (autofocus #t) (required #t))))
                         (button (@ (type "submit")) "Sign in")))))))

    (define (html-ok body)
      (make-http-response 200
        '(("Content-Type" . "text/html; charset=utf-8"))
        body))

    ;; --- routes ---

    (define (install-auth-routes! router auth)
      (router-add! router "GET" "/login"
        (lambda (req params)
          (if (authed? auth req)
              (redirect "/")
              (html-ok (login-page-html #f)))))

      (router-add! router "POST" "/login"
        (lambda (req params)
          (let ((pw (form-ref (http-request-form req) "passphrase" "")))
            (cond
              ((verify-passphrase auth pw)
               (redirect "/" (login-cookie auth)))
              (else
               ;; Constant-ish delay on failure caps the rate at ~2
               ;; attempts/sec/connection, taking online brute-force
               ;; from "feasible" to "needs a botnet".
               (thread-sleep! 0.5)
               (html-ok (login-page-html #t)))))))

      (router-add! router "POST" "/logout"
        (lambda (req params)
          (redirect "/" (session-clear-cookie (auth-session auth))))))

    ;; --- middleware ---

    (define (require-auth auth handler)
      (lambda (req params)
        (if (authed? auth req)
            (handler req params)
            (redirect "/login"))))

))
