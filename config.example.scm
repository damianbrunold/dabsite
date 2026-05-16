;; damian_www configuration.
;; Copy this file to config.scm and edit values. config.scm is gitignored.
;; The webapp reads it via (load) at startup, so it is plain scheme:
;; just define the bindings below.

;; --- HTTP ---
(define http-port  8088)
(define http-host  "127.0.0.1")   ; bind only loopback; reverse proxy fronts it

;; --- Paths ---
;; static-dir is served at /static/. Defaults to ./static next to bin/server.scm.
(define static-dir "static")
;; migrations-dir holds the schema migration SQL files run at startup.
(define migrations-dir "migrations")
;; files-dir is the content-addressed blob store for uploaded files.
;; Created on startup if missing. Gitignore this directory.
(define files-dir "data/files")

;; --- Database ---
(define db-host     "127.0.0.1")
(define db-port     5432)
(define db-user     "damian_www")
(define db-password "change-me")
(define db-name     "damian_www")

;; --- Auth ---
;; Generate with: scm bin/hash-passphrase.scm 'your passphrase here'
;; The result is a string of the form "pbkdf2$<iterations>$<salt-b64>$<hash-b64>".
(define auth-passphrase-hash
  "pbkdf2$200000$REPLACE_WITH_BASE64_SALT$REPLACE_WITH_BASE64_HASH")

;; Secret used to sign the auth cookie. 32+ random bytes, base64-encoded.
;; Generate with: scm bin/gen-secret.scm
(define cookie-secret "REPLACE_WITH_BASE64_SECRET")

;; Cookie name and max-age (seconds). 10 years by default.
(define cookie-name    "damian_auth")
(define cookie-max-age (* 60 60 24 365 10))

;; When #t, the auth cookie is marked Secure (HTTPS only). Production
;; behind the reverse proxy must use #t. For local dev over plain HTTP,
;; set #f so the browser will store the cookie.
(define secure-cookies? #f)
