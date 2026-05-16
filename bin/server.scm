;; Usage: scm bin/server.scm [config-path]
;;
;; Reads config.scm (or the path given as argv) and starts the webapp.
;; The config file is plain scheme — see config.example.scm for the
;; expected bindings.

(import (scheme base)
        (scheme write)
        (scheme load)
        (scheme process-context)
        (scm module)
        (scm fs))

;; Resolve the project root from this script's location so the server can be
;; launched from anywhere. command-line returns the script path as arg 0.
(define (project-root)
  (let* ((argv (command-line))
         (this (car argv)))
    (directory-name (absolute-path this))))

(define root
  (let ((bin-dir (project-root)))
    (directory-name bin-dir)))

;; Make src/ importable.
(module-search-path! (cons (string-append root "/src")
                           (module-search-path)))

(import (damian db)
        (damian auth)
        (damian app))

(define (config-path)
  (let ((args (cdr (command-line))))
    (cond ((pair? args) (car args))
          (else (string-append root "/config.scm")))))

(define cfg-path (config-path))
(when (not (file-exists? cfg-path))
  (display "config file not found: ") (display cfg-path) (newline)
  (display "copy config.example.scm to config.scm and edit it.") (newline)
  (exit 1))

(load cfg-path)

;; Resolve relative paths against the project root.
(define (abs-path p)
  (if (and (> (string-length p) 0)
           (char=? (string-ref p 0) #\/))
      p
      (string-append root "/" p)))

(define dbcfg
  (make-db-config db-host db-port db-user db-password db-name))

(run-migrations! dbcfg (abs-path migrations-dir))

(define auth
  (make-auth cookie-name cookie-secret cookie-max-age auth-passphrase-hash))

(serve http-port
       http-host
       (abs-path static-dir)
       (abs-path files-dir)
       dbcfg
       auth
       secure-cookies?)
