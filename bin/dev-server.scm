;; Usage: scm bin/dev-server.scm [config-path]
;;
;; Development supervisor for the webapp: restarts bin/server.scm whenever
;; a watched source file changes (all .sld under src/, bin/server.scm, and
;; the config file), and auto-retries it on a backoff if it exits on its
;; own. In-flight requests are dropped on restart, as with Flask's reloader.
;;
;; The supervise mechanism lives in (scm reloader); this script just wires
;; up the watch set and the child command.

(import (scheme base)
        (scheme write)
        (scheme process-context)
        (scm fs)
        (scm reloader))

(define root
  (let ((this (car (command-line))))
    (directory-name (directory-name (absolute-path this)))))

(define cfg-path
  (let ((args (cdr (command-line))))
    (if (pair? args) (car args) (string-append root "/config.scm"))))

(when (not (file-exists? cfg-path))
  (display "config file not found: ") (display cfg-path) (newline)
  (exit 1))

(define (watched)
  (append (files-with-suffix (string-append root "/src") ".sld")
          (list (string-append root "/bin/server.scm")
                cfg-path)))

(supervise (list "scm" (string-append root "/bin/server.scm") cfg-path)
           watched
           `((label . "dev-server")
             (work-dir . ,root)
             (root . ,root)))
