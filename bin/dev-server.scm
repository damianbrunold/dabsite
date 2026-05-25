;; Usage: scm bin/dev-server.scm [config-path]
;;
;; Development supervisor: launches bin/server.scm as a child process and
;; restarts it whenever a watched source file changes. Watches all .sld
;; files under src/, bin/server.scm, and the config file. In-flight
;; requests are dropped on restart, as with Flask's reloader.

(import (scheme base)
        (scheme write)
        (scheme process-context)
        (scm fs)
        (scm system)
        (srfi 18))

(define (project-root)
  (let* ((argv (command-line))
         (this (car argv)))
    (directory-name (directory-name (absolute-path this)))))

(define root (project-root))

(define (config-path)
  (let ((args (cdr (command-line))))
    (cond ((pair? args) (car args))
          (else (string-append root "/config.scm")))))

(define cfg-path (config-path))

(define (sld-files dir)
  (let loop ((entries (directory-files dir)) (acc '()))
    (cond
      ((null? entries) acc)
      (else
       (let* ((name (car entries))
              (n (string-length name)))
         (loop (cdr entries)
               (if (and (> n 4)
                        (string=? (substring name (- n 4) n) ".sld"))
                   (cons (string-append dir "/" name) acc)
                   acc)))))))

(define (watched-files)
  (append (sld-files (string-append root "/src"))
          (list (string-append root "/bin/server.scm")
                cfg-path)))

(define (snapshot files)
  (map (lambda (f)
         (cons f (if (file-exists? f)
                     (file-modification-timestamp f)
                     0)))
       files))

(define (changed? before after)
  (let loop ((a before) (b after))
    (cond
      ((or (null? a) (null? b)) (not (and (null? a) (null? b))))
      ((not (= (cdr (car a)) (cdr (car b)))) #t)
      (else (loop (cdr a) (cdr b))))))

(define (changed-paths before after)
  (let loop ((a before) (b after) (acc '()))
    (cond
      ((or (null? a) (null? b)) (reverse acc))
      ((= (cdr (car a)) (cdr (car b)))
       (loop (cdr a) (cdr b) acc))
      (else
       (loop (cdr a) (cdr b) (cons (car (car a)) acc))))))

(define (log msg)
  (display "[dev-server] ") (display msg) (newline))

(define (start-child)
  (log (string-append "starting: scm bin/server.scm " cfg-path))
  (start-program
    (list "scm" (string-append root "/bin/server.scm") cfg-path)
    `((work-dir ,root))))

(define (stop-child proc)
  (when (process-alive? proc)
    (process-kill proc #t))
  (process-wait proc))

(define poll-interval 0.5)
(define debounce-interval 0.3)

;; After a change is first detected, wait until mtimes stop moving before
;; restarting. Editors often write files in chunks; restarting mid-write
;; makes the child die on a truncated read.
(define (settle snap)
  (thread-sleep! debounce-interval)
  (let ((next (snapshot (watched-files))))
    (if (changed? snap next)
        (settle next)
        next)))

(define (relative-to-root path)
  (let ((prefix (string-append root "/"))
        (n (string-length path)))
    (if (and (>= n (string-length prefix))
             (string=? (substring path 0 (string-length prefix)) prefix))
        (substring path (string-length prefix) n)
        path)))

(define (run)
  (when (not (file-exists? cfg-path))
    (display "config file not found: ") (display cfg-path) (newline)
    (exit 1))
  (log (string-append "watching " (number->string (length (watched-files)))
                      " files; poll every "
                      (number->string poll-interval) "s"))
  (let loop ((proc (start-child))
             (snap (snapshot (watched-files))))
    (thread-sleep! poll-interval)
    (let* ((files (watched-files))
           (new-snap (snapshot files))
           (alive (process-alive? proc)))
      (cond
        ((changed? snap new-snap)
         (let* ((settled (settle new-snap))
                (changes (changed-paths snap settled)))
           (log (string-append "change detected ("
                               (number->string (length changes))
                               "): "
                               (apply string-append
                                      (let join ((xs (map relative-to-root changes)))
                                        (cond
                                          ((null? xs) '(""))
                                          ((null? (cdr xs)) (list (car xs)))
                                          (else (cons (car xs)
                                                      (cons ", " (join (cdr xs))))))))))
           (when alive
             (stop-child proc))
           (log (if alive "restarting child" "child not running; starting"))
           (loop (start-child) settled)))
        ((not alive)
         (process-wait proc)
         (log "child exited; waiting for file change to retry")
         (loop proc snap))
        (else
         (loop proc snap))))))

(run)
