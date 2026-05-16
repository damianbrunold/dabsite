(define-library (dabsite db)
  (import (scheme base)
          (scheme write)
          (scheme file)
          (scheme read)
          (scm fs)
          (srfi 1)
          (srfi 132)
          (scm database postgres)
          (scm log))
  (export db-config
          make-db-config
          with-db
          db-exec
          db-query
          db-rows
          db-alist
          run-migrations!)
  (begin

    ;; A db-config is just an opaque vector of connection parameters.
    ;; Features should never touch (scm database postgres) directly;
    ;; they go through with-db / db-exec / db-query.

    (define-record-type db-config
      (make-db-config host port user password database)
      db-config?
      (host     db-config-host)
      (port     db-config-port)
      (user     db-config-user)
      (password db-config-password)
      (database db-config-database))

    (define (with-db cfg proc)
      (with-pg-connection (db-config-host cfg)
                          (db-config-port cfg)
                          (db-config-user cfg)
                          (db-config-password cfg)
                          (db-config-database cfg)
                          proc))

    (define (db-exec cfg sql)
      (with-db cfg (lambda (c) (pg-exec c sql))))

    (define (db-query cfg sql)
      (with-db cfg (lambda (c) (pg-query c sql))))

    (define (db-rows cfg sql)
      (pg-result-rows (db-query cfg sql)))

    (define (db-alist cfg sql)
      (pg-result->alist-list (db-query cfg sql)))

    ;; --- Migrations ---
    ;;
    ;; Migrations live in <migrations-dir>/NNNN_name.sql. They are applied in
    ;; lexical order. Each filename is recorded in schema_migrations after
    ;; successful application so it is never re-run.

    (define (read-file-string path)
      (call-with-input-file path
        (lambda (port)
          (let ((out (open-output-string)))
            (let loop ()
              (let ((c (read-char port)))
                (cond
                  ((eof-object? c) (get-output-string out))
                  (else (write-char c out) (loop)))))))))

    (define (sql-files dir)
      ;; Returns a sorted list of "NNNN_name.sql" filenames (no path).
      (let* ((all (directory-files dir))
             (sql (filter (lambda (f)
                            (let ((n (string-length f)))
                              (and (>= n 4)
                                   (string=? (substring f (- n 4) n) ".sql"))))
                          all)))
        (list-sort string<? sql)))

    (define (applied-set conn)
      (pg-exec conn
        "CREATE TABLE IF NOT EXISTS schema_migrations (
           filename text PRIMARY KEY,
           applied_at timestamptz NOT NULL DEFAULT now()
         )")
      (let* ((res  (pg-query conn "SELECT filename FROM schema_migrations"))
             (rows (pg-result-rows res)))
        (map (lambda (row) (vector-ref row 0)) rows)))

    (define (already-applied? applied name)
      (let loop ((xs applied))
        (cond ((null? xs) #f)
              ((string=? (car xs) name) #t)
              (else (loop (cdr xs))))))

    (define (sql-quote-literal s)
      ;; Minimal single-quote escape for inline SQL literals (filename).
      (let* ((out (open-output-string)))
        (write-char #\' out)
        (let loop ((i 0))
          (cond
            ((= i (string-length s))
             (write-char #\' out)
             (get-output-string out))
            (else
             (let ((c (string-ref s i)))
               (when (char=? c #\') (write-char #\' out))
               (write-char c out)
               (loop (+ i 1))))))))

    (define (apply-migration! conn dir filename)
      (let* ((path (string-append dir "/" filename))
             (sql  (read-file-string path)))
        (log-info "migrate" (string-append "applying " filename))
        (pg-exec conn "BEGIN")
        (guard (exn (#t
                     (guard (e (#t #f)) (pg-exec conn "ROLLBACK"))
                     (raise exn)))
          (pg-exec conn sql)
          (pg-exec conn
            (string-append
              "INSERT INTO schema_migrations (filename) VALUES ("
              (sql-quote-literal filename)
              ") ON CONFLICT DO NOTHING"))
          (pg-exec conn "COMMIT"))))

    (define (run-migrations! cfg dir)
      "Apply any pending SQL migrations from dir. Idempotent."
      (when (not (directory-exists? dir))
        (error "migrations directory does not exist" dir))
      (with-db cfg
        (lambda (conn)
          (let ((applied (applied-set conn))
                (files   (sql-files dir)))
            (log-info "migrate"
              (string-append "running migrations from " dir))
            (for-each
              (lambda (f)
                (cond
                  ((already-applied? applied f) #f)  ; skip silently
                  (else (apply-migration! conn dir f))))
              files)
            (log-info "migrate" "migrations complete")))))

))
