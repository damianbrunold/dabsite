(define-library (dabsite db)
  (import (scheme base)
          (scm database postgres)
          (rename (scm database migrations)
                  (run-migrations! migrations-run!))
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
    ;; Thin wrapper over (scm database migrations): opens a pg
    ;; connection and supplies the postgres-specific exec/query
    ;; callbacks. Each applied filename is logged via (scm log) so
    ;; lines end up in journald alongside everything else.

    (define (run-migrations! cfg dir)
      "Apply any pending SQL migrations from dir. Idempotent."
      (with-db cfg
        (lambda (conn)
          (log-info "migrate"
                    (string-append "running migrations from " dir))
          (migrations-run!
            (lambda (sql) (pg-exec conn sql))
            (lambda (sql) (pg-result-rows (pg-query conn sql)))
            dir
            `((log-proc ,(lambda (m) (log-info "migrate" m))))))))
))
