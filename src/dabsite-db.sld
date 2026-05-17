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

    ;; A db-config carries the connection params plus an attached
    ;; connection pool. Features go through with-db / db-exec /
    ;; db-query and never touch the pool directly.

    (define-record-type db-config
      (%make-db-config-record host port user password database pool)
      db-config?
      (host     db-config-host)
      (port     db-config-port)
      (user     db-config-user)
      (password db-config-password)
      (database db-config-database)
      (pool     db-config-pool))

    (define default-pool-capacity 8)

    (define (make-db-config host port user password database . opt)
      "Create a db-config and its backing connection pool. The optional
       argument is the pool capacity (default 8)."
      (let ((capacity (cond ((pair? opt) (car opt))
                            (else default-pool-capacity))))
        (%make-db-config-record
          host port user password database
          (make-pg-pool host port user password database capacity))))

    (define (with-db cfg proc)
      (with-pg-pool-connection (db-config-pool cfg) proc))

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
