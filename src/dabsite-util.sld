(define-library (dabsite util)
  (import (scheme base)
          (srfi 13)
          (scm database postgres)
          (dabsite db))
  (export non-empty-string?
          non-empty-trimmed?
          non-empty-or-false
          assoc-val
          row-field
          rows
          alist-rows
          exec
          mime-from-path)
  (begin

    ;; ----------------------------------------------------------------
    ;; String predicates used across modules. A "non-empty string" is
    ;; a string that, after optional trimming, has at least one char.
    ;; The trimming variant is what most form-input checks want; the
    ;; plain one is for already-normalized values.
    ;; ----------------------------------------------------------------

    (define (non-empty-string? x)
      (and (string? x) (not (string=? x ""))))

    (define (non-empty-trimmed? x)
      (and (string? x) (not (string=? (string-trim-both x) ""))))

    (define (non-empty-or-false x)
      (and (non-empty-string? x) x))

    ;; ----------------------------------------------------------------
    ;; alist accessors. assoc-val returns #f when the key is missing;
    ;; row-field returns "" so HTML/format callers can splice it in
    ;; without a separate guard.
    ;; ----------------------------------------------------------------

    (define (assoc-val alist key)
      (let ((p (assoc key alist)))
        (and p (cdr p))))

    (define (row-field r k)
      (let ((p (assoc k r)))
        (if p (cdr p) "")))

    ;; ----------------------------------------------------------------
    ;; DB convenience wrappers. Each accepts an optional params list
    ;; that is interpolated via pg-format-sql when present.
    ;; ----------------------------------------------------------------

    (define (rows cfg sql . maybe-params)
      (let ((params (if (null? maybe-params) '() (car maybe-params))))
        (with-db cfg
          (lambda (c)
            (pg-result-rows
              (if (null? params)
                  (pg-query c sql)
                  (pg-query c (pg-format-sql sql params))))))))

    (define (alist-rows cfg sql . maybe-params)
      (let ((params (if (null? maybe-params) '() (car maybe-params))))
        (with-db cfg
          (lambda (c)
            (pg-result->alist-list
              (if (null? params)
                  (pg-query c sql)
                  (pg-query c (pg-format-sql sql params))))))))

    (define (exec cfg sql . maybe-params)
      (let ((params (if (null? maybe-params) '() (car maybe-params))))
        (with-db cfg
          (lambda (c)
            (if (null? params)
                (pg-exec c sql)
                (pg-exec c (pg-format-sql sql params)))))))

    ;; ----------------------------------------------------------------
    ;; MIME types by filename suffix. Single source of truth used by
    ;; both the static-file handler and the uploads module.
    ;; ----------------------------------------------------------------

    (define mime-suffix-map
      '((".html" . "text/html; charset=utf-8")
        (".htm"  . "text/html; charset=utf-8")
        (".css"  . "text/css; charset=utf-8")
        (".js"   . "application/javascript; charset=utf-8")
        (".json" . "application/json; charset=utf-8")
        (".svg"  . "image/svg+xml")
        (".png"  . "image/png")
        (".jpg"  . "image/jpeg")
        (".jpeg" . "image/jpeg")
        (".gif"  . "image/gif")
        (".webp" . "image/webp")
        (".avif" . "image/avif")
        (".ico"  . "image/x-icon")
        (".pdf"  . "application/pdf")
        (".txt"  . "text/plain; charset=utf-8")
        (".md"   . "text/markdown; charset=utf-8")
        (".mp3"  . "audio/mpeg")
        (".mp4"  . "video/mp4")
        (".zip"  . "application/zip")))

    (define (mime-from-path path)
      (let ((lower (string-downcase path)))
        (let loop ((m mime-suffix-map))
          (cond
            ((null? m) "application/octet-stream")
            ((string-suffix? (caar m) lower) (cdar m))
            (else (loop (cdr m)))))))
))
