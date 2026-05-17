(define-library (dabsite shortener)
  (import (scheme base)
          (scheme write)
          (scheme char)
          (srfi 13)
          (scm crypto)
          (scm database postgres)
          (scm net http request)
          (scm net http response)
          (scm net http route)
          (scm net http forms)
          (scm html builder)
          (dabsite db)
          (dabsite auth)
          (dabsite views))
  (export install-shortener-routes!
          ;; exposed for tests
          random-code
          valid-code?)
  (begin

    ;; ----- helpers -----

    (define base62-chars
      "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")

    (define code-length 6)

    (define (random-code)
      ;; 6 base62 characters → ~57 bits of entropy. Plenty for personal use.
      (let* ((bv (random-bytes code-length))
             (out (open-output-string)))
        (let loop ((i 0))
          (cond
            ((= i code-length) (get-output-string out))
            (else
             (write-char (string-ref base62-chars
                                     (modulo (bytevector-u8-ref bv i) 62))
                         out)
             (loop (+ i 1)))))))

    (define (valid-code? s)
      ;; Mirror the DB constraint so we reject bad inputs before SQL runs.
      (let ((n (string-length s)))
        (and (> n 0) (<= n 32)
             (let loop ((i 0))
               (cond
                 ((= i n) #t)
                 (else
                  (let ((c (string-ref s i)))
                    (cond
                      ((or (and (char>=? c #\a) (char<=? c #\z))
                           (and (char>=? c #\A) (char<=? c #\Z))
                           (and (char>=? c #\0) (char<=? c #\9))
                           (char=? c #\-) (char=? c #\_))
                       (loop (+ i 1)))
                      (else #f)))))))))

    (define (valid-target? s)
      ;; Accept http(s) URLs only — protects against javascript: or data:
      ;; redirects sneaking in.
      (or (and (>= (string-length s) 7)
               (string=? (substring s 0 7) "http://"))
          (and (>= (string-length s) 8)
               (string=? (substring s 0 8) "https://"))))

    ;; ----- DB ops -----

    (define (lookup-code cfg code)
      ;; Returns the target string, or #f.
      (let ((rs (with-db cfg
                  (lambda (c)
                    (pg-result-rows
                      (pg-query c
                        (string-append
                          "SELECT target FROM short_urls WHERE code = "
                          (pg-quote-literal code))))))))
        (cond
          ((pair? rs) (vector-ref (car rs) 0))
          (else #f))))

    (define (bump-hits! cfg code)
      ;; Best-effort counter. Wrap in guard so a transient failure doesn't
      ;; turn a successful redirect into a 5xx.
      (guard (exn (#t #f))
        (with-db cfg
          (lambda (c)
            (pg-exec c
              (string-append
                "UPDATE short_urls SET hits = hits + 1 WHERE code = "
                (pg-quote-literal code)))))))

    (define (code-exists? cfg code)
      (let ((rs (with-db cfg
                  (lambda (c)
                    (pg-result-rows
                      (pg-query c
                        (string-append
                          "SELECT 1 FROM short_urls WHERE code = "
                          (pg-quote-literal code) " LIMIT 1")))))))
        (pair? rs)))

    (define (allocate-code cfg)
      (let loop ((tries 0))
        (cond
          ((>= tries 20)
           (error "shortener: failed to allocate a unique random code"))
          (else
           (let ((c (random-code)))
             (cond ((code-exists? cfg c) (loop (+ tries 1)))
                   (else c)))))))

    (define (insert! cfg code target note)
      (with-db cfg
        (lambda (c)
          (pg-exec c
            (string-append
              "INSERT INTO short_urls (code, target, note) VALUES ("
              (pg-quote-literal code) ", "
              (pg-quote-literal target) ", "
              (pg-quote-literal note) ")")))))

    (define (delete-code! cfg code)
      (with-db cfg
        (lambda (c)
          (pg-exec c
            (string-append
              "DELETE FROM short_urls WHERE code = "
              (pg-quote-literal code))))))

    (define (list-codes cfg)
      (with-db cfg
        (lambda (c)
          (pg-result->alist-list
            (pg-query c
              (string-append
                "SELECT code, target, note, "
                "       hits::text AS hits, "
                "       to_char(created_at, 'YYYY-MM-DD HH24:MI') AS created "
                "FROM short_urls ORDER BY created_at DESC"))))))

    ;; ----- views -----

    (define (row-field r k) (let ((p (assoc k r))) (if p (cdr p) "")))

    (define (short-row-sxml e)
      (let ((code    (row-field e "code"))
            (target  (row-field e "target"))
            (note    (row-field e "note"))
            (hits    (row-field e "hits"))
            (created (row-field e "created")))
        `(tr
           (td (@ (class "code")) ,code)
           (td (@ (class "short"))
               (a (@ (href ,(string-append "/s/" code))
                     (target "_blank") (rel "noopener"))
                  ,(string-append "/s/" code)))
           (td (@ (class "url"))
               (a (@ (href ,target) (target "_blank") (rel "noopener"))
                  ,target))
           (td (@ (class "note"))    ,note)
           (td (@ (class "hits"))    ,hits)
           (td (@ (class "created")) ,created)
           (td (@ (class "acts"))
               (form (@ (method "post")
                        (action ,(string-append "/shortener/" code "/delete"))
                        (class "inline")
                        (data-confirm "Delete this short link?"))
                 (button (@ (class "linkish danger")) "delete"))))))

    (define (render-admin req auth cfg . opt)
      (let* ((entries (list-codes cfg))
             (msg     (cond ((pair? opt) (car opt)) (else #f)))
             (body
               `((header (@ (class "feeds-head")) (h1 "URL shortener"))
                 ,@(if msg `((p (@ (class "hint")) ,msg)) '())
                 (form (@ (method "post") (action "/shortener")
                          (class "feed-new"))
                   (h2 "New short link")
                   (label "Target URL "
                     (input (@ (type "url") (name "target") (required #t)
                               (placeholder "https://example.com/"))))
                   (label "Code (optional) "
                     (input (@ (type "text") (name "code")
                               (pattern "[A-Za-z0-9_-]{1,32}")
                               (placeholder "auto: 6 random chars"))))
                   (label "Note "
                     (input (@ (type "text") (name "note")
                               (placeholder "what's this for?"))))
                   (button (@ (type "submit")) "Add"))
                 (table (@ (class "feed-table mobile-cards links-list"))
                   (thead (tr (th "code") (th "short url") (th "target")
                              (th "note") (th "hits") (th "created") (th)))
                   (tbody ,@(map short-row-sxml entries))))))
        (html-response
          (render-page req auth
                       '((title  . "URL shortener")
                         (active . shortener)
                         (body-class . "feeds-page"))
                       (html->string body)))))

    ;; ----- routes -----

    (define (install-shortener-routes! router cfg auth)
      ;; Public redirect. No auth — anyone with the code follows the link.
      (router-add! router "GET" "/s/:code"
        (lambda (req params)
          (let* ((code (params-ref params "code"))
                 (target (and code (valid-code? code) (lookup-code cfg code))))
            (cond
              ((not target) (http-not-found))
              (else
               (bump-hits! cfg code)
               (make-http-response 302
                 (list (cons "Location" target)
                       (cons "Cache-Control" "no-store"))
                 ""))))))

      ;; Private admin.
      (router-add! router "GET" "/shortener"
        (require-auth auth
          (lambda (req params) (render-admin req auth cfg))))

      (router-add! router "POST" "/shortener"
        (require-auth auth
          (lambda (req params)
            (let* ((form   (parse-www-form (or (http-request-body req) "")))
                   (raw    (string-trim-both (form-ref form "code" "")))
                   (target (string-trim-both (form-ref form "target" "")))
                   (note   (string-trim-both (form-ref form "note" ""))))
              (cond
                ((string=? target "")
                 (render-error 400 "Target URL is required."))
                ((not (valid-target? target))
                 (render-error 400 "Target must be an http(s) URL."))
                ((and (not (string=? raw "")) (not (valid-code? raw)))
                 (render-error 400
                   "Code must match [A-Za-z0-9_-]{1,32}."))
                (else
                 (let ((code (cond ((string=? raw "") (allocate-code cfg))
                                   (else raw))))
                   (cond
                     ((and (not (string=? raw "")) (code-exists? cfg code))
                      (render-admin req auth cfg
                        (string-append "Code '" code "' is already taken.")))
                     (else
                      (insert! cfg code target note)
                      (make-http-response 302
                        (list (cons "Location" "/shortener")) ""))))))))))

      (router-add! router "POST" "/shortener/:code/delete"
        (require-auth auth
          (lambda (req params)
            (let ((code (params-ref params "code")))
              (when (and code (valid-code? code))
                (delete-code! cfg code))
              (make-http-response 302
                (list (cons "Location" "/shortener")) ""))))))

))
