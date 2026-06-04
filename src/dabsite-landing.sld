(define-library (dabsite landing)
  (import (scheme base)
          (scheme write)
          (srfi 13)
          (scm database postgres)
          (scm net http request)
          (scm net http response)
          (scm net http route)
          (scm net http forms)
          (scm html builder)
          (scm markdown)
          (dabsite db)
          (dabsite util)
          (dabsite auth)
          (dabsite views))
  (export install-landing-routes!)
  (begin

    ;; ------------------------------------------------------------
    ;; Pages are keyed by short slugs. The home page lives at "home" and
    ;; is rendered at "/". All other slugs live under "/p/:slug".
    ;; ------------------------------------------------------------

    (define (page-by-slug cfg slug)
      ;; Returns alist or #f.
      (let* ((rows (with-db cfg
                     (lambda (c)
                       (pg-result->alist-list
                         (pg-query c
                           "SELECT slug, title, format, source, html_cache FROM pages WHERE slug = $1 LIMIT 1"
                           slug))))))
        (if (pair? rows) (car rows) #f)))

    (define (list-pages cfg)
      (with-db cfg
        (lambda (c)
          (pg-result->alist-list
            (pg-query c
              "SELECT slug, title, to_char(updated_at, 'YYYY-MM-DD HH24:MI') AS updated FROM pages ORDER BY slug")))))

    (define (valid-slug? s)
      ;; Lowercase letters, digits, hyphen, underscore. 1..40 chars.
      ;; Excludes `home` from being created by URL because the home page
      ;; is the implicit / route — but the editor uses `home` directly,
      ;; so it's fine here. We just block weird characters.
      (and (string? s)
           (let ((n (string-length s)))
             (and (> n 0) (<= n 40)
                  (let loop ((i 0))
                    (cond
                      ((= i n) #t)
                      (else
                       (let ((c (string-ref s i)))
                         (cond
                           ((or (and (char>=? c #\a) (char<=? c #\z))
                                (and (char>=? c #\0) (char<=? c #\9))
                                (char=? c #\-)
                                (char=? c #\_))
                            (loop (+ i 1)))
                           (else #f))))))))))

    (define (page-field page key)
      (let ((p (assoc key page)))
        (if p (cdr p) "")))

    (define (render-page-source format source)
      (if (string=? format "html") source (markdown->html source)))

    (define (save-page! cfg slug title format source)
      (let ((html (render-page-source format source)))
        (with-db cfg
          (lambda (c)
            (pg-exec c
              (string-append
                "INSERT INTO pages (slug, title, format, source, html_cache, updated_at) "
                "VALUES ($1, $2, $3, $4, $5, now()) "
                "ON CONFLICT (slug) DO UPDATE SET "
                "title = EXCLUDED.title, "
                "format = EXCLUDED.format, "
                "source = EXCLUDED.source, "
                "html_cache = EXCLUDED.html_cache, "
                "updated_at = now()")
              slug title format source html)))))

    (define (cached-or-render cfg page)
      ;; If html_cache is empty (e.g. seeded row), render and persist.
      (let ((cached (page-field page "html_cache"))
            (source (page-field page "source"))
            (format (page-field page "format")))
        (if (non-empty-string? cached)
            cached
            (let ((html (render-page-source format source)))
             (with-db cfg
               (lambda (c)
                 (pg-exec c
                   "UPDATE pages SET html_cache = $1, updated_at = now() WHERE slug = $2"
                   html (page-field page "slug"))))
             html))))

    ;; --- views ---

    (define (render-page-view req auth cfg slug active)
      (let ((page (page-by-slug cfg slug)))
        (cond
          ((not page) (render-error 404 "Page not found."))
          (else
           (let* ((title (page-field page "title"))
                  (html  (cached-or-render cfg page))
                  (body  `(article (@ (class "page"))
                            ,(raw html)
                            ,@(if (authed? auth req)
                                  `((p (@ (class "page-actions"))
                                       (a (@ (href ,(string-append "/edit/" slug)))
                                          "Edit")))
                                  '()))))
             (html-response
               (render-page req auth
                            (list (cons 'title title)
                                  (cons 'active active)
                                  (cons 'lang "de"))
                            (html->string body))))))))

    (define (render-edit-view req auth cfg slug)
      (let* ((existing (page-by-slug cfg slug))
             (title    (if existing
                           (page-field existing "title")
                           (if (string=? slug "home") "Damian Brunold" slug)))
             (format   (if existing (page-field existing "format") "markdown"))
             (source   (if existing (page-field existing "source") ""))
             (cancel-href (if (string=? slug "home")
                              "/"
                              (string-append "/p/" slug)))
             (body
               `((h1 "Edit: " ,slug)
                 (form (@ (method "post")
                          (action ,(string-append "/edit/" slug))
                          (class "page-edit"))
                   (label "Title"
                     (input (@ (type "text") (name "title")
                               (value ,title) (required #t))))
                   (label "Format"
                     (select (@ (name "format"))
                       (option (@ (value "markdown")
                                  (selected ,(string=? format "markdown")))
                               "Markdown")
                       (option (@ (value "html")
                                  (selected ,(string=? format "html")))
                               "HTML fragment")))
                   (label "Source"
                     (textarea (@ (name "source") (rows "20") (required #t))
                       ,source))
                   (div (@ (class "actions"))
                     (button (@ (type "submit")) "Save")
                     (a (@ (href ,cancel-href)) "Cancel"))))))
        (html-response
          (render-page req auth
                       (list (cons 'title (string-append "Edit " slug))
                             (cons 'active (if (string=? slug "home") 'home #f))
                             (cons 'body-class "editor"))
                       (html->string body)))))

    (define (page-row-sxml row)
      (let* ((slug    (page-field row "slug"))
             (title   (page-field row "title"))
             (updated (page-field row "updated"))
             (view-href (if (string=? slug "home") "/" (string-append "/p/" slug)))
             (display-title (if (string=? title "") slug title)))
        `(li (a (@ (class "row") (href ,view-href))
                (span (@ (class "name")) ,display-title)
                (span (@ (class "meta"))
                      ,slug " " ,(raw "&middot;") " updated " ,updated))
             " "
             (a (@ (class "linkish")
                   (href ,(string-append "/edit/" slug)))
                "edit")
             ,@(if (string=? slug "home")
                   '()
                   `(" "
                     (form (@ (method "post")
                              (action ,(string-append "/pages/" slug "/delete"))
                              (class "inline")
                              (data-confirm "Delete this page?"))
                       (button (@ (class "linkish danger")) "delete")))))))

    (define (render-index req auth cfg)
      (let* ((rows (list-pages cfg))
             (body
               `((header (@ (class "feeds-head")) (h1 "Pages"))
                 (form (@ (method "post") (action "/pages")
                          (class "grocery-new"))
                   (input (@ (type "text") (name "slug") (required #t)
                             (maxlength "40") (pattern "[a-z0-9_-]+")
                             (placeholder "new slug (a-z, 0-9, -, _)")))
                   (button (@ (type "submit")) "Create / open"))
                 ,(cond
                    ((null? rows)
                     `(p (@ (class "empty")) "No pages yet."))
                    (else
                     `(ul (@ (class "grocery-lists"))
                          ,@(map page-row-sxml rows)))))))
        (html-response
          (render-page req auth
                       '((title . "Pages")
                         (active . pages))
                       (html->string body)))))

    (define (delete-page! cfg slug)
      (with-db cfg
        (lambda (c)
          (pg-exec c "DELETE FROM pages WHERE slug = $1" slug))))

    (define (handle-edit-save req params cfg auth)
      (let* ((slug (params-ref params "slug"))
             (form (parse-www-form (or (http-request-body req) "")))
             (title  (form-ref form "title" ""))
             (format (form-ref form "format" "markdown"))
             (source (form-ref form "source" "")))
        (if (or (not (non-empty-trimmed? slug))
                (not (non-empty-trimmed? title)))
            (render-error 400 "Slug and title are required.")
            (begin
              (save-page! cfg slug title format source)
              (make-http-response 302
                (list (cons "Location"
                            (if (string=? slug "home")
                                "/"
                                (string-append "/p/" slug))))
                "")))))

    ;; --- route registration ---

    (define (install-landing-routes! router cfg auth)
      (router-add! router "GET" "/"
        (lambda (req params)
          (render-page-view req auth cfg "home" 'home)))

      (router-add! router "GET" "/p/:slug"
        (lambda (req params)
          (render-page-view req auth cfg (params-ref params "slug") #f)))

      (router-add! router "GET" "/edit/:slug"
        (require-auth auth
          (lambda (req params)
            (let ((slug (params-ref params "slug")))
              (cond
                ((not (valid-slug? slug))
                 (render-error 400 "Invalid slug."))
                (else (render-edit-view req auth cfg slug)))))))

      (router-add! router "POST" "/edit/:slug"
        (require-auth auth
          (lambda (req params)
            (let ((slug (params-ref params "slug")))
              (cond
                ((not (valid-slug? slug))
                 (render-error 400 "Invalid slug."))
                (else (handle-edit-save req params cfg auth)))))))

      ;; Pages index — authed.
      (router-add! router "GET" "/pages"
        (require-auth auth
          (lambda (req params) (render-index req auth cfg))))

      ;; "New page" form posts here. Just redirects to /edit/<slug> after
      ;; validating; the actual row is created when /edit/<slug> is saved.
      (router-add! router "POST" "/pages"
        (require-auth auth
          (lambda (req params)
            (let* ((form (parse-www-form (or (http-request-body req) "")))
                   (slug (string-trim-both (form-ref form "slug" ""))))
              (cond
                ((not (valid-slug? slug))
                 (render-error 400 "Slug must match [a-z0-9_-]{1,40}."))
                (else
                 (make-http-response 302
                   (list (cons "Location"
                               (string-append "/edit/" slug))) "")))))))

      (router-add! router "POST" "/pages/:slug/delete"
        (require-auth auth
          (lambda (req params)
            (let ((slug (params-ref params "slug")))
              (cond
                ((not (valid-slug? slug))
                 (render-error 400 "Invalid slug."))
                ((string=? slug "home")
                 (render-error 400 "The home page can't be deleted."))
                (else
                 (delete-page! cfg slug)
                 (make-http-response 302
                   (list (cons "Location" "/pages")) ""))))))))

))
