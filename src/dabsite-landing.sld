(define-library (dabsite landing)
  (import (scheme base)
          (scheme write)
          (srfi 13)
          (scm database postgres)
          (scm net http request)
          (scm net http response)
          (scm net http route)
          (scm net http forms)
          (scm html)
          (dabsite db)
          (dabsite auth)
          (dabsite views)
          (dabsite markdown))
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
                           (string-append
                             "SELECT slug, title, format, source, html_cache "
                             "FROM pages WHERE slug = "
                             (pg-quote-literal slug)
                             " LIMIT 1")))))))
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
      (cond
        ((string=? format "html")     source)
        (else                         (render-markdown source))))

    (define (save-page! cfg slug title format source)
      (let ((html (render-page-source format source)))
        (with-db cfg
          (lambda (c)
            (pg-exec c
              (string-append
                "INSERT INTO pages (slug, title, format, source, html_cache, updated_at) VALUES ("
                (pg-quote-literal slug) ", "
                (pg-quote-literal title) ", "
                (pg-quote-literal format) ", "
                (pg-quote-literal source) ", "
                (pg-quote-literal html) ", now()) "
                "ON CONFLICT (slug) DO UPDATE SET "
                "title = EXCLUDED.title, "
                "format = EXCLUDED.format, "
                "source = EXCLUDED.source, "
                "html_cache = EXCLUDED.html_cache, "
                "updated_at = now()"))))))

    (define (cached-or-render cfg page)
      ;; If html_cache is empty (e.g. seeded row), render and persist.
      (let ((cached (page-field page "html_cache"))
            (source (page-field page "source"))
            (format (page-field page "format")))
        (cond
          ((and cached (> (string-length cached) 0)) cached)
          (else
           (let ((html (render-page-source format source)))
             (with-db cfg
               (lambda (c)
                 (pg-exec c
                   (string-append
                     "UPDATE pages SET html_cache = "
                     (pg-quote-literal html)
                     ", updated_at = now() WHERE slug = "
                     (pg-quote-literal (page-field page "slug"))))))
             html)))))

    ;; --- views ---

    (define (render-page-view req auth cfg slug active)
      (let ((page (page-by-slug cfg slug)))
        (cond
          ((not page) (render-error 404 "Page not found."))
          (else
           (let* ((title    (page-field page "title"))
                  (html     (cached-or-render cfg page))
                  (out      (open-output-string)))
             (write-string "<article class=\"page\">" out)
             (write-string html out)
             (when (authed? auth req)
               (write-string "<p class=\"page-actions\">" out)
               (write-string "<a href=\"/edit/" out)
               (write-string (html-attr-escape slug) out)
               (write-string "\">Edit</a></p>" out))
             (write-string "</article>" out)
             (html-response
               (render-page req auth
                            (list (cons 'title title)
                                  (cons 'active active))
                            (get-output-string out))))))))

    (define (render-edit-view req auth cfg slug)
      (let* ((existing (page-by-slug cfg slug))
             (title    (if existing (page-field existing "title")
                           (cond ((string=? slug "home") "Damian Brunold")
                                 (else slug))))
             (format   (if existing (page-field existing "format") "markdown"))
             (source   (if existing (page-field existing "source") ""))
             (out      (open-output-string)))
        (write-string "<h1>Edit: " out)
        (write-string (html-escape slug) out)
        (write-string "</h1>" out)
        (write-string "<form method=\"post\" action=\"/edit/" out)
        (write-string (html-attr-escape slug) out)
        (write-string "\" class=\"page-edit\">" out)

        (write-string "<label>Title<input type=\"text\" name=\"title\" value=\"" out)
        (write-string (html-attr-escape title) out)
        (write-string "\" required></label>" out)

        (write-string "<label>Format<select name=\"format\">" out)
        (write-string "<option value=\"markdown\"" out)
        (when (string=? format "markdown") (write-string " selected" out))
        (write-string ">Markdown</option>" out)
        (write-string "<option value=\"html\"" out)
        (when (string=? format "html") (write-string " selected" out))
        (write-string ">HTML fragment</option>" out)
        (write-string "</select></label>" out)

        (write-string "<label>Source<textarea name=\"source\" rows=\"20\" required>" out)
        (write-string (html-escape source) out)
        (write-string "</textarea></label>" out)

        (write-string "<div class=\"actions\">" out)
        (write-string "<button type=\"submit\">Save</button>" out)
        (write-string "<a href=\"/" out)
        (when (not (string=? slug "home"))
          (write-string "p/" out)
          (write-string (html-attr-escape slug) out))
        (write-string "\">Cancel</a>" out)
        (write-string "</div>" out)
        (write-string "</form>" out)
        (html-response
          (render-page req auth
                       (list (cons 'title (string-append "Edit " slug))
                             (cons 'active (if (string=? slug "home") 'home #f))
                             (cons 'body-class "editor"))
                       (get-output-string out)))))

    (define (render-index req auth cfg)
      (let ((rows (list-pages cfg))
            (out  (open-output-string)))
        (out! out "<header class=\"feeds-head\"><h1>Pages</h1></header>"
                  "<form method=\"post\" action=\"/pages\" class=\"grocery-new\">"
                  "<input type=\"text\" name=\"slug\" required "
                  "maxlength=\"40\" pattern=\"[a-z0-9_-]+\" "
                  "placeholder=\"new slug (a-z, 0-9, -, _)\">"
                  "<button type=\"submit\">Create / open</button>"
                  "</form>")
        (cond
          ((null? rows)
           (out! out "<p class=\"empty\">No pages yet.</p>"))
          (else
           (out! out "<ul class=\"grocery-lists\">")
           (for-each
             (lambda (row)
               (let* ((slug (page-field row "slug"))
                      (title (page-field row "title"))
                      (updated (page-field row "updated"))
                      (view-href (cond ((string=? slug "home") "/")
                                       (else (string-append
                                               "/p/"
                                               (html-attr-escape slug))))))
                 (out! out "<li>"
                           "<a class=\"row\" href=\""
                           view-href "\">"
                           "<span class=\"name\">"
                           (html-escape (cond ((string=? title "") slug)
                                              (else title)))
                           "</span>"
                           "<span class=\"meta\">"
                           (html-escape slug)
                           " &middot; updated "
                           (html-escape updated) "</span></a>"
                           " <a class=\"linkish\" href=\"/edit/"
                           (html-attr-escape slug) "\">edit</a>"
                           (cond
                             ((string=? slug "home") "")
                             (else
                              (string-append
                                " <form method=\"post\" action=\"/pages/"
                                (html-attr-escape slug)
                                "/delete\" class=\"inline\" "
                                "data-confirm=\"Delete this page?\">"
                                "<button class=\"linkish danger\">delete</button>"
                                "</form>")))
                           "</li>")))
             rows)
           (out! out "</ul>")))
        (html-response
          (render-page req auth
                       '((title . "Pages")
                         (active . pages))
                       (get-output-string out)))))

    (define (delete-page! cfg slug)
      (with-db cfg
        (lambda (c)
          (pg-exec c
            (string-append
              "DELETE FROM pages WHERE slug = "
              (pg-quote-literal slug))))))

    (define (handle-edit-save req params cfg auth)
      (let* ((slug (params-ref params "slug"))
             (form (parse-www-form (or (http-request-body req) "")))
             (title  (form-ref form "title" ""))
             (format (form-ref form "format" "markdown"))
             (source (form-ref form "source" "")))
        (cond
          ((or (= 0 (string-length (string-trim-both slug)))
               (= 0 (string-length (string-trim-both title))))
           (render-error 400 "Slug and title are required."))
          (else
           (save-page! cfg slug title format source)
           (make-http-response 302
             (list (cons "Location"
                         (cond ((string=? slug "home") "/")
                               (else (string-append "/p/" slug)))))
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
