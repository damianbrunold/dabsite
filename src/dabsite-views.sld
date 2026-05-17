(define-library (dabsite views)
  (import (scheme base)
          (scheme write)
          (scheme cxr)
          (scm html builder)
          (dabsite auth)
          (scm net http response))
  (export render-page
          html-response
          render-error)
  (begin

    ;; ------------------------------------------------------------
    ;; Single shared layout. Pages call (render-page req auth opts body)
    ;; with a piece of body HTML; this wraps it with the same shell so the
    ;; nav, login state and stylesheet are consistent everywhere.
    ;;
    ;; opts is an alist; recognised keys:
    ;;   title         — page <title> (string)
    ;;   active        — symbol identifying the active nav entry: 'home, 'notes
    ;;   body-class    — extra class on <body>
    ;;   lang          — BCP 47 language tag for <html lang="…">; falls
    ;;                   back to (site-lang). Use this on pages whose
    ;;                   primary text differs from the site default.
    ;;
    ;; Pages that still build their body the old way pass a string of
    ;; pre-rendered HTML and we splice it in via (raw …). New pages
    ;; should build SXML directly and pass that through; render-page
    ;; accepts either.
    ;; ------------------------------------------------------------

    (define (opt-ref opts key default)
      (let ((p (assq key opts)))
        (if p (cdr p) default)))

    ;; Nav rendering ----------------------------------------------

    (define nav-targets
      ;; (path label active-key)
      '(("/"          "Home"    home)
        ("/pages"     "Pages"   pages)
        ("/feeds"     "Feeds"   feeds)
        ("/notes"     "Notes"   notes)
        ("/shortener" "Links"   shortener)
        ("/polls"     "Polls"   polls)
        ("/tracker"   "Tracker" tracker)
        ("/files"     "Files"   files)
        ("/grocery"   "Grocery" grocery)))

    (define (nav-link-sxml entry active)
      (let ((href (car entry))
            (lbl  (cadr entry))
            (key  (caddr entry)))
        `(li (a (@ (href ,href)
                   (class ,(if (eq? key active) "active" #f)))
                ,lbl))))

    ;; Inline SVGs for the theme toggle. They're static and complex;
    ;; (raw …) is the right tool, marking them as trusted output rather
    ;; than user content.
    (define theme-sun-svg
      (raw (string-append
             "<svg class=\"icon-sun\" viewBox=\"0 0 24 24\" width=\"18\" height=\"18\" "
             "fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" "
             "stroke-linecap=\"round\" stroke-linejoin=\"round\" aria-hidden=\"true\">"
             "<circle cx=\"12\" cy=\"12\" r=\"4\"/>"
             "<path d=\"M12 2v2M12 20v2M4.93 4.93l1.41 1.41M17.66 17.66l1.41 1.41"
             "M2 12h2M20 12h2M4.93 19.07l1.41-1.41M17.66 6.34l1.41-1.41\"/>"
             "</svg>")))

    (define theme-moon-svg
      (raw (string-append
             "<svg class=\"icon-moon\" viewBox=\"0 0 24 24\" width=\"18\" height=\"18\" "
             "fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" "
             "stroke-linecap=\"round\" stroke-linejoin=\"round\" aria-hidden=\"true\">"
             "<path d=\"M21 12.79A9 9 0 1 1 11.21 3 7 7 0 0 0 21 12.79z\"/>"
             "</svg>")))

    ;; The mobile nav-toggle is a pure-CSS pattern: a hidden checkbox
    ;; toggles the sibling list via :checked + general-sibling.
    (define hamburger-sxml
      `((input (@ (type "checkbox") (id "nav-toggle") (class "nav-toggle")))
        (label (@ (for "nav-toggle") (class "nav-hamburger")
                  (aria-label "Menu") (title "Menu"))
          (span (@ (class "hamburger") (aria-hidden "true"))
            (span) (span) (span)))))

    (define (nav-sxml req auth active)
      (let ((authed (authed? auth req)))
        `(nav (@ (class "top"))
              (div (@ (class "brand"))
                   (a (@ (href "/"))
                      (span "Damian") " " (span "Brunold")))
              ;; Authed nav: hamburger + link list. Unauthed view has
              ;; no other destinations, so the link list is omitted.
              ,@(if authed
                    `(,@hamburger-sxml
                      (ul (@ (class "links"))
                          ,@(map (lambda (e) (nav-link-sxml e active))
                                 nav-targets)))
                    '())
              ;; Theme toggle: shows whichever icon represents what
              ;; clicking will switch to; CSS swaps based on the
              ;; data-theme attribute that site.js maintains on <html>.
              (button (@ (id "theme-toggle") (type "button")
                         (class "theme-toggle")
                         (aria-label "Toggle dark mode")
                         (title "Toggle dark mode"))
                ,theme-sun-svg
                ,theme-moon-svg)
              (div (@ (class "auth"))
                ,(cond
                   (authed
                    `(form (@ (method "post") (action "/logout"))
                       (button (@ (type "submit") (class "linkish"))
                         "Log out")))
                   (else
                    `(a (@ (href "/login")) "Log in")))))))

    ;; Page shell -------------------------------------------------

    (define (render-page req auth opts body-html)
      (let* ((title      (opt-ref opts 'title "Damian Brunold"))
             (active     (opt-ref opts 'active 'home))
             (body-class (opt-ref opts 'body-class #f))
             (lang       (opt-ref opts 'lang (site-lang)))
             ;; body-html is either a string (legacy, from pages still
             ;; building to a port) or an SXML tree (new style). We
             ;; wrap strings in (raw …) so they pass through unescaped.
             (body-node  (cond
                           ((string? body-html) (raw body-html))
                           (else                body-html))))
        (string-append
         (html->string
          (html5
            `(@ (lang ,lang))
            `(head
               (meta (@ (charset "utf-8")))
               (meta (@ (name "viewport")
                        (content "width=device-width, initial-scale=1")))
               (title ,title)
               (link (@ (rel "icon") (type "image/png") (href "/static/icon.png")))
               ;; theme.js runs synchronously so the chosen theme is
               ;; applied before first paint. Both scripts are same-
               ;; origin so the strict CSP (script-src 'self') is happy.
               (script (@ (src "/static/theme.js")))
               (link (@ (rel "stylesheet") (href "/static/site.css")))
               (script (@ (src "/static/site.js") (defer #t))))
            `(body (@ (class ,body-class))
              ,(nav-sxml req auth active)
              (main ,body-node))))
         "\n")))

    (define (html-response body)
      (make-http-response 200
        '(("Content-Type" . "text/html; charset=utf-8"))
        body))

    (define (render-error status msg)
      (make-http-response status
        '(("Content-Type" . "text/html; charset=utf-8"))
        (html->string
          (html5
            `(head (meta (@ (charset "utf-8")))
                   (title "Error")
                   (link (@ (rel "stylesheet") (href "/static/site.css"))))
            `(body (main (h1 ,(number->string status))
                         (p ,msg)))))))

))
