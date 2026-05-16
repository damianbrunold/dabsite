(define-library (dabsite views)
  (import (scheme base)
          (scheme write)
          (scm html)
          (dabsite auth)
          (scm net http response))
  (export render-page
          html-response
          render-error
          out!)
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
    ;; ------------------------------------------------------------

    ;; Defensive helper. write-string takes optional start/end indexes
    ;; AFTER the port, so (write-string "a" "b" port) silently misuses
    ;; "b" as the port. out! accepts any number of string fragments and
    ;; emits them in order to the given port.
    (define (out! port . strings)
      (for-each (lambda (s) (write-string s port)) strings))

    (define (opt-ref opts key default)
      (let ((p (assq key opts)))
        (if p (cdr p) default)))

    (define (nav-link out href label active? extra-class)
      (write-string "<a href=\"" out)
      (write-string (html-attr-escape href) out)
      (write-string "\"" out)
      (when active?
        (write-string " class=\"active\"" out))
      (when extra-class
        (when (not active?) (write-string " class=\"" out))
        (when active?      (write-string " class=\"active " out))
        (write-string extra-class out)
        (write-string "\"" out))
      (write-string ">" out)
      (write-string (html-escape label) out)
      (write-string "</a>" out))

    (define (render-nav req auth active out)
      (let ((authed (authed? auth req)))
        (write-string "<nav class=\"top\">" out)
        (write-string
          (string-append
            "<div class=\"brand\"><a href=\"/\">"
            "<span>Damian</span> <span>Brunold</span></a></div>")
          out)
        ;; Authed nav: an invisible checkbox + label[for] gives a pure-CSS
        ;; toggle on mobile (input is :checked → sibling ul becomes
        ;; visible via the general-sibling combinator). On desktop CSS
        ;; hides the hamburger and forces the ul to always display.
        ;;
        ;; Unauthed view has no other destinations than the page already
        ;; on screen, so the link list is omitted entirely.
        (when authed
          (write-string
            (string-append
              "<input type=\"checkbox\" id=\"nav-toggle\" class=\"nav-toggle\">"
              "<label for=\"nav-toggle\" class=\"nav-hamburger\" "
              "aria-label=\"Menu\" title=\"Menu\">"
              "<span class=\"hamburger\" aria-hidden=\"true\">"
              "<span></span><span></span><span></span></span>"
              "</label>")
            out)
          (write-string "<ul class=\"links\">" out)
          (write-string "<li>" out)
          (nav-link out "/" "Home" (eq? active 'home) #f)
          (write-string "</li>" out)
          (write-string "<li>" out)
          (nav-link out "/pages" "Pages" (eq? active 'pages) #f)
          (write-string "</li>"  out)
          (write-string "<li>" out)
          (nav-link out "/feeds" "Feeds" (eq? active 'feeds) #f)
          (write-string "</li>"  out)
          (write-string "<li>" out)
          (nav-link out "/notes" "Notes" (eq? active 'notes) #f)
          (write-string "</li>"  out)
          (write-string "<li>" out)
          (nav-link out "/shortener" "Links" (eq? active 'shortener) #f)
          (write-string "</li>"  out)
          (write-string "<li>" out)
          (nav-link out "/polls" "Polls" (eq? active 'polls) #f)
          (write-string "</li>"  out)
          (write-string "<li>" out)
          (nav-link out "/tracker" "Tracker" (eq? active 'tracker) #f)
          (write-string "</li>"  out)
          (write-string "<li>" out)
          (nav-link out "/files" "Files" (eq? active 'files) #f)
          (write-string "</li>"  out)
          (write-string "<li>" out)
          (nav-link out "/grocery" "Grocery" (eq? active 'grocery) #f)
          (write-string "</li>"  out)
          (write-string "</ul>" out))

        ;; Theme toggle: clicked once → explicit light/dark, persisted in
        ;; localStorage. The button shows whichever icon represents what
        ;; clicking will switch *to* — CSS swaps the visible icon based on
        ;; the data-theme attribute that site.js maintains on <html>.
        (write-string "<button id=\"theme-toggle\" type=\"button\" class=\"theme-toggle\" " out)
        (write-string "aria-label=\"Toggle dark mode\" title=\"Toggle dark mode\">" out)
        ;; sun (shown in dark mode → click to go light)
        (write-string (string-append
          "<svg class=\"icon-sun\" viewBox=\"0 0 24 24\" width=\"18\" height=\"18\" "
          "fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" "
          "stroke-linecap=\"round\" stroke-linejoin=\"round\" aria-hidden=\"true\">"
          "<circle cx=\"12\" cy=\"12\" r=\"4\"/>"
          "<path d=\"M12 2v2M12 20v2M4.93 4.93l1.41 1.41M17.66 17.66l1.41 1.41"
          "M2 12h2M20 12h2M4.93 19.07l1.41-1.41M17.66 6.34l1.41-1.41\"/>"
          "</svg>")
          out)
        ;; moon (shown in light mode → click to go dark)
        (write-string (string-append
          "<svg class=\"icon-moon\" viewBox=\"0 0 24 24\" width=\"18\" height=\"18\" "
          "fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" "
          "stroke-linecap=\"round\" stroke-linejoin=\"round\" aria-hidden=\"true\">"
          "<path d=\"M21 12.79A9 9 0 1 1 11.21 3 7 7 0 0 0 21 12.79z\"/>"
          "</svg>")
          out)
        (write-string "</button>" out)

        (write-string "<div class=\"auth\">" out)
        (cond
          (authed
           (write-string "<form method=\"post\" action=\"/logout\">" out)
           (write-string "<button type=\"submit\" class=\"linkish\">Log out</button>" out)
           (write-string "</form>" out))
          (else
           (write-string "<a href=\"/login\">Log in</a>" out)))
        (write-string "</div>" out)
        (write-string "</nav>" out)))

    (define (render-page req auth opts body-html)
      (let* ((title      (opt-ref opts 'title "Damian Brunold"))
             (active     (opt-ref opts 'active 'home))
             (body-class (opt-ref opts 'body-class #f))
             (out        (open-output-string)))
        (write-string "<!doctype html>\n" out)
        (write-string "<html lang=\"en\"><head>" out)
        (write-string "<meta charset=\"utf-8\">" out)
        (write-string "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">" out)
        (write-string "<title>" out)
        (write-string (html-escape title) out)
        (write-string "</title>" out)
        (write-string "<link rel=\"icon\" type=\"image/png\" href=\"/static/icon.png\">" out)
        ;; theme.js runs synchronously so the chosen theme is applied
        ;; before first paint; both scripts come from the same origin so
        ;; the strict CSP (script-src 'self') is satisfied.
        (write-string "<script src=\"/static/theme.js\"></script>" out)
        (write-string "<link rel=\"stylesheet\" href=\"/static/site.css\">" out)
        (write-string "<script src=\"/static/site.js\" defer></script>" out)
        (write-string "</head><body" out)
        (when body-class
          (write-string " class=\"" out)
          (write-string (html-attr-escape body-class) out)
          (write-string "\"" out))
        (write-string ">" out)

        (render-nav req auth active out)

        (write-string "<main>" out)
        (write-string body-html out)
        (write-string "</main>" out)

        (write-string "</body></html>\n" out)
        (get-output-string out)))

    (define (html-response body)
      (make-http-response 200
        '(("Content-Type" . "text/html; charset=utf-8"))
        body))

    (define (render-error status msg)
      (make-http-response status
        '(("Content-Type" . "text/html; charset=utf-8"))
        (string-append
          "<!doctype html><html><head><meta charset=\"utf-8\">"
          "<title>Error</title>"
          "<link rel=\"stylesheet\" href=\"/static/site.css\"></head>"
          "<body><main><h1>" (number->string status) "</h1><p>"
          (html-escape msg) "</p></main></body></html>")))

))
