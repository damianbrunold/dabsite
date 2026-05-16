(define-library (dabsite markdown)
  (import (scheme base)
          (scheme write)
          (scheme char)
          (srfi 13)
          (scm html))
  (export render-markdown)
  (begin

    ;; ------------------------------------------------------------
    ;; Tiny markdown subset.
    ;;
    ;; Block:
    ;;   # H1, ## H2, ### H3
    ;;   ```lang  fenced code block  ```
    ;;   - bullet list item   (consecutive lines form one <ul>)
    ;;   blank line separates paragraphs
    ;;   everything else is a paragraph
    ;;
    ;; Inline (in non-code text only):
    ;;   **bold**  *italic*  `code`  [text](url)
    ;;
    ;; Anything else is rendered as escaped text. HTML in the source is
    ;; escaped, never passed through. That's intentional — content editing
    ;; is restricted to the logged-in admin, but we still don't want a
    ;; copy-pasted snippet to break the page.
    ;; ------------------------------------------------------------

    (define (split-lines s)
      ;; Splits on \n; drops trailing \r if present. Preserves empty lines.
      (let* ((n (string-length s))
             (acc '()))
        (let loop ((i 0) (start 0))
          (cond
            ((= i n)
             (reverse (cons (substring s start n) acc)))
            ((char=? (string-ref s i) #\newline)
             (let* ((end (if (and (> i start)
                                  (char=? (string-ref s (- i 1)) #\return))
                             (- i 1)
                             i))
                    (line (substring s start end)))
               (set! acc (cons line acc))
               (loop (+ i 1) (+ i 1))))
            (else (loop (+ i 1) start))))))

    ;; --- Inline rendering ---

    (define (write-escaped s out)
      (write-string (html-escape s) out))

    (define (find-char-from s start ch)
      (let ((n (string-length s)))
        (let loop ((i start))
          (cond
            ((= i n) #f)
            ((char=? (string-ref s i) ch) i)
            (else (loop (+ i 1)))))))

    (define (find-substring-from s start needle)
      (let* ((sn (string-length s))
             (nn (string-length needle)))
        (let loop ((i start))
          (cond
            ((> (+ i nn) sn) #f)
            ((string=? (substring s i (+ i nn)) needle) i)
            (else (loop (+ i 1)))))))

    (define (safe-link-url? url)
      ;; Allow http(s), mailto, and same-origin paths/anchors/queries.
      ;; Anything else (javascript:, data:, vbscript:, ...) gets the
      ;; link suppressed and the bracketed text rendered as plain text.
      (let* ((trimmed (string-trim url))
             (n (string-length trimmed)))
        (cond
          ((= n 0) #f)
          ((or (string-prefix? "http://"   trimmed)
               (string-prefix? "https://"  trimmed)
               (string-prefix? "mailto:"   trimmed)
               (string-prefix? "/" trimmed)
               (string-prefix? "#" trimmed)
               (string-prefix? "?" trimmed))
           #t)
          (else #f))))

    (define (render-inline s out)
      ;; Single left-to-right pass. The recogniser order is: backtick code,
      ;; then **bold**, then *italic*, then [text](url). Everything else
      ;; is escaped literal text.
      (let ((n (string-length s)))
        (let loop ((i 0))
          (cond
            ((= i n) #t)

            ;; `code`
            ((char=? (string-ref s i) #\`)
             (let ((end (find-char-from s (+ i 1) #\`)))
               (cond
                 (end
                  (write-string "<code>" out)
                  (write-escaped (substring s (+ i 1) end) out)
                  (write-string "</code>" out)
                  (loop (+ end 1)))
                 (else
                  (write-escaped (substring s i (+ i 1)) out)
                  (loop (+ i 1))))))

            ;; **bold**
            ((and (<= (+ i 2) n)
                  (char=? (string-ref s i) #\*)
                  (char=? (string-ref s (+ i 1)) #\*))
             (let ((end (find-substring-from s (+ i 2) "**")))
               (cond
                 (end
                  (write-string "<strong>" out)
                  (render-inline (substring s (+ i 2) end) out)
                  (write-string "</strong>" out)
                  (loop (+ end 2)))
                 (else
                  (write-escaped (substring s i (+ i 1)) out)
                  (loop (+ i 1))))))

            ;; *italic*
            ((char=? (string-ref s i) #\*)
             (let ((end (find-char-from s (+ i 1) #\*)))
               (cond
                 ((and end (> end (+ i 1)))
                  (write-string "<em>" out)
                  (render-inline (substring s (+ i 1) end) out)
                  (write-string "</em>" out)
                  (loop (+ end 1)))
                 (else
                  (write-escaped (substring s i (+ i 1)) out)
                  (loop (+ i 1))))))

            ;; [text](url)
            ((char=? (string-ref s i) #\[)
             (let* ((rb (find-char-from s (+ i 1) #\])))
               (cond
                 ((and rb (< (+ rb 1) n)
                       (char=? (string-ref s (+ rb 1)) #\())
                  (let ((rp (find-char-from s (+ rb 2) #\))))
                    (cond
                      (rp
                       (let ((text (substring s (+ i 1) rb))
                             (url  (substring s (+ rb 2) rp)))
                         ;; Only emit the href when the URL has a safe
                         ;; scheme. Reject javascript:, data:, vbscript:
                         ;; and the like — they'd otherwise allow
                         ;; markdown content to execute scripts at our
                         ;; origin.
                         (cond
                           ((safe-link-url? url)
                            (write-string "<a href=\"" out)
                            (write-string (html-attr-escape url) out)
                            (write-string "\">" out)
                            (render-inline text out)
                            (write-string "</a>" out))
                           (else (render-inline text out)))
                         (loop (+ rp 1))))
                      (else
                       (write-escaped (substring s i (+ i 1)) out)
                       (loop (+ i 1))))))
                 (else
                  (write-escaped (substring s i (+ i 1)) out)
                  (loop (+ i 1))))))

            (else
             (write-escaped (substring s i (+ i 1)) out)
             (loop (+ i 1)))))))

    ;; --- Block rendering ---

    (define (heading-prefix line)
      ;; Returns (level . text) or #f. Level is 1..3.
      (cond
        ((and (>= (string-length line) 4)
              (string=? (substring line 0 4) "### ")) (cons 3 (substring line 4 (string-length line))))
        ((and (>= (string-length line) 3)
              (string=? (substring line 0 3) "## "))  (cons 2 (substring line 3 (string-length line))))
        ((and (>= (string-length line) 2)
              (string=? (substring line 0 2) "# "))   (cons 1 (substring line 2 (string-length line))))
        (else #f)))

    (define (bullet-line? line)
      (and (>= (string-length line) 2)
           (char=? (string-ref line 0) #\-)
           (char=? (string-ref line 1) #\space)))

    (define (fence-line? line)
      (and (>= (string-length line) 3)
           (string=? (substring line 0 3) "```")))

    (define (blank-line? line)
      (string=? (string-trim-both line) ""))

    (define (render-paragraph lines out)
      ;; Joins lines with single space, renders inline.
      (write-string "<p>" out)
      (let loop ((ls lines) (first? #t))
        (cond
          ((null? ls) #t)
          (else
           (when (not first?) (write-char #\space out))
           (render-inline (car ls) out)
           (loop (cdr ls) #f))))
      (write-string "</p>\n" out))

    (define (render-list items out)
      (write-string "<ul>\n" out)
      (for-each
        (lambda (line)
          (write-string "  <li>" out)
          (render-inline (substring line 2 (string-length line)) out)
          (write-string "</li>\n" out))
        items)
      (write-string "</ul>\n" out))

    (define (render-code-block lines out)
      (write-string "<pre><code>" out)
      (let loop ((ls lines) (first? #t))
        (cond
          ((null? ls) #t)
          (else
           (when (not first?) (write-char #\newline out))
           (write-escaped (car ls) out)
           (loop (cdr ls) #f))))
      (write-string "</code></pre>\n" out))

    (define (render-heading level text out)
      (let ((tag (cond ((= level 1) "h1")
                       ((= level 2) "h2")
                       (else "h3"))))
        (write-string "<" out) (write-string tag out) (write-string ">" out)
        (render-inline text out)
        (write-string "</" out) (write-string tag out) (write-string ">\n" out)))

    (define (process-blocks lines out)
      (let loop ((ls lines))
        (cond
          ((null? ls) #t)
          ((blank-line? (car ls)) (loop (cdr ls)))
          ((fence-line? (car ls))
           ;; Consume until matching closing fence.
           (let collect ((rest (cdr ls)) (buf '()))
             (cond
               ((null? rest)
                (render-code-block (reverse buf) out)
                (loop '()))
               ((fence-line? (car rest))
                (render-code-block (reverse buf) out)
                (loop (cdr rest)))
               (else (collect (cdr rest) (cons (car rest) buf))))))
          ((heading-prefix (car ls))
           (let ((h (heading-prefix (car ls))))
             (render-heading (car h) (cdr h) out)
             (loop (cdr ls))))
          ((bullet-line? (car ls))
           (let collect ((rest ls) (buf '()))
             (cond
               ((or (null? rest) (not (bullet-line? (car rest))))
                (render-list (reverse buf) out)
                (loop rest))
               (else (collect (cdr rest) (cons (car rest) buf))))))
          (else
           ;; Paragraph: consume until blank, heading, fence, or list.
           (let collect ((rest ls) (buf '()))
             (cond
               ((or (null? rest)
                    (blank-line? (car rest))
                    (heading-prefix (car rest))
                    (fence-line? (car rest))
                    (bullet-line? (car rest)))
                (render-paragraph (reverse buf) out)
                (loop rest))
               (else (collect (cdr rest) (cons (car rest) buf)))))))))

    (define (render-markdown source)
      (let ((out (open-output-string)))
        (process-blocks (split-lines source) out)
        (get-output-string out)))

))
