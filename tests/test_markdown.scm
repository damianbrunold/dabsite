;; Integration tests: dabsite renders Markdown via the (scm markdown)
;; standard-library renderer. These assertions guard the behaviour dabsite
;; relies on — escaping, safe link schemes, lists (incl. nesting).

(import (scheme base) (scheme write) (scheme process-context)
        (srfi 13))

(import (scm markdown) (scm test) (srfi 64))

;; dabsite historically called this `render-markdown`; keep the alias so the
;; assertions read against the stable name.
(define render-markdown markdown->html)

(test-runner-factory scm-test-runner)
(test-begin "markdown")

(define (contains? s needle) (not (not (string-contains s needle))))

(test-group "headings"
  (test-assert "h1" (contains? (render-markdown "# Hello") "<h1>Hello</h1>"))
  (test-assert "h2" (contains? (render-markdown "## Sub") "<h2>Sub</h2>"))
  (test-assert "h3" (contains? (render-markdown "### S3") "<h3>S3</h3>")))

(test-group "inline"
  (test-assert "bold"   (contains? (render-markdown "a **b** c") "<strong>b</strong>"))
  (test-assert "italic" (contains? (render-markdown "a *b* c")   "<em>b</em>"))
  (test-assert "code"   (contains? (render-markdown "a `b` c")   "<code>b</code>"))
  (test-assert "link http"  (contains? (render-markdown "[t](http://x)")
                                       "<a href=\"http://x\">t</a>"))
  (test-assert "link path"  (contains? (render-markdown "[t](/a)")
                                       "<a href=\"/a\">t</a>"))
  (test-assert "javascript url dropped"
               (not (contains? (render-markdown "[t](javascript:alert(1))")
                               "<a")))
  (test-assert "javascript text preserved"
               (contains? (render-markdown "[t](javascript:alert(1))") "t"))
  (test-assert "data url dropped"
               (not (contains? (render-markdown "[t](data:text/html,x)") "<a"))))

(test-group "lists"
  (let ((h (render-markdown "- one\n- two\n- three\n")))
    (test-assert "<ul>"   (contains? h "<ul>"))
    (test-assert "li one" (contains? h "<li>one</li>"))
    (test-assert "li two" (contains? h "<li>two</li>")))
  (let ((h (render-markdown "1. one\n2. two\n3. three\n")))
    (test-assert "<ol>"   (contains? h "<ol>"))
    (test-assert "ol li one" (contains? h "<li>one</li>"))
    (test-assert "ol li two" (contains? h "<li>two</li>"))
    (test-assert "no ul"  (not (contains? h "<ul>"))))
  ;; '*' and '+' bullet markers are accepted (not just '-')
  (let ((h (render-markdown "* a\n+ b\n")))
    (test-assert "star/plus bullets" (contains? h "<li>a</li>")))
  ;; nested lists render a sub-<ul> inside the parent <li>
  (let ((h (render-markdown "- a\n  - b\n  - c\n- d\n")))
    (test-assert "nested ul" (contains? h "<li>a\n<ul>"))
    (test-assert "nested item" (contains? h "<li>b</li>"))))

(test-group "paragraphs"
  (let ((h (render-markdown "first line\nsecond line\n\nnext para\n")))
    (test-assert "p1" (contains? h "<p>first line second line</p>"))
    (test-assert "p2" (contains? h "<p>next para</p>"))))

(test-group "fenced code"
  (let ((h (render-markdown "```\nint x = 1;\nprintln(x);\n```\n")))
    (test-assert "pre/code" (contains? h "<pre><code>"))
    (test-assert "contents" (contains? h "println(x);"))))

(test-group "html escaping"
  ;; Raw HTML must be escaped — even in paragraphs.
  (let ((h (render-markdown "hello <script>alert(1)</script>")))
    (test-assert "no raw script" (not (contains? h "<script>")))
    (test-assert "escaped"       (contains? h "&lt;script&gt;"))))

(test-end "markdown")

(exit (if (= 0 (last-run-failed-tests)) 0 1))
