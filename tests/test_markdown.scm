;; Unit tests for (damian markdown).

(import (scheme base) (scheme write) (scheme process-context)
        (srfi 13) (scm module))
(module-search-path! (cons "src" (module-search-path)))

(import (damian markdown) (scm test) (srfi 64))

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
    (test-assert "li two" (contains? h "<li>two</li>"))))

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
