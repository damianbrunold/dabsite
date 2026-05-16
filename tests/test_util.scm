;; Unit tests for (damian util). No DB, no network.

(import (scheme base) (scheme write) (scheme process-context) (scm module))
(module-search-path! (cons "src" (module-search-path)))

(import (damian util)
        (scm test)
        (srfi 64))

(test-runner-factory scm-test-runner)
(test-begin "util")

(test-group "html-escape"
  (test-equal "&lt;a&gt;&amp;&quot;&#39;&lt;/a&gt;"
              (html-escape "<a>&\"'</a>"))
  (test-equal "" (html-escape ""))
  (test-equal "no escape" (html-escape "no escape")))

(test-group "percent encode/decode roundtrip"
  (test-equal "hello world" (percent-decode "hello%20world"))
  (test-equal "hello world" (percent-decode "hello+world"))
  (test-equal "a&b" (percent-decode "a%26b"))
  (test-equal "A=B" (percent-decode "A%3DB"))
  (test-equal "räuber"
              (percent-decode (percent-encode "räuber")))
  (test-equal "abc.-_~" (percent-encode "abc.-_~")))

(test-group "parse-www-form"
  (test-equal '() (parse-www-form ""))
  (test-equal '() (parse-www-form #f))
  (test-equal '(("a" . "1") ("b" . "hi there") ("c" . "x&y"))
              (parse-www-form "a=1&b=hi+there&c=x%26y"))
  (test-equal "1" (form-ref (parse-www-form "a=1") "a"))
  (test-equal "default" (form-ref '() "missing" "default"))
  (test-equal #f (form-ref '() "missing")))

(test-group "cookies"
  (test-equal '() (parse-cookie-header #f))
  (test-equal '() (parse-cookie-header ""))
  (test-equal '(("a" . "1") ("b" . "2"))
              (parse-cookie-header "a=1; b=2"))
  (test-equal '(("a" . "1") ("b" . "hi there"))
              (parse-cookie-header "  a=1 ; b=hi there  "))
  (test-equal "x" (cookie-ref '(("k" . "x")) "k"))
  (test-equal #f  (cookie-ref '() "k")))

(test-group "format-set-cookie"
  (test-equal "n=v; Path=/; Max-Age=60; HttpOnly; SameSite=Strict; Secure"
              (format-set-cookie "n" "v" 60 "/"))
  (test-equal "n=v; Path=/; Max-Age=60; HttpOnly; SameSite=Strict"
              (format-set-cookie "n" "v" 60 "/" 'no-secure)))

(test-group "sql-quote-literal"
  (test-equal "'plain'"  (sql-quote-literal "plain"))
  (test-equal "'O''Brien'"  (sql-quote-literal "O'Brien"))
  ;; Postgres standard_conforming_strings: backslashes are literal.
  (test-equal "'a\\b'"  (sql-quote-literal "a\\b"))
  ;; Cannot inject SQL by closing the quote: the leading ' gets doubled,
  ;; so the closing quote in the input becomes part of the string literal.
  (test-equal "'''); DROP TABLE x;--'"
              (sql-quote-literal "'); DROP TABLE x;--")))

(test-group "constant-time-bv-equal?"
  (test-assert "equal"
               (constant-time-bv-equal? (string->utf8 "abc") (string->utf8 "abc")))
  (test-assert "different last byte"
               (not (constant-time-bv-equal? (string->utf8 "abc") (string->utf8 "abd"))))
  (test-assert "different length"
               (not (constant-time-bv-equal? (string->utf8 "abc") (string->utf8 "abcd")))))

(test-group "hex roundtrip"
  (test-equal "abc"
              (utf8->string (hex->bytevector (bytevector->hex (string->utf8 "abc")))))
  (test-equal "00ff" (bytevector->hex (bytevector 0 255))))

(test-group "parse-duration"
  (test-eqv 30    (parse-duration "30s"))
  (test-eqv 600   (parse-duration "10m"))
  (test-eqv 10800 (parse-duration "3h"))
  (test-eqv 86400 (parse-duration "1d"))
  (test-eqv 3600  (parse-duration "3600"))
  (test-eqv 7200  (parse-duration "  2h  "))
  (test-eqv 0     (parse-duration "0"))
  (test-eqv #f    (parse-duration "abc"))
  (test-eqv #f    (parse-duration ""))
  (test-eqv #f    (parse-duration "-5"))
  (test-eqv #f    (parse-duration "1.5h"))
  (test-eqv #f    (parse-duration #f)))

(test-group "format-duration"
  (test-equal "30s"  (format-duration 30))
  (test-equal "1m"   (format-duration 60))
  (test-equal "1h"   (format-duration 3600))
  (test-equal "1d"   (format-duration 86400))
  (test-equal "2d"   (format-duration 172800))
  (test-equal "90s"  (format-duration 90))
  (test-equal "0s"   (format-duration 0)))

(test-end "util")

(exit (if (= 0 (last-run-failed-tests)) 0 1))
