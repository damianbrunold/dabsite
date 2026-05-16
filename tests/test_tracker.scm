;; Unit tests for (damian tracker) — pure-scheme helpers only.

(import (scheme base) (scheme write) (scheme process-context) (scm module))
(module-search-path! (cons "src" (module-search-path)))

(import (damian tracker) (scm test) (srfi 64))

(test-runner-factory scm-test-runner)
(test-begin "tracker")

(test-group "format-minutes"
  (test-equal "0:00h"  (format-minutes 0))
  (test-equal "0:05h"  (format-minutes 5))
  (test-equal "0:45h"  (format-minutes 45))
  (test-equal "1:00h"  (format-minutes 60))
  (test-equal "1:30h"  (format-minutes 90))
  (test-equal "10:00h" (format-minutes 600))
  (test-equal "100:30h" (format-minutes 6030)))

(test-group "parse-minutes"
  (test-eqv 0     (parse-minutes "0"))
  (test-eqv 90    (parse-minutes "90"))
  (test-eqv 90    (parse-minutes "1:30"))
  (test-eqv 60    (parse-minutes "1:00"))
  (test-eqv 60    (parse-minutes "1h"))
  (test-eqv 90    (parse-minutes "1h30"))
  (test-eqv 45    (parse-minutes "45m"))
  (test-eqv 90    (parse-minutes "  1:30  "))
  (test-eqv 90    (parse-minutes "1.5h"))
  (test-eqv 30    (parse-minutes "0.5h"))
  (test-eqv 135   (parse-minutes "2.25h"))
  ;; rejections
  (test-eqv #f    (parse-minutes ""))
  (test-eqv #f    (parse-minutes "abc"))
  (test-eqv #f    (parse-minutes "-5"))
  (test-eqv #f    (parse-minutes "1:60"))   ; minutes ≥ 60 in H:MM
  (test-eqv #f    (parse-minutes "1.5h30"))
  (test-eqv #f    (parse-minutes #f)))

(test-group "csv-escape"
  (test-equal "plain" (csv-escape "plain"))
  (test-equal "\"a,b\"" (csv-escape "a,b"))
  (test-equal "\"a\"\"b\"" (csv-escape "a\"b"))
  (test-equal "\"line1\nline2\"" (csv-escape "line1\nline2"))
  (test-equal "" (csv-escape "")))

(test-group "csv-row"
  (test-equal "a,b,c\r\n" (csv-row '("a" "b" "c")))
  (test-equal "\"a,1\",\"b\"\"2\",c\r\n"
              (csv-row '("a,1" "b\"2" "c")))
  (test-equal "\r\n" (csv-row '())))

(test-group "split-topics"
  (test-equal '() (split-topics ""))
  (test-equal '() (split-topics "   "))
  (test-equal '("one") (split-topics "one"))
  (test-equal '("one" "two") (split-topics "one,two"))
  (test-equal '("one" "two") (split-topics "  one ,  two  "))
  (test-equal '("Two Words" "another")
              (split-topics "Two Words, another"))
  ;; empties collapsed
  (test-equal '("a" "b") (split-topics ", a, ,b,")))

(test-end "tracker")

(exit (if (= 0 (last-run-failed-tests)) 0 1))
