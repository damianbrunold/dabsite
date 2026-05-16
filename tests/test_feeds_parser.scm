;; Unit tests for (dabsite feeds-parser). Uses XML fixtures next to this file.

(import (scheme base) (scheme write) (scheme process-context) (scm module))
(module-search-path! (cons "src" (module-search-path)))

(import (dabsite feeds-parser) (scm test) (srfi 64))

(test-runner-factory scm-test-runner)
(test-begin "feeds-parser")

(define (entry-field e k) (cdr (assoc k e)))

(test-group "local-name"
  (test-equal "title"  (local-name "title"))
  (test-equal "title"  (local-name "atom:Title"))
  (test-equal "entry"  (local-name "ns:ENTRY")))

(test-group "RSS"
  (let* ((parsed (parse-feed-file "tests/fixtures/sample.rss"))
         (title  (car parsed))
         (es     (cdr parsed)))
    (test-equal "Demo Feed" title)
    (test-eqv 2 (length es))
    (let ((e1 (car es)))
      (test-equal "First post"          (entry-field e1 "title"))
      (test-equal "https://example.com/1" (entry-field e1 "link"))
      (test-equal "post-1"              (entry-field e1 "guid"))
      (test-equal "Hello & goodbye"     (entry-field e1 "summary")))
    (let ((e2 (cadr es)))
      (test-equal "Second post"         (entry-field e2 "title"))
      (test-equal "post-2"              (entry-field e2 "guid")))))

(test-group "Atom"
  (let* ((parsed (parse-feed-file "tests/fixtures/sample.atom"))
         (title  (car parsed))
         (es     (cdr parsed)))
    (test-equal "Demo Atom" title)
    (test-eqv 2 (length es))
    (let ((e1 (car es)))
      (test-equal "Atom one"             (entry-field e1 "title"))
      (test-equal "https://example.com/a/1" (entry-field e1 "link"))
      (test-equal "urn:atom:demo:1"      (entry-field e1 "guid"))
      (test-equal "Atom summary one"     (entry-field e1 "summary"))
      (test-equal "2024-05-16T12:34:56Z" (entry-field e1 "published")))
    (let ((e2 (cadr es)))
      (test-equal "<p>Body 2</p>"        (entry-field e2 "summary"))
      (test-equal "2024-05-17T08:00:00+02:00" (entry-field e2 "published")))))

(test-group "parse-pubdate"
  ;; ISO: 2024-05-16T12:34:56Z = 1715862896
  (test-eqv 1715862896 (parse-pubdate "2024-05-16T12:34:56Z"))
  ;; ISO with +02:00 offset
  (test-eqv 1715855696 (parse-pubdate "2024-05-16T12:34:56+02:00"))
  ;; RFC 822 with numeric tz
  (test-eqv 1715855696 (parse-pubdate "Thu, 16 May 2024 12:34:56 +0200"))
  ;; RFC 822 with GMT
  (test-eqv 1715932800 (parse-pubdate "Fri, 17 May 2024 08:00:00 GMT"))
  ;; bogus → #f
  (test-eqv #f (parse-pubdate "not a date"))
  (test-eqv #f (parse-pubdate "")))

(test-end "feeds-parser")

(exit (if (= 0 (last-run-failed-tests)) 0 1))
