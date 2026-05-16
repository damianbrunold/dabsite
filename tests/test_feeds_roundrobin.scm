;; Unit tests for round-robin-by-label.

(import (scheme base) (scheme write) (scheme process-context) (scm module))
(module-search-path! (cons "src" (module-search-path)))

(import (damian feeds) (scm test) (srfi 64))

(test-runner-factory scm-test-runner)
(test-begin "feeds-roundrobin")

;; Helper to build a fake row alist with a label and id.
(define (e lab id) (list (cons "label" lab) (cons "id" id)))
(define (labels-of rows) (map (lambda (r) (cdr (assoc "label" r))) rows))
(define (ids-of    rows) (map (lambda (r) (cdr (assoc "id"    r))) rows))

(test-group "even three labels"
  (let* ((in  (list (e "G"  "1") (e "G"  "2") (e "G"  "3")
                    (e "R"  "4") (e "R"  "5") (e "R"  "6")
                    (e "NG" "7") (e "NG" "8") (e "NG" "9")))
         (out (round-robin-by-label in)))
    (test-equal '("G" "R" "NG" "G" "R" "NG" "G" "R" "NG")
                (labels-of out))
    (test-equal '("1" "4" "7" "2" "5" "8" "3" "6" "9")
                (ids-of out))))

(test-group "lopsided: one big, two small"
  (let* ((in  (list (e "G" "1") (e "G" "2") (e "G" "3") (e "G" "4")
                    (e "G" "5") (e "G" "6")
                    (e "R" "7")
                    (e "NG" "8") (e "NG" "9")))
         (out (round-robin-by-label in)))
    ;; First three rounds: G R NG, then G NG, then G alone four times.
    (test-equal '("G" "R" "NG" "G" "NG" "G" "G" "G" "G")
                (labels-of out))
    (test-equal '("1" "7" "8" "2" "9" "3" "4" "5" "6")
                (ids-of out))))

(test-group "preserves first-seen label order"
  ;; Input visits labels in NG/G/R order; round-robin should keep that.
  (let* ((in  (list (e "NG" "1") (e "G" "2") (e "R" "3")
                    (e "NG" "4") (e "G" "5") (e "R" "6")))
         (out (round-robin-by-label in)))
    (test-equal '("NG" "G" "R" "NG" "G" "R") (labels-of out))))

(test-group "single label is a no-op"
  (let ((in (list (e "G" "1") (e "G" "2") (e "G" "3"))))
    (test-equal '("1" "2" "3") (ids-of (round-robin-by-label in)))))

(test-group "empty"
  (test-equal '() (round-robin-by-label '())))

(test-end "feeds-roundrobin")

(exit (if (= 0 (last-run-failed-tests)) 0 1))
