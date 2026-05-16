;; Unit tests for (dabsite polls) — pure-scheme helpers only.

(import (scheme base) (scheme write) (scheme cxr) (scheme process-context) (scm module))
(module-search-path! (cons "src" (module-search-path)))

(import (dabsite polls) (scm test) (srfi 64))

(test-runner-factory scm-test-runner)
(test-begin "polls")

(test-group "valid-slug?"
  (test-assert "lowercase"   (valid-slug? "pizza"))
  (test-assert "digits"      (valid-slug? "pizza-2024"))
  (test-assert "underscore"  (valid-slug? "my_poll"))
  (test-assert "three words" (valid-slug? "amber-jade-pine"))
  (test-assert "single char" (valid-slug? "x"))
  (test-assert "empty rejected"        (not (valid-slug? "")))
  (test-assert "uppercase rejected"    (not (valid-slug? "Pizza")))
  (test-assert "space rejected"        (not (valid-slug? "my poll")))
  (test-assert "slash rejected"        (not (valid-slug? "a/b")))
  (test-assert "65 chars rejected"
    (not (valid-slug? (make-string 65 #\a)))))

(test-group "random-slug"
  (let ((s (random-slug)))
    (test-assert "random-slug passes valid-slug?" (valid-slug? s))
    ;; word-word-word: two dashes
    (let loop ((i 0) (dashes 0))
      (cond
        ((= i (string-length s))
         (test-eqv 2 dashes))
        ((char=? (string-ref s i) #\-)
         (loop (+ i 1) (+ dashes 1)))
        (else (loop (+ i 1) dashes))))))

(test-group "tally"
  ;; Three options. Six rows in choices (two voters, three each).
  (let* ((opts '("1" "2" "3"))
         (choices '(("1" . "yes")   ("2" . "maybe") ("3" . "no")
                    ("1" . "yes")   ("2" . "yes")   ("3" . "maybe")))
         (t (tally opts choices)))
    ;; option 1: 2 yes
    (test-equal '(2 0 0) (car t))
    ;; option 2: 1 yes 1 maybe
    (test-equal '(1 1 0) (cadr t))
    ;; option 3: 1 no 1 maybe
    (test-equal '(0 1 1) (caddr t))))

(test-group "tally with no choices"
  (test-equal '((0 0 0) (0 0 0)) (tally '("1" "2") '())))

(test-end "polls")

(exit (if (= 0 (last-run-failed-tests)) 0 1))
