;; Unit tests for (damian shortener) — pure-scheme helpers only.

(import (scheme base) (scheme write) (scheme process-context) (scm module))
(module-search-path! (cons "src" (module-search-path)))

(import (damian shortener) (scm test) (srfi 64))

(test-runner-factory scm-test-runner)
(test-begin "shortener")

(test-group "valid-code?"
  (test-assert "alphanumeric"   (valid-code? "abc123"))
  (test-assert "dash"           (valid-code? "my-link"))
  (test-assert "underscore"     (valid-code? "my_link"))
  (test-assert "single char"    (valid-code? "x"))
  (test-assert "32 chars"
    (valid-code? "0123456789abcdefghij0123456789ab"))
  (test-assert "empty rejected"     (not (valid-code? "")))
  (test-assert "33 chars rejected"
    (not (valid-code? "0123456789abcdefghij0123456789abc")))
  (test-assert "slash rejected"     (not (valid-code? "ab/cd")))
  (test-assert "space rejected"     (not (valid-code? "ab cd")))
  (test-assert "dot rejected"       (not (valid-code? "ab.cd")))
  (test-assert "ampersand rejected" (not (valid-code? "a&b"))))

(test-group "random-code"
  (let ((c (random-code)))
    (test-eqv 6 (string-length c))
    (test-assert "passes valid-code?" (valid-code? c)))
  ;; 100 random codes should all be unique (collision probability is
  ;; ~100*100/2/62^6 ~= 8e-8) and all valid.
  (let loop ((i 0) (seen '()))
    (cond
      ((= i 100)
       (test-eqv 100 (length seen)))
      (else
       (let ((c (random-code)))
         (cond
           ((member c seen string=?)
            (test-assert "collision in 100 codes is implausible" #f)
            (loop (+ i 1) seen))
           ((not (valid-code? c))
            (test-assert "random-code emits valid codes" #f)
            (loop (+ i 1) (cons c seen)))
           (else (loop (+ i 1) (cons c seen)))))))))

(test-end "shortener")

(exit (if (= 0 (last-run-failed-tests)) 0 1))
