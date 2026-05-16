;; Unit tests for (damian files) — pure helpers only.

(import (scheme base) (scheme write) (scheme process-context) (scm module))
(module-search-path! (cons "src" (module-search-path)))

(import (damian files) (scm test) (srfi 64))

(test-runner-factory scm-test-runner)
(test-begin "files")

(test-group "mime-from-name"
  (test-equal "image/png"  (mime-from-name "icon.png"))
  (test-equal "image/jpeg" (mime-from-name "PHOTO.JPG"))
  (test-equal "application/pdf" (mime-from-name "doc.pdf"))
  (test-equal "application/octet-stream"
              (mime-from-name "weird"))
  (test-equal "text/plain; charset=utf-8"
              (mime-from-name "notes.txt")))

(test-group "safe-public-name"
  (test-equal "x.png"      (safe-public-name "x.png"))
  (test-equal "photo.jpg"  (safe-public-name "/tmp/photo.jpg"))
  (test-equal "photo.jpg"  (safe-public-name "C:\\users\\x\\photo.jpg"))
  (test-eqv #f (safe-public-name ""))
  (test-eqv #f (safe-public-name #f))
  (test-eqv #f (safe-public-name "../etc/passwd"))
  (test-eqv #f (safe-public-name ".hidden"))
  (test-eqv #f (safe-public-name "/foo/../bar"))
  ;; control bytes + quotes must be rejected
  (test-eqv #f (safe-public-name "x\ry.png"))
  (test-eqv #f (safe-public-name "x\ny.png"))
  (test-eqv #f (safe-public-name (string #\a (integer->char 0) #\b)))
  (test-eqv #f (safe-public-name "x\"y.png")))

(test-end "files")

(exit (if (= 0 (last-run-failed-tests)) 0 1))
