;; Usage: scm bin/hash-passphrase.scm '<passphrase>'
;; Emits: pbkdf2$<iterations>$<base64-salt>$<base64-hash>
;;
;; Paste the output into config.scm as auth-passphrase-hash.

(import (scheme base)
        (scheme write)
        (scheme process-context)
        (scm crypto password))

(define (main args)
  (when (not (= (length args) 1))
    (display "usage: scm bin/hash-passphrase.scm '<passphrase>'\n"
             (current-error-port))
    (exit 2))
  (display (password-hash (car args)))
  (newline))

(main (cdr (command-line)))
