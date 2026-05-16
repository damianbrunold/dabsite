;; Usage: scm bin/hash-passphrase.scm '<passphrase>'
;; Emits: pbkdf2$<iterations>$<base64-salt>$<base64-hash>
;;
;; Paste the output into config.scm as auth-passphrase-hash.

(import (scheme base)
        (scheme write)
        (scheme process-context)
        (scm crypto))

(define iterations 200000)
(define salt-bytes 16)
(define key-bytes  32)

(define (bytevector->base64 bv)
  (base64-encode bv))

(define (main args)
  (when (not (= (length args) 1))
    (display "usage: scm bin/hash-passphrase.scm '<passphrase>'\n"
             (current-error-port))
    (exit 2))
  (let* ((pw   (car args))
         (salt (random-bytes salt-bytes))
         (hash (pbkdf2-sha256 (string->utf8 pw) salt iterations key-bytes)))
    (display "pbkdf2$")
    (display iterations)
    (display "$")
    (display (bytevector->base64 salt))
    (display "$")
    (display (bytevector->base64 hash))
    (newline)))

(main (cdr (command-line)))
