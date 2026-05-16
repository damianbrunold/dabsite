;; Usage: scm bin/gen-secret.scm
;; Emits a base64-encoded 32-byte random secret for cookie signing.

(import (scheme base)
        (scheme write)
        (scm crypto))

(display (base64-encode (random-bytes 32)))
(newline)
