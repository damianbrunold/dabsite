;; Usage:
;;   scm bin/gen-passphrase.scm              # 5 random words
;;   scm bin/gen-passphrase.scm words 6      # N random words
;;   scm bin/gen-passphrase.scm chars        # 20 random alphanumerics
;;   scm bin/gen-passphrase.scm chars 32     # N random alphanumerics
;;
;; Prints two things:
;;   - the plaintext passphrase (write it down somewhere safe)
;;   - the pbkdf2 hash line, ready to paste into config.scm as
;;     auth-passphrase-hash.

(import (scheme base)
        (scheme write)
        (scheme process-context)
        (srfi 1)
        (scm crypto))

(define iterations 200000)
(define salt-bytes 16)
(define key-bytes  32)

;; A small curated wordlist of short, distinct, easy-to-type English
;; nouns. ~250 words = ~7.95 bits/word; five words = ~39 bits of
;; entropy — well past what's reachable through PBKDF2 + the 0.5 s
;; login delay. Swap in EFF's larger list for more entropy if you like.
(define wordlist
  '("amber" "anchor" "ant" "anvil" "apple" "apricot" "arch" "arrow"
    "ash" "aspen" "badger" "bagel" "bamboo" "barn" "basil" "basin"
    "bat" "bay" "beach" "bean" "bear" "beech" "beetle" "bell"
    "berry" "birch" "bird" "blade" "blaze" "bloom" "blue" "boat"
    "bolt" "bone" "book" "boot" "bow" "brass" "bread" "brick"
    "bridge" "brisk" "brook" "brown" "brush" "bud" "bug" "cabin"
    "cactus" "calm" "candle" "cape" "carp" "cart" "cedar" "chair"
    "cherry" "chest" "chime" "chip" "clay" "cliff" "cloak" "cloud"
    "clover" "coal" "coast" "cobalt" "comb" "cone" "copper" "coral"
    "cord" "cove" "crab" "crane" "creek" "crest" "crow" "crown"
    "crystal" "cup" "daisy" "dawn" "deer" "delta" "desk" "dew"
    "dish" "dock" "doe" "dome" "dove" "drift" "drum" "duck"
    "dune" "dusk" "eagle" "ember" "fall" "fawn" "feather" "fence"
    "fern" "field" "finch" "fish" "flame" "flask" "flax" "flint"
    "flood" "flour" "foam" "forest" "fox" "frog" "frost" "fur"
    "garden" "gate" "gem" "ginger" "glade" "gold" "goose" "grain"
    "grass" "green" "grove" "gull" "harbor" "hawk" "hazel" "heart"
    "hedge" "hen" "heron" "hill" "hive" "honey" "hood" "horn"
    "horse" "ice" "ink" "iris" "iron" "island" "ivory" "ivy"
    "jade" "jasper" "jay" "juniper" "kettle" "key" "kiln" "knot"
    "lake" "lamp" "lantern" "lark" "lava" "leaf" "leek" "lemon"
    "lens" "lichen" "lily" "linen" "lion" "log" "lotus" "lynx"
    "magpie" "mango" "maple" "marble" "marsh" "meadow" "mint" "mist"
    "mole" "moon" "moss" "moth" "nest" "north" "oak" "oat"
    "ocean" "olive" "onion" "onyx" "orchid" "otter" "oval" "owl"
    "panda" "peach" "pearl" "peak" "pebble" "pen" "petal" "pier"
    "pillar" "pine" "pink" "plum" "pond" "poplar" "poppy" "port"
    "pot" "prairie" "quail" "quartz" "queen" "quill" "rain" "raven"
    "reed" "reef" "ridge" "ring" "river" "robin" "rock" "rope"
    "rose" "ruby" "sage" "salt" "sand" "sea" "seal" "shade"
    "shed" "shell" "ship" "silk" "silver" "sky" "slate" "snail"
    "snow" "song" "south" "spark" "spice" "spring" "spruce" "star"))

(define wordvec (list->vector wordlist))
(define nwords  (vector-length wordvec))

(define charset
  ;; 62 alphanumerics — no lookalikes are stripped on purpose so the
  ;; passphrase keeps all entropy. log2(62) ≈ 5.95 bits per char.
  "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")
(define charset-len (string-length charset))

(define (rand-byte)
  (bytevector-u8-ref (random-bytes 1) 0))

(define (rand-below n)
  ;; Unbiased modular pick: reject bytes in the trailing residue.
  (let* ((limit (- 256 (modulo 256 n))))
    (let loop ()
      (let ((b (rand-byte)))
        (cond ((< b limit) (modulo b n))
              (else (loop)))))))

(define (rand-word)
  (vector-ref wordvec (rand-below nwords)))

(define (rand-char)
  (string-ref charset (rand-below charset-len)))

(define (gen-words n)
  (let loop ((i 0) (acc '()))
    (cond ((= i n) (reverse acc))
          (else (loop (+ i 1) (cons (rand-word) acc))))))

(define (gen-chars n)
  (let ((out (open-output-string)))
    (let loop ((i 0))
      (cond ((= i n) (get-output-string out))
            (else (write-char (rand-char) out) (loop (+ i 1)))))))

(define (compute-hash pw)
  (let* ((salt (random-bytes salt-bytes))
         (hash (pbkdf2-sha256 (string->utf8 pw) salt iterations key-bytes)))
    (string-append "pbkdf2$"
                   (number->string iterations) "$"
                   (base64-encode salt) "$"
                   (base64-encode hash))))

(define (usage)
  (display "usage: scm bin/gen-passphrase.scm [words [N] | chars [N]]\n"
           (current-error-port))
  (exit 2))

(define (parse-args args)
  ;; Returns (values mode n) where mode is 'words or 'chars.
  (cond
    ((null? args) (values 'words 5))
    ((string=? (car args) "words")
     (cond
       ((null? (cdr args)) (values 'words 5))
       (else
        (let ((n (string->number (cadr args))))
          (cond ((and n (integer? n) (> n 0)) (values 'words n))
                (else (usage)))))))
    ((string=? (car args) "chars")
     (cond
       ((null? (cdr args)) (values 'chars 20))
       (else
        (let ((n (string->number (cadr args))))
          (cond ((and n (integer? n) (> n 0)) (values 'chars n))
                (else (usage)))))))
    (else (usage))))

(define (main)
  (call-with-values
    (lambda () (parse-args (cdr (command-line))))
    (lambda (mode n)
      (let* ((pw (cond
                   ((eq? mode 'words)
                    (let ((words (gen-words n)))
                      (fold (lambda (w acc)
                              (cond ((string=? acc "") w)
                                    (else (string-append acc " " w))))
                            "" words)))
                   (else (gen-chars n))))
             (h  (compute-hash pw)))
        (display "Passphrase (store this!):") (newline)
        (display "  ") (display pw) (newline)
        (newline)
        (display "auth-passphrase-hash for config.scm:") (newline)
        (display "  ") (display h) (newline)))))

(main)
