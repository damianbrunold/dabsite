(define-library (dabsite util)
  (import (scheme base)
          (scheme write)
          (scheme char)
          (scm string)
          (srfi 13)
          (srfi 151)
          (scm crypto))
  (export ;; encoding / decoding
          percent-encode
          percent-decode
          html-escape
          html-attr-escape
          strip-html-tags
          ;; form bodies and query strings
          parse-www-form
          form-ref
          form-refs-by-prefix
          ;; cookies
          parse-cookie-header
          cookie-ref
          format-set-cookie
          ;; sql safety
          sql-quote-literal
          sql-quote-int
          ;; durations
          parse-duration
          format-duration
          ;; misc
          constant-time-bv-equal?
          bytevector->hex
          hex->bytevector
          string-bytes
          bytes->string)
  (begin

    ;; ============================================================
    ;; HTML escaping
    ;; ============================================================

    (define (html-escape s)
      (let* ((n (string-length s))
             (out (open-output-string)))
        (let loop ((i 0))
          (cond
            ((= i n) (get-output-string out))
            (else
             (let ((c (string-ref s i)))
               (cond ((char=? c #\<) (write-string "&lt;"   out))
                     ((char=? c #\>) (write-string "&gt;"   out))
                     ((char=? c #\&) (write-string "&amp;"  out))
                     ((char=? c #\") (write-string "&quot;" out))
                     ((char=? c #\') (write-string "&#39;"  out))
                     (else           (write-char c out))))
             (loop (+ i 1)))))))

    ;; Alias for clarity at call sites that escape attribute values.
    (define html-attr-escape html-escape)

    ;; Strips anything looking like an HTML tag and collapses runs of
    ;; whitespace. Intended for plain-text contexts like tooltip values,
    ;; where you want a readable string, not the rendered HTML. Does NOT
    ;; decode entities — html-escape afterwards still works correctly.
    (define (strip-html-tags s)
      (let* ((n   (string-length s))
             (out (open-output-string)))
        (let loop ((i 0) (in-tag? #f) (in-ws? #f) (any-out? #f))
          (cond
            ((= i n) (get-output-string out))
            (else
             (let ((c (string-ref s i)))
               (cond
                 (in-tag?
                  (cond
                    ((char=? c #\>) (loop (+ i 1) #f in-ws? any-out?))
                    (else           (loop (+ i 1) #t in-ws? any-out?))))
                 ((char=? c #\<)
                  (loop (+ i 1) #t in-ws? any-out?))
                 ((or (char=? c #\space) (char=? c #\tab)
                      (char=? c #\newline) (char=? c #\return))
                  (cond
                    ((not any-out?) (loop (+ i 1) #f #f #f))
                    (in-ws?         (loop (+ i 1) #f #t any-out?))
                    (else (write-char #\space out)
                          (loop (+ i 1) #f #t any-out?))))
                 (else
                  (write-char c out)
                  (loop (+ i 1) #f #f #t)))))))))

    ;; ============================================================
    ;; Percent-encoding (RFC 3986 unreserved + "+ for space" decode)
    ;; ============================================================

    (define (hex-digit? c)
      (or (and (char>=? c #\0) (char<=? c #\9))
          (and (char>=? c #\a) (char<=? c #\f))
          (and (char>=? c #\A) (char<=? c #\F))))

    (define (hex-value c)
      (cond ((and (char>=? c #\0) (char<=? c #\9))
             (- (char->integer c) (char->integer #\0)))
            ((and (char>=? c #\a) (char<=? c #\f))
             (+ 10 (- (char->integer c) (char->integer #\a))))
            ((and (char>=? c #\A) (char<=? c #\F))
             (+ 10 (- (char->integer c) (char->integer #\A))))
            (else (error "hex-value: not a hex digit" c))))

    (define (unreserved? c)
      (or (and (char>=? c #\a) (char<=? c #\z))
          (and (char>=? c #\A) (char<=? c #\Z))
          (and (char>=? c #\0) (char<=? c #\9))
          (char=? c #\-) (char=? c #\_) (char=? c #\.) (char=? c #\~)))

    (define (write-hex-byte n out)
      (let ((digits "0123456789ABCDEF"))
        (write-char #\% out)
        (write-char (string-ref digits (quotient n 16)) out)
        (write-char (string-ref digits (modulo n 16)) out)))

    (define (percent-encode s)
      ;; Encodes a string as UTF-8 then percent-escapes everything except
      ;; the unreserved set. Suitable for query values and path segments.
      (let* ((bv (string->utf8 s))
             (n  (bytevector-length bv))
             (out (open-output-string)))
        (let loop ((i 0))
          (cond
            ((= i n) (get-output-string out))
            (else
             (let* ((b (bytevector-u8-ref bv i))
                    (c (integer->char b)))
               (cond ((and (< b 128) (unreserved? c)) (write-char c out))
                     (else (write-hex-byte b out)))
               (loop (+ i 1))))))))

    (define (percent-decode s . opt)
      ;; Decodes percent-escaped UTF-8. When (car opt) is true (default),
      ;; '+' is treated as space (application/x-www-form-urlencoded rules).
      (let* ((plus-as-space? (if (null? opt) #t (car opt)))
             (n (string-length s))
             (bv (make-bytevector n 0)))
        (let loop ((i 0) (j 0))
          (cond
            ((= i n) (utf8->string bv 0 j))
            ((and (char=? (string-ref s i) #\%)
                  (< (+ i 2) n)
                  (hex-digit? (string-ref s (+ i 1)))
                  (hex-digit? (string-ref s (+ i 2))))
             (bytevector-u8-set! bv j
               (+ (* 16 (hex-value (string-ref s (+ i 1))))
                  (hex-value (string-ref s (+ i 2)))))
             (loop (+ i 3) (+ j 1)))
            ((and plus-as-space? (char=? (string-ref s i) #\+))
             (bytevector-u8-set! bv j 32)
             (loop (+ i 1) (+ j 1)))
            (else
             (let ((c (string-ref s i)))
               ;; ASCII fast path; otherwise re-encode UTF-8.
               (cond ((< (char->integer c) 128)
                      (bytevector-u8-set! bv j (char->integer c))
                      (loop (+ i 1) (+ j 1)))
                     (else
                      ;; Should not occur for valid URL-encoded input.
                      (let ((cb (string->utf8 (string c))))
                        (let copy ((k 0) (j j))
                          (cond
                            ((= k (bytevector-length cb)) (loop (+ i 1) j))
                            (else
                             (bytevector-u8-set! bv j (bytevector-u8-ref cb k))
                             (copy (+ k 1) (+ j 1))))))))))))))

    ;; ============================================================
    ;; application/x-www-form-urlencoded
    ;; ============================================================

    (define (split-on-char s ch)
      (let* ((n (string-length s))
             (acc '()))
        (let loop ((i 0) (start 0))
          (cond
            ((= i n)
             (reverse (cons (substring s start n) acc)))
            ((char=? (string-ref s i) ch)
             (let ((part (substring s start i)))
               (set! acc (cons part acc))
               (loop (+ i 1) (+ i 1))))
            (else (loop (+ i 1) start))))))

    (define (parse-www-form body)
      ;; Parses a urlencoded body (or query string) into an alist of decoded
      ;; (key . value) string pairs. Empty body → '().
      (cond
        ((or (not body) (string=? body "")) '())
        (else
         (map (lambda (pair)
                (let* ((eq-idx (string-index pair #\=)))
                  (cond
                    (eq-idx
                     (cons (percent-decode (substring pair 0 eq-idx))
                           (percent-decode (substring pair (+ eq-idx 1)
                                                     (string-length pair)))))
                    (else
                     (cons (percent-decode pair) "")))))
              (split-on-char body #\&)))))

    (define (form-ref form key . default)
      (let ((p (assoc key form)))
        (cond (p (cdr p))
              ((pair? default) (car default))
              (else #f))))

    (define (form-refs-by-prefix form prefix)
      ;; Returns all (key . value) pairs from a parsed form whose key
      ;; starts with prefix. Order is preserved.
      (let ((plen (string-length prefix)))
        (let loop ((ps form) (acc '()))
          (cond
            ((null? ps) (reverse acc))
            (else
             (let* ((p (car ps))
                    (k (car p)))
               (cond
                 ((and (>= (string-length k) plen)
                       (string=? (substring k 0 plen) prefix))
                  (loop (cdr ps) (cons p acc)))
                 (else (loop (cdr ps) acc)))))))))

    ;; ============================================================
    ;; Cookies
    ;; ============================================================

    (define (parse-cookie-header header)
      ;; Parses "Cookie: a=1; b=2" header value into an alist. Whitespace
      ;; around names/values is trimmed. Values are NOT url-decoded; the
      ;; caller decides (auth cookie is base64 + hex, which need no decode).
      (cond
        ((or (not header) (string=? header "")) '())
        (else
         (let ((parts (split-on-char header #\;)))
           (let loop ((ps parts) (acc '()))
             (cond
               ((null? ps) (reverse acc))
               (else
                (let* ((raw (car ps))
                       (s   (string-trim-both raw))
                       (eq-idx (string-index s #\=)))
                  (cond
                    (eq-idx
                     (loop (cdr ps)
                           (cons (cons (substring s 0 eq-idx)
                                       (substring s (+ eq-idx 1)
                                                  (string-length s)))
                                 acc)))
                    (else (loop (cdr ps) acc)))))))))))

    (define (cookie-ref cookies name)
      (let ((p (assoc name cookies)))
        (if p (cdr p) #f)))

    (define (format-set-cookie name value max-age path . opt)
      ;; opt: 'secure 'samesite-strict 'httponly are all on by default.
      (let ((out (open-output-string)))
        (write-string name out)
        (write-char #\= out)
        (write-string value out)
        (write-string "; Path=" out)
        (write-string path out)
        (when max-age
          (write-string "; Max-Age=" out)
          (write-string (number->string max-age) out))
        (write-string "; HttpOnly; SameSite=Strict" out)
        ;; Secure is added when the caller knows TLS is in play. Default on:
        ;; for local dev set (config) secure-cookies? to #f and use a wrapper.
        (when (or (null? opt) (memv 'secure opt))
          (write-string "; Secure" out))
        (get-output-string out)))

    ;; ============================================================
    ;; SQL quoting
    ;; ============================================================

    (define (sql-quote-literal s)
      ;; Doubles single quotes. Wraps in single quotes. PostgreSQL ships
      ;; with standard_conforming_strings=on (default since 9.1) so a
      ;; backslash inside '...' is a literal backslash — doubling it
      ;; would corrupt real data without adding any safety. CALLERS MUST
      ;; use this for any user-controlled string in SQL.
      (let* ((n (string-length s))
             (out (open-output-string)))
        (write-char #\' out)
        (let loop ((i 0))
          (cond
            ((= i n)
             (write-char #\' out)
             (get-output-string out))
            (else
             (let ((c (string-ref s i)))
               (cond ((char=? c #\') (write-string "''" out))
                     (else           (write-char c out)))
               (loop (+ i 1))))))))

    (define (sql-quote-int n)
      ;; Accepts an integer; returns its decimal representation. Use for
      ;; numeric parameters so callers can't slip in SQL via string concat.
      (cond
        ((integer? n) (number->string n))
        ((string? n)
         ;; Validate that the string is a plain integer.
         (let ((parsed (string->number n)))
           (cond ((and parsed (integer? parsed)) (number->string parsed))
                 (else (error "sql-quote-int: not an integer string" n)))))
        (else (error "sql-quote-int: not an integer" n))))

    ;; ============================================================
    ;; Duration parsing
    ;;
    ;; Accepts plain integers (seconds), or numbers suffixed with one of
    ;; s/m/h/d (e.g. "30s", "10m", "3h", "1d"). Returns the number of
    ;; seconds as an integer, or #f if the input doesn't parse.
    ;; ============================================================

    (define (parse-duration s)
      (cond
        ((not (string? s)) #f)
        ((string=? s "") #f)
        (else
         (let* ((s     (string-trim-both s))
                (n     (string-length s))
                (last  (string-ref s (- n 1)))
                (digits (cond
                          ((or (char=? last #\s) (char=? last #\m)
                               (char=? last #\h) (char=? last #\d))
                           (substring s 0 (- n 1)))
                          (else s)))
                (num   (string->number digits))
                (mult  (cond
                         ((char=? last #\m) 60)
                         ((char=? last #\h) 3600)
                         ((char=? last #\d) 86400)
                         (else 1))))
           (cond
             ((and num (integer? num) (>= num 0))
              (* (exact num) mult))
             (else #f))))))

    (define (format-duration seconds)
      ;; Human-readable inverse: 3600 → "1h", 86400 → "1d", 90 → "90s".
      ;; Only emits a suffix when the value divides cleanly.
      (cond
        ((not (and (integer? seconds) (>= seconds 0)))
         (cond ((integer? seconds) (number->string seconds))
               (else "")))
        ((and (> seconds 0) (zero? (modulo seconds 86400)))
         (string-append (number->string (quotient seconds 86400)) "d"))
        ((and (> seconds 0) (zero? (modulo seconds 3600)))
         (string-append (number->string (quotient seconds 3600)) "h"))
        ((and (> seconds 0) (zero? (modulo seconds 60)))
         (string-append (number->string (quotient seconds 60)) "m"))
        (else (string-append (number->string seconds) "s"))))

    ;; ============================================================
    ;; Bytevector helpers
    ;; ============================================================

    (define (constant-time-bv-equal? a b)
      ;; Constant-time comparison of two bytevectors. Returns #f for
      ;; different lengths but only after iterating both fully when they
      ;; are equal length, so timing leaks just length, not content.
      (cond
        ((not (= (bytevector-length a) (bytevector-length b))) #f)
        (else
         (let ((n (bytevector-length a)))
           (let loop ((i 0) (acc 0))
             (cond
               ((= i n) (= acc 0))
               (else
                (loop (+ i 1)
                      (bitwise-ior acc
                                   (bitwise-xor (bytevector-u8-ref a i)
                                                (bytevector-u8-ref b i)))))))))))

    (define (bytevector->hex bv)
      (let* ((n (bytevector-length bv))
             (out (open-output-string))
             (digits "0123456789abcdef"))
        (let loop ((i 0))
          (cond
            ((= i n) (get-output-string out))
            (else
             (let ((b (bytevector-u8-ref bv i)))
               (write-char (string-ref digits (quotient b 16)) out)
               (write-char (string-ref digits (modulo b 16)) out)
               (loop (+ i 1))))))))

    (define (hex->bytevector s)
      (let* ((n (string-length s))
             (bv (make-bytevector (quotient n 2) 0)))
        (when (odd? n) (error "hex->bytevector: odd-length hex string"))
        (let loop ((i 0) (j 0))
          (cond
            ((= i n) bv)
            (else
             (bytevector-u8-set! bv j
               (+ (* 16 (hex-value (string-ref s i)))
                  (hex-value (string-ref s (+ i 1)))))
             (loop (+ i 2) (+ j 1)))))))

    (define (string-bytes s) (string->utf8 s))
    (define (bytes->string bv) (utf8->string bv))

))
