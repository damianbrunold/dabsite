;; Unit tests for (dabsite multipart).

(import (scheme base) (scheme write) (scheme process-context) (scm module))
(module-search-path! (cons "src" (module-search-path)))

(import (dabsite multipart) (scm test) (srfi 64))

(test-runner-factory scm-test-runner)
(test-begin "multipart")

(test-group "multipart-boundary"
  (test-equal "abc123"
              (multipart-boundary "multipart/form-data; boundary=abc123"))
  (test-equal "abc123"
              (multipart-boundary "multipart/form-data; boundary=\"abc123\""))
  (test-equal "abc 123"
              (multipart-boundary
                "multipart/form-data; boundary=\"abc 123\"; charset=utf-8"))
  (test-equal "abc123"
              (multipart-boundary
                "Multipart/Form-Data; Boundary=abc123"))
  (test-equal "x"
              (multipart-boundary "multipart/form-data; boundary=x;next=1"))
  (test-eqv #f (multipart-boundary "application/json"))
  (test-eqv #f (multipart-boundary #f))
  (test-eqv #f (multipart-boundary "multipart/form-data")))

(define (crlf . parts)
  (apply string-append
         (let loop ((xs parts) (acc '()))
           (cond ((null? xs) (reverse acc))
                 ((null? (cdr xs)) (reverse (cons (car xs) acc)))
                 (else (loop (cdr xs)
                             (cons "\r\n" (cons (car xs) acc))))))))

(test-group "single text field"
  (define body
    (string-append
      "--BBB\r\n"
      "Content-Disposition: form-data; name=\"foo\"\r\n"
      "\r\n"
      "hello"
      "\r\n--BBB--\r\n"))
  (define parts (parse-multipart body "BBB"))
  (test-eqv 1 (length parts))
  (test-equal "foo"  (part-ref (car parts) 'name))
  (test-eqv #f       (part-ref (car parts) 'filename))
  (test-equal "hello" (part-ref (car parts) 'body)))

(test-group "two fields plus file"
  (define body
    (string-append
      "--X\r\n"
      "Content-Disposition: form-data; name=\"title\"\r\n"
      "\r\n"
      "My photo"
      "\r\n--X\r\n"
      "Content-Disposition: form-data; name=\"public\"\r\n"
      "\r\n"
      "1"
      "\r\n--X\r\n"
      "Content-Disposition: form-data; name=\"file\"; filename=\"a.txt\"\r\n"
      "Content-Type: text/plain\r\n"
      "\r\n"
      "line1\r\nline2"
      "\r\n--X--\r\n"))
  (define parts (parse-multipart body "X"))
  (test-eqv 3 (length parts))
  (test-equal "title"       (part-ref (list-ref parts 0) 'name))
  (test-equal "My photo"    (part-ref (list-ref parts 0) 'body))
  (test-equal "public"      (part-ref (list-ref parts 1) 'name))
  (test-equal "1"           (part-ref (list-ref parts 1) 'body))
  (test-equal "file"        (part-ref (list-ref parts 2) 'name))
  (test-equal "a.txt"       (part-ref (list-ref parts 2) 'filename))
  (test-equal "text/plain"  (part-ref (list-ref parts 2) 'content-type))
  (test-equal "line1\r\nline2" (part-ref (list-ref parts 2) 'body)))

(test-group "body containing the boundary prefix but not the delimiter"
  ;; A body fragment "--Bnot" must not be mistaken for the boundary "B".
  (define body
    (string-append
      "--B\r\n"
      "Content-Disposition: form-data; name=\"x\"\r\n"
      "\r\n"
      "--Bnot-actually-a-boundary"
      "\r\n--B--\r\n"))
  (define parts (parse-multipart body "B"))
  (test-eqv 1 (length parts))
  (test-equal "--Bnot-actually-a-boundary"
              (part-ref (car parts) 'body)))

(test-group "byte-preserving body"
  ;; Build a body containing each byte 0..255. The parser must return the
  ;; same string back. Uses integer->char so this works regardless of
  ;; whether the implementation's strings are utf-8 or byte-indexed.
  (define raw
    (let ((out (open-output-string)))
      (let loop ((i 0))
        (cond
          ((= i 256) (get-output-string out))
          (else
           ;; Skip CR and LF — those would interfere with the boundary
           ;; sequence we splice around the payload.
           (cond
             ((or (= i 13) (= i 10)) #t)
             (else (write-char (integer->char i) out)))
           (loop (+ i 1)))))))
  (define body
    (string-append
      "--Z\r\n"
      "Content-Disposition: form-data; name=\"bin\"; filename=\"b.bin\"\r\n"
      "Content-Type: application/octet-stream\r\n"
      "\r\n"
      raw
      "\r\n--Z--\r\n"))
  (define parts (parse-multipart body "Z"))
  (test-eqv 1 (length parts))
  (test-equal raw (part-ref (car parts) 'body)))

(test-group "malformed inputs"
  (test-equal '() (parse-multipart "" "B"))
  (test-equal '() (parse-multipart "garbage with no boundary" "B"))
  (test-equal '() (parse-multipart "--B\r\nheaders only" "B"))
  (test-equal '() (parse-multipart #f "B"))
  (test-equal '() (parse-multipart "data" "")))

(define (str->bv s)
  (let* ((n (string-length s)) (bv (make-bytevector n)))
    (let loop ((i 0))
      (cond ((= i n) bv)
            (else
             (bytevector-u8-set! bv i (char->integer (string-ref s i)))
             (loop (+ i 1)))))))

(test-group "parse-multipart-bytes"
  (define body
    (string-append
      "--Z\r\n"
      "Content-Disposition: form-data; name=\"file\"; filename=\"b.bin\"\r\n"
      "Content-Type: application/octet-stream\r\n"
      "\r\n"
      "AB\xFE;\xFF;CD"
      "\r\n--Z\r\n"
      "Content-Disposition: form-data; name=\"note\"\r\n"
      "\r\n"
      "hello"
      "\r\n--Z--\r\n"))
  (define parts (parse-multipart-bytes (str->bv body) "Z"))
  (test-eqv 2 (length parts))
  (test-equal "file" (part-ref (list-ref parts 0) 'name))
  (test-equal "b.bin" (part-ref (list-ref parts 0) 'filename))
  (test-assert (bytevector? (part-ref (list-ref parts 0) 'body)))
  (test-eqv 6 (bytevector-length (part-ref (list-ref parts 0) 'body)))
  (test-eqv #xFE (bytevector-u8-ref
                   (part-ref (list-ref parts 0) 'body) 2))
  (test-eqv #xFF (bytevector-u8-ref
                   (part-ref (list-ref parts 0) 'body) 3))
  (test-equal "note" (part-ref (list-ref parts 1) 'name))
  (test-equal "hello"
              (let* ((bv (part-ref (list-ref parts 1) 'body))
                     (n  (bytevector-length bv))
                     (out (open-output-string)))
                (let loop ((i 0))
                  (cond ((= i n) (get-output-string out))
                        (else
                         (write-char (integer->char
                                       (bytevector-u8-ref bv i)) out)
                         (loop (+ i 1))))))))

(test-end "multipart")

(exit (if (= 0 (last-run-failed-tests)) 0 1))
