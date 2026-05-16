(define-library (damian feeds-parser)
  (import (scheme base)
          (scheme write)
          (scheme char)
          (scheme cxr)
          (srfi 1)
          (srfi 13)
          (scm string)
          (scm xml))
  (export parse-feed-file
          ;; exposed for testing
          local-name
          qname-field
          parse-pubdate)
  (begin

    ;; --------------------------------------------------------------
    ;; Parses an Atom or RSS 2.0 feed from a local file. The reader
    ;; primitives only accept paths; the fetcher takes care of writing
    ;; the HTTP body to a temp file first.
    ;;
    ;; Returns:
    ;;   (cons feed-title entry-list)
    ;; where entry-list is a list of alists with the keys
    ;;   "title" "link" "guid" "summary" "published" (as text)
    ;; Missing fields default to "".
    ;; --------------------------------------------------------------

    (define (local-name qname)
      ;; Strip any namespace prefix and downcase. Used for the item/entry
      ;; element detection where namespacing doesn't matter.
      (let ((n (string-length qname)))
        (let loop ((i 0))
          (cond
            ((= i n) (string-downcase qname))
            ((char=? (string-ref qname i) #\:)
             (string-downcase (substring qname (+ i 1) n)))
            (else (loop (+ i 1)))))))

    (define entry-field-names
      '("title" "link" "id" "guid" "description" "summary" "content"
        "published" "pubdate" "updated"))

    (define (qname-field qname)
      ;; Returns the entry-field name (lowercased) for a qname if it is
      ;; unprefixed or uses the atom: prefix, else #f. This is what guards
      ;; us against namespaced extension elements (e.g. <media:content>,
      ;; <dc:creator>) that would otherwise collide with real Atom fields.
      (let ((colon (string-index qname #\:)))
        (cond
          ((not colon)
           (let ((n (string-downcase qname)))
             (cond ((member n entry-field-names string=?) n)
                   (else #f))))
          (else
           (let ((prefix (string-downcase (substring qname 0 colon)))
                 (rest   (string-downcase
                           (substring qname (+ colon 1) (string-length qname)))))
             (cond
               ((string=? prefix "atom")
                (cond ((member rest entry-field-names string=?) rest)
                      (else #f)))
               (else #f)))))))

    (define (pick alist . keys)
      ;; Returns the first non-empty value for any of keys, or "".
      (let loop ((ks keys))
        (cond
          ((null? ks) "")
          (else
           (let ((p (assoc (car ks) alist)))
             (cond
               ((and p (not (= 0 (string-length (cdr p))))) (cdr p))
               (else (loop (cdr ks)))))))))

    (define (normalise-entry raw)
      (list (cons "title"     (pick raw "title"))
            (cons "link"      (pick raw "link"))
            (cons "guid"      (pick raw "id" "guid" "link"))
            (cons "summary"   (pick raw "summary" "description" "content"))
            (cons "published" (pick raw "published" "updated" "pubdate"))))

    (define (xml-text-or-empty r)
      (or (xml-value r) ""))

    (define (parse-from-reader r)
      (let scan ((feed-title #f)
                 (entries '())
                 (entry #f)
                 (continue? #t))
        (cond
          ((not continue?)
           (cons (or feed-title "") (reverse entries)))

          (else
           (case (xml-node-type r)
             ((element)
              (let* ((qname (xml-name r))
                     (lnm   (local-name qname))
                     (field (qname-field qname)))
                (cond
                  ;; --- enter an item/entry ---
                  ((or (string=? lnm "item") (string=? lnm "entry"))
                   (scan feed-title entries '() (xml-read r)))

                  ;; --- inside an item ---
                  (entry
                   (cond
                     ;; Atom uses <link href="..."/>; RSS has text content.
                     ((and field (string=? field "link"))
                      (let ((href (xml-attribute r "href")))
                        (cond
                          (href
                           (scan feed-title entries
                                 (cons (cons "link" href) entry)
                                 (xml-read r)))
                          (else
                           (let ((v (xml-text-or-empty r)))
                             (scan feed-title entries
                                   (cons (cons "link" v) entry)
                                   #t))))))
                     (field
                      (let ((v (xml-text-or-empty r)))
                        (scan feed-title entries
                              (cons (cons field v) entry)
                              #t)))
                     (else
                      (scan feed-title entries entry (xml-read r)))))

                  ;; --- feed metadata (before first item) ---
                  ((and (not feed-title) (string=? lnm "title")
                        (qname-field qname))  ; require unprefixed/atom
                   (let ((v (xml-text-or-empty r)))
                     (scan v entries entry #t)))

                  (else
                   (scan feed-title entries entry (xml-read r))))))

             ((end-element)
              (let ((nm (local-name (xml-name r))))
                (cond
                  ((and entry
                        (or (string=? nm "item") (string=? nm "entry")))
                   (scan feed-title
                         (cons (normalise-entry (reverse entry)) entries)
                         #f
                         (xml-read r)))
                  (else
                   (scan feed-title entries entry (xml-read r))))))

             (else
              (scan feed-title entries entry (xml-read r))))))))

    (define (parse-feed-file path)
      (let ((r (open-xml-file path)))
        (guard (exn (#t (close-xml r) (raise exn)))
          (let ((result (parse-from-reader r)))
            (close-xml r)
            result))))

    ;; --------------------------------------------------------------
    ;; Date parsing: RSS 2.0 uses RFC 822/2822 ("Thu, 16 May 2024
    ;; 12:34:56 +0200"); Atom uses ISO 8601 ("2024-05-16T12:34:56Z").
    ;; We parse both into unix seconds and return an integer, or #f
    ;; if unparseable. Callers can then format with to_timestamp().
    ;; --------------------------------------------------------------

    (define (digit? c) (and (char>=? c #\0) (char<=? c #\9)))

    (define (parse-int s start end)
      ;; Returns the integer value of s[start..end] or #f on failure.
      (cond
        ((or (< end start) (> end (string-length s))) #f)
        ((= end start) 0)
        (else
         (let loop ((i start) (acc 0))
           (cond
             ((= i end) acc)
             ((digit? (string-ref s i))
              (loop (+ i 1) (+ (* acc 10)
                               (- (char->integer (string-ref s i))
                                  (char->integer #\0)))))
             (else #f))))))

    (define month-table
      '(("Jan" . 1) ("Feb" . 2) ("Mar" . 3) ("Apr" . 4)
        ("May" . 5) ("Jun" . 6) ("Jul" . 7) ("Aug" . 8)
        ("Sep" . 9) ("Oct" . 10) ("Nov" . 11) ("Dec" . 12)))

    (define (month-number name)
      (let ((p (assoc (substring name 0 (min 3 (string-length name)))
                      month-table)))
        (and p (cdr p))))

    (define (days-before-month month year)
      ;; Cumulative days in this year before the given (1-based) month.
      (let ((leap (and (zero? (modulo year 4))
                       (or (not (zero? (modulo year 100)))
                           (zero? (modulo year 400))))))
        (let ((tab (if leap
                       '#(0 31 60 91 121 152 182 213 244 274 305 335)
                       '#(0 31 59 90 120 151 181 212 243 273 304 334))))
          (vector-ref tab (- month 1)))))

    (define (days-before-year y)
      ;; Days from 1970-01-01 to Jan 1 of y. Handles 1970..2200 cleanly.
      (let loop ((yr 1970) (acc 0))
        (cond
          ((= yr y) acc)
          (else
           (let ((leap (and (zero? (modulo yr 4))
                            (or (not (zero? (modulo yr 100)))
                                (zero? (modulo yr 400))))))
             (loop (+ yr 1) (+ acc (if leap 366 365))))))))

    (define (to-unix year month day hour minute second tz-offset-minutes)
      ;; tz-offset-minutes is the offset from UTC, so subtract it.
      (and year month day
           (let ((days (+ (days-before-year year)
                          (days-before-month month year)
                          (- day 1))))
             (- (+ (* days 86400)
                   (* hour 3600)
                   (* minute 60)
                   second)
                (* tz-offset-minutes 60)))))

    (define (parse-tz s)
      ;; Returns minutes east of UTC. Handles "Z", "+HH:MM", "-HHMM",
      ;; "GMT", "UT", "UTC", and the legacy zone names from RFC 822
      ;; (EST, EDT, CST, CDT, MST, MDT, PST, PDT). Unknown → 0.
      (cond
        ((or (string=? s "") (string=? s "Z")
             (string=? s "GMT") (string=? s "UT") (string=? s "UTC"))
         0)
        ((or (char=? (string-ref s 0) #\+)
             (char=? (string-ref s 0) #\-))
         (let* ((sign (if (char=? (string-ref s 0) #\+) 1 -1))
                (rest (substring s 1 (string-length s)))
                (rest (if (string-index rest #\:)
                          (string-append (substring rest 0 (string-index rest #\:))
                                         (substring rest (+ 1 (string-index rest #\:))
                                                    (string-length rest)))
                          rest)))
           (cond
             ((>= (string-length rest) 4)
              (let ((h (parse-int rest 0 2))
                    (m (parse-int rest 2 4)))
                (if (and h m) (* sign (+ (* h 60) m)) 0)))
             ((>= (string-length rest) 2)
              (let ((h (parse-int rest 0 2)))
                (if h (* sign (* h 60)) 0)))
             (else 0))))
        ((string=? s "EST") -300) ((string=? s "EDT") -240)
        ((string=? s "CST") -360) ((string=? s "CDT") -300)
        ((string=? s "MST") -420) ((string=? s "MDT") -360)
        ((string=? s "PST") -480) ((string=? s "PDT") -420)
        (else 0)))

    (define (parse-rfc822 s)
      ;; "Thu, 16 May 2024 12:34:56 +0200"
      (let* ((s (string-trim-both s))
             ;; drop optional day-of-week prefix "Day, "
             (comma (string-index s #\,))
             (rest  (if comma
                        (string-trim-both
                          (substring s (+ comma 1) (string-length s)))
                        s))
             (parts (filter (lambda (p) (not (string=? p "")))
                            (string-split rest " "))))
        (and (>= (length parts) 5)
             (let* ((day   (parse-int (car parts) 0 (string-length (car parts))))
                    (mon   (month-number (cadr parts)))
                    (year  (let ((y (parse-int (caddr parts) 0
                                                (string-length (caddr parts)))))
                             (cond
                               ((not y) #f)
                               ((< y 100) (+ y 2000))
                               (else y))))
                    (time-parts (string-split (cadddr parts) ":"))
                    (hour   (and (>= (length time-parts) 1)
                                 (parse-int (list-ref time-parts 0) 0
                                            (string-length (list-ref time-parts 0)))))
                    (minute (and (>= (length time-parts) 2)
                                 (parse-int (list-ref time-parts 1) 0
                                            (string-length (list-ref time-parts 1)))))
                    (second (cond ((>= (length time-parts) 3)
                                   (parse-int (list-ref time-parts 2) 0
                                              (string-length (list-ref time-parts 2))))
                                  (else 0)))
                    (tz     (parse-tz (list-ref parts 4))))
               (to-unix year mon day (or hour 0) (or minute 0)
                        (or second 0) tz)))))

    (define (parse-iso8601 s)
      ;; "2024-05-16T12:34:56Z" or "2024-05-16T12:34:56+02:00"
      ;; Fractional seconds are ignored.
      (let ((s (string-trim-both s)))
        (and (>= (string-length s) 10)
             (char=? (string-ref s 4) #\-)
             (let* ((year (parse-int s 0 4))
                    (mon  (parse-int s 5 7))
                    (day  (parse-int s 8 10)))
               (cond
                 ((not (and year mon day)) #f)
                 ((<= (string-length s) 10)
                  (to-unix year mon day 0 0 0 0))
                 ((or (char=? (string-ref s 10) #\T)
                      (char=? (string-ref s 10) #\space))
                  (let* ((hour   (parse-int s 11 13))
                         (minute (parse-int s 14 16))
                         (second (parse-int s 17 19))
                         ;; find tz starting at first +/-/Z after position 19
                         (tz-start
                           (let loop ((i 19))
                             (cond
                               ((>= i (string-length s)) (string-length s))
                               (else
                                 (let ((c (string-ref s i)))
                                   (cond
                                     ((or (char=? c #\+) (char=? c #\-)
                                          (char=? c #\Z))
                                      i)
                                     (else (loop (+ i 1))))))))))
                    (to-unix year mon day (or hour 0) (or minute 0)
                             (or second 0)
                             (parse-tz (substring s tz-start (string-length s))))))
                 (else #f))))))

    (define (parse-pubdate s)
      ;; Try ISO first (has hyphens at index 4); fall back to RFC 822.
      (cond
        ((or (not s) (string=? s "")) #f)
        ((and (>= (string-length s) 5)
              (char=? (string-ref s 4) #\-))
         (or (parse-iso8601 s) (parse-rfc822 s)))
        (else (or (parse-rfc822 s) (parse-iso8601 s)))))

))
