(define-library (dabsite files)
  (import (scheme base)
          (scheme write)
          (scheme file)
          (scheme char)
          (scheme cxr)
          (srfi 1)
          (srfi 13)
          (scm crypto)
          (scm database postgres)
          (scm fs)
          (scm net http request)
          (scm net http response)
          (scm net http route)
          (scm net http multipart)
          (scm html builder)
          (scm uri)
          (scm log)
          (dabsite db)
          (dabsite auth)
          (dabsite views))
  (export install-files-routes!
          ;; exposed for tests
          mime-from-name
          safe-public-name)
  (begin

    ;; ============================================================
    ;; Files: upload, list, delete, toggle visibility, public hosting.
    ;;
    ;; Storage: content-addressed under <files-dir>/<sha256>. The DB
    ;; carries metadata. Multiple rows may reference the same blob; we
    ;; only delete the blob when no row references it anymore.
    ;; ============================================================

    ;; Match the user-confirmed cap from the plan. The whole multipart
    ;; body is held in memory before parsing, so this cap also protects
    ;; against memory exhaustion via giant uploads.
    (define max-upload-bytes (* 25 1024 1024))

    ;; ---- mime helpers ----

    (define (mime-from-name name)
      (let ((lower (string-downcase name)))
        (cond
          ((string-suffix? ".html" lower) "text/html; charset=utf-8")
          ((string-suffix? ".htm"  lower) "text/html; charset=utf-8")
          ((string-suffix? ".css"  lower) "text/css; charset=utf-8")
          ((string-suffix? ".js"   lower) "application/javascript; charset=utf-8")
          ((string-suffix? ".json" lower) "application/json; charset=utf-8")
          ((string-suffix? ".svg"  lower) "image/svg+xml")
          ((string-suffix? ".png"  lower) "image/png")
          ((string-suffix? ".jpg"  lower) "image/jpeg")
          ((string-suffix? ".jpeg" lower) "image/jpeg")
          ((string-suffix? ".gif"  lower) "image/gif")
          ((string-suffix? ".webp" lower) "image/webp")
          ((string-suffix? ".avif" lower) "image/avif")
          ((string-suffix? ".ico"  lower) "image/x-icon")
          ((string-suffix? ".pdf"  lower) "application/pdf")
          ((string-suffix? ".txt"  lower) "text/plain; charset=utf-8")
          ((string-suffix? ".md"   lower) "text/markdown; charset=utf-8")
          ((string-suffix? ".mp3"  lower) "audio/mpeg")
          ((string-suffix? ".mp4"  lower) "video/mp4")
          ((string-suffix? ".zip"  lower) "application/zip")
          (else "application/octet-stream"))))

    (define (image-mime? mime)
      (and (string? mime) (string-prefix? "image/" mime)))

    ;; Allowlist of MIME prefixes we trust the browser to render inline.
    ;; Anything else gets served as an attachment with a generic type so
    ;; a maliciously-typed upload can't run scripts in our origin.
    (define (inline-safe-mime? mime)
      (and (string? mime)
           (or (string-prefix? "image/" mime)
               (string=? mime "application/pdf")
               (string-prefix? "audio/" mime)
               (string-prefix? "video/" mime))))

    (define (safe-content-type mime name)
      ;; Strip any CR/LF/NUL the upload may have placed in the mime type
      ;; (defence in depth — the HTTP server now sanitises too).
      (let ((clean (header-clean (or mime ""))))
        (cond
          ((string=? clean "") (mime-from-name (or name "")))
          (else clean))))

    (define (content-disposition mime name)
      ;; Always include a filename for browser save-as. Inline only for
      ;; the allowlist; everything else forces a download.
      (let* ((disp (cond ((inline-safe-mime? mime) "inline")
                         (else "attachment")))
             (clean-name (header-clean (or name ""))))
        (string-append disp
                       "; filename=\""
                       (filename-escape clean-name)
                       "\"")))

    (define nul-char (integer->char 0))
    (define (header-clean s)
      ;; Strip CR, LF, NUL — and any other control byte that has no
      ;; place in an HTTP header value.
      (let* ((n (string-length s)) (out (open-output-string)))
        (let loop ((i 0))
          (cond
            ((= i n) (get-output-string out))
            (else
             (let ((c (string-ref s i)))
               (cond ((< (char->integer c) #x20) #t)  ; skip control byte
                     (else (write-char c out)))
               (loop (+ i 1))))))))

    (define (filename-escape s)
      ;; Backslash-escape backslash and double-quote so the filename
      ;; can't break out of the surrounding "...".
      (let* ((n (string-length s)) (out (open-output-string)))
        (let loop ((i 0))
          (cond
            ((= i n) (get-output-string out))
            (else
             (let ((c (string-ref s i)))
               (cond ((or (char=? c #\\) (char=? c #\"))
                      (write-char #\\ out) (write-char c out))
                     (else (write-char c out)))
               (loop (+ i 1))))))))

    ;; ---- name sanitation ----

    (define (has-control-or-quote? s)
      (let* ((n (string-length s)))
        (let loop ((i 0))
          (cond
            ((= i n) #f)
            (else
             (let ((c (string-ref s i)))
               (cond ((< (char->integer c) #x20) #t)
                     ((= (char->integer c) #x7F) #t)
                     ((char=? c #\") #t)
                     (else (loop (+ i 1))))))))))

    (define (safe-public-name name)
      ;; Strip directory components; keep only the basename. Reject
      ;; names that try to escape, contain control chars or quotes, or
      ;; are otherwise unsafe. Returns #f if unsafe.
      (cond
        ((or (not (string? name)) (string=? name "")) #f)
        ((string-contains name "..") #f)
        ((has-control-or-quote? name) #f)
        (else
         (let* ((slash (string-index-right name #\/))
                (base  (cond (slash (substring name (+ slash 1)
                                               (string-length name)))
                             (else name)))
                (back  (string-index-right base #\\)))
           (let ((base (cond (back (substring base (+ back 1)
                                              (string-length base)))
                             (else base))))
             (cond
               ((string=? base "") #f)
               ((char=? (string-ref base 0) #\.) #f)
               (else base)))))))

    ;; ---- bytes <-> string ----

    (define (bv->utf8-string bv)
      ;; Decode a part body that we know is text (e.g. a checkbox value
      ;; or a small form field) as UTF-8. Used for non-file parts.
      (utf8->string bv))

    (define (sha256-hex bv)
      ;; (sha256-hash bv) -> bytevector; we want lowercase hex.
      (let* ((h (sha256-hash bv))
             (n (bytevector-length h))
             (out (open-output-string)))
        (let loop ((i 0))
          (cond
            ((= i n) (get-output-string out))
            (else
             (let* ((b  (bytevector-u8-ref h i))
                    (hi (quotient b 16))
                    (lo (modulo b 16)))
               (write-char (nibble->char hi) out)
               (write-char (nibble->char lo) out)
               (loop (+ i 1))))))))

    (define (nibble->char n)
      (cond ((< n 10) (integer->char (+ (char->integer #\0) n)))
            (else    (integer->char (+ (char->integer #\a) (- n 10))))))

    ;; ---- blob store on disk ----

    (define (blob-path files-dir sha)
      (string-append files-dir "/" sha))

    (define (ensure-files-dir! files-dir)
      (when (not (directory-exists? files-dir))
        (make-directory files-dir)))

    (define (write-blob! files-dir sha bv)
      (ensure-files-dir! files-dir)
      (let ((path (blob-path files-dir sha)))
        (cond
          ((file-exists? path) #t)
          (else
           (let ((port (open-binary-output-file path)))
             (write-bytevector bv port)
             (close-output-port port)
             #t)))))

    (define (read-blob files-dir sha)
      (let* ((path (blob-path files-dir sha))
             (size (file-size path))
             (port (open-binary-input-file path))
             (bv   (read-bytevector size port)))
        (close-input-port port)
        (cond ((eof-object? bv) (bytevector))
              (else bv))))

    (define (refcount-blob cfg sha)
      (let ((rs (with-db cfg
                  (lambda (c)
                    (pg-result-rows
                      (pg-query c
                        "SELECT COUNT(*)::text FROM files WHERE sha256 = $1"
                        sha))))))
        (cond ((pair? rs) (string->number (vector-ref (car rs) 0)))
              (else 0))))

    ;; ---- DB ----

    (define (list-files cfg q vis)
      (let ((where (list "1=1"))
            (params '())
            (n 0))
        (when (and (string? q) (> (string-length (string-trim-both q)) 0))
          (set! n (+ n 1))
          (let ((p (string-append "$" (number->string n))))
            (set! where
                  (cons (string-append
                          "(name ILIKE '%' || " p " || '%' "
                          "OR note ILIKE '%' || " p " || '%')")
                        where))
            (set! params (append params (list q)))))
        (when (and vis (member vis '("public" "private") string=?))
          (set! n (+ n 1))
          (set! where
                (cons (string-append "visibility = $" (number->string n))
                      where))
          (set! params (append params (list vis))))
        (alist-rows cfg
          (string-append
            "SELECT id::text AS id, name, mime, size::text AS size, "
            "       sha256, visibility, note, "
            "       to_char(created_at, 'YYYY-MM-DD HH24:MI') AS created "
            "FROM files WHERE " (string-join where " AND ") " "
            "ORDER BY created_at DESC, id DESC")
          params)))

    (define (find-file-by-id cfg id)
      (let ((rs (alist-rows cfg
                  (string-append
                    "SELECT id::text AS id, name, mime, size::text AS size, "
                    "       sha256, visibility, note "
                    "FROM files WHERE id = $1")
                  (list id))))
        (cond ((pair? rs) (car rs)) (else #f))))

    (define (find-file-by-public-name cfg name)
      (let ((rs (alist-rows cfg
                  (string-append
                    "SELECT id::text AS id, name, mime, size::text AS size, "
                    "       sha256, visibility, note "
                    "FROM files WHERE visibility = 'public' AND name = $1 "
                    "LIMIT 1")
                  (list name))))
        (cond ((pair? rs) (car rs)) (else #f))))

    (define (insert-file! cfg name mime size sha vis note)
      (with-db cfg
        (lambda (c)
          (pg-query c
            (string-append
              "INSERT INTO files (name, mime, size, sha256, visibility, note) "
              "VALUES ($1, $2, $3, $4, $5, $6)")
            name mime size sha vis note))))

    (define (delete-file-row! cfg id)
      (exec cfg "DELETE FROM files WHERE id = $1" (list id)))

    (define (toggle-file-visibility! cfg id)
      (exec cfg
            (string-append
              "UPDATE files SET visibility = CASE visibility "
              "WHEN 'public' THEN 'private' ELSE 'public' END "
              "WHERE id = $1")
            (list id)))

    (define (update-note! cfg id note)
      (exec cfg "UPDATE files SET note = $1 WHERE id = $2" (list note id)))

    ;; ---- DB helpers ----
    (define (alist-rows cfg sql . maybe-params)
      (let ((params (if (null? maybe-params) '() (car maybe-params))))
        (with-db cfg
          (lambda (c)
            (pg-result->alist-list
              (cond ((null? params) (pg-query c sql))
                    (else (pg-query c (pg-format-sql sql params)))))))))
    (define (exec cfg sql . maybe-params)
      (let ((params (if (null? maybe-params) '() (car maybe-params))))
        (with-db cfg
          (lambda (c)
            (cond ((null? params) (pg-exec c sql))
                  (else (pg-exec c (pg-format-sql sql params))))))))

    ;; ---- formatting ----

    (define (format-size bytes)
      (cond
        ((< bytes 1024) (string-append (number->string bytes) " B"))
        ((< bytes (* 1024 1024))
         (string-append (number->string (quotient bytes 1024)) " KB"))
        (else
         (let* ((mb (/ bytes 1024.0 1024.0))
                (rounded (/ (round (* mb 10)) 10)))
           (string-append (number->string (inexact rounded)) " MB")))))

    ;; ---- views ----

    (define (row-field r k) (let ((p (assoc k r))) (if p (cdr p) "")))

    (define (upload-form-sxml err)
      `(form (@ (method "post") (action "/files")
                (enctype "multipart/form-data")
                (class "feed-new files-add"))
         (h2 "Upload")
         ,@(if err `((p (@ (class "error")) ,err)) '())
         (label (@ (class "files-pick")) "File"
           (input (@ (type "file") (name "file") (required #t))))
         (label "Name (optional, public files must be unique)"
           (input (@ (type "text") (name "name")
                     (placeholder "defaults to uploaded filename"))))
         (label "Note (optional)"
           (input (@ (type "text") (name "note")
                     (placeholder "description"))))
         (label (@ (class "files-public"))
           (input (@ (type "checkbox") (name "public") (value "1")))
           " Make public (served at /f/" ,(raw "&lt;name&gt;") ")")
         (button (@ (type "submit")) "Upload")))

    (define (filter-form-sxml q vis)
      `(form (@ (method "get") (action "/files") (class "feed-filters"))
         (input (@ (type "search") (name "q")
                   (placeholder "search name + note")
                   (value ,(or q ""))))
         (select (@ (name "vis"))
           (option (@ (value "")) "all")
           (option (@ (value "public")
                      (selected ,(if (equal? vis "public") #t #f)))
                   "public")
           (option (@ (value "private")
                      (selected ,(if (equal? vis "private") #t #f)))
                   "private"))
         (button (@ (type "submit")) "Apply")))

    (define (file-row-sxml f)
      (let* ((id    (row-field f "id"))
             (name  (row-field f "name"))
             (mime  (row-field f "mime"))
             (size  (string->number (row-field f "size")))
             (vis   (row-field f "visibility"))
             (note  (row-field f "note"))
             (created (row-field f "created"))
             (public? (string=? vis "public"))
             (thumb
               (cond
                 ((and public? (image-mime? mime))
                  `(img (@ (src ,(string-append "/f/" name))
                           (alt "") (loading "lazy"))))
                 ((image-mime? mime)
                  `(img (@ (src ,(string-append "/files/" id))
                           (alt "") (loading "lazy"))))
                 (else
                  `(span (@ (class "ext"))
                         ,(mime-tag mime name)))))
             (name-link
               (cond
                 (public?
                  `(a (@ (href ,(string-append "/f/" name))
                         (target "_blank") (rel "noopener"))
                      ,name))
                 (else
                  `(a (@ (href ,(string-append "/files/" id))) ,name)))))
        `(li (@ (class ,(string-append "file"
                                       (if public? " public" " private"))))
           (div (@ (class "thumb")) ,thumb)
           (div (@ (class "meta"))
             (div (@ (class "name")) ,name-link)
             (div (@ (class "info"))
               (span (@ (class ,(if public? "badge open" "badge closed")))
                     ,(if public? "public" "private"))
               " " (span (@ (class "sz")) ,(format-size (or size 0)))
               " " (span (@ (class "dt")) ,created))
             ,@(if (string=? note "") '()
                   `((div (@ (class "note")) ,note)))
             (div (@ (class "acts"))
               (form (@ (method "post")
                        (action ,(string-append "/files/" id "/visibility"))
                        (class "inline"))
                 (button (@ (class "linkish"))
                   ,(if public? "make private" "make public")))
               " "
               (form (@ (method "post")
                        (action ,(string-append "/files/" id "/delete"))
                        (class "inline")
                        (data-confirm "Delete this file?"))
                 (button (@ (class "linkish danger")) "delete")))))))

    (define (files-list-sxml files)
      (cond
        ((null? files) `(p (@ (class "empty")) "No files yet."))
        (else
         `(ul (@ (class "files-list"))
              ,@(map file-row-sxml files)))))

    (define (mime-tag mime name)
      ;; Short tag shown when there's no preview thumbnail.
      (cond
        ((not (string? name)) "FILE")
        (else
         (let ((dot (string-index-right name #\.)))
           (cond
             ((not dot) "FILE")
             (else
              (string-upcase
                (substring name (+ dot 1) (string-length name)))))))))

    (define (render-main req auth cfg q vis err)
      (let* ((files (list-files cfg q vis))
             (body
               `((header (@ (class "feeds-head")) (h1 "Files"))
                 ,(filter-form-sxml q vis)
                 ,(upload-form-sxml err)
                 ,(files-list-sxml files))))
        (html-response
          (render-page req auth
                       '((title  . "Files")
                         (active . files)
                         (body-class . "feeds-page"))
                       (html->string body)))))

    ;; ---- request helpers ----

    (define (param-or req name default)
      (let ((p (assoc name (url-query-params (http-request-url req)))))
        (cond ((and p (string? (cdr p))) (percent-decode (cdr p)))
              (else default))))

    (define (find-part parts name)
      (let loop ((ps parts))
        (cond
          ((null? ps) #f)
          ((string=? (part-ref (car ps) 'name) name) (car ps))
          (else (loop (cdr ps))))))

    (define (part-text parts name)
      ;; Decode the named part's body bytevector as UTF-8 text. Returns
      ;; #f if the part is missing.
      (let ((p (find-part parts name)))
        (cond (p (bv->utf8-string (part-ref p 'body)))
              (else #f))))

    ;; ---- upload pipeline (broken into stages to keep nesting shallow) ----

    (define (do-upload-store! cfg files-dir safe-name mime data note public?)
      (let* ((size (bytevector-length data))
             (sha  (sha256-hex data))
             (vis  (cond (public? "public") (else "private"))))
        (guard
            (exn
             (#t
              (let* ((out (open-output-string))
                     (_   (display exn out))
                     (msg (get-output-string out)))
                ;; Full exception goes to the log; the client sees a
                ;; generic message so we don't leak DB column names,
                ;; paths, or stack details to the browser.
                (log-error "files"
                  (string-append "upload " safe-name " failed: " msg))
                (render-error 400 "Upload failed."))))
          (write-blob! files-dir sha data)
          (insert-file! cfg safe-name mime size sha vis note)
          (log-info "files"
            (string-append "uploaded " safe-name
                           " (" (number->string size) " B, "
                           vis ")"))
          (make-http-response 302
                              (list (cons "Location" "/files")) ""))))

    (define (do-upload-parsed cfg files-dir file-part name-override note public?)
      (let* ((raw-name (or (part-ref file-part 'filename) ""))
             (chosen   (cond ((not (string=? name-override ""))
                              name-override)
                             (else raw-name)))
             (safe (safe-public-name chosen)))
        (cond
          ((not safe) (render-error 400 "Invalid filename."))
          (else
           (let* ((mime-hdr (part-ref file-part 'content-type))
                  (mime     (cond
                              ((and mime-hdr (not (string=? mime-hdr "")))
                               mime-hdr)
                              (else (mime-from-name safe))))
                  ;; The bytevector parser returns the part body as a
                  ;; bytevector already, so no per-byte conversion loop.
                  (data (part-ref file-part 'body)))
             (do-upload-store! cfg files-dir safe mime data note public?))))))

    (define (handle-upload cfg files-dir req)
      (let* ((ct       (http-request-header req "Content-Type"))
             (boundary (multipart-boundary ct))
             ;; Binary uploads must be read as raw bytes: the legacy
             ;; http-request-body decodes through UTF-8 and corrupts
             ;; anything non-textual.
             (body-bv  (or (http-request-body-bytes req) (bytevector))))
        (cond
          ((not boundary)
           (render-error 400 "Expected multipart/form-data."))
          ((> (bytevector-length body-bv) max-upload-bytes)
           (render-error 413 "Upload too large (limit is 25 MB)."))
          (else
           (let* ((parts     (parse-multipart-bytes body-bv boundary))
                  (file-part (find-part parts "file"))
                  (name-override (string-trim-both
                                   (or (part-text parts "name") "")))
                  (note      (string-trim-both
                                   (or (part-text parts "note") "")))
                  (public?   (string=? (or (part-text parts "public") "")
                                       "1")))
             (cond
               ((not file-part) (render-error 400 "No file uploaded."))
               (else
                (do-upload-parsed cfg files-dir file-part
                                  name-override note public?))))))))

    ;; ---- routes ----

    (define (install-files-routes! router cfg auth files-dir)

      (ensure-files-dir! files-dir)

      ;; ----- private list / admin -----
      (router-add! router "GET" "/files"
        (require-auth auth
          (lambda (req params)
            (let ((q   (param-or req "q" ""))
                  (vis (param-or req "vis" "")))
              (render-main req auth cfg
                           (cond ((string=? q "") #f) (else q))
                           (cond ((string=? vis "") #f) (else vis))
                           #f)))))

      ;; ----- upload -----
      (router-add! router "POST" "/files"
        (require-auth auth
          (lambda (req params)
            (handle-upload cfg files-dir req))))

      ;; ----- private download -----
      (router-add! router "GET" "/files/:id"
        (require-auth auth
          (lambda (req params)
            (let* ((id (string->number (params-ref params "id")))
                   (f  (and id (find-file-by-id cfg id))))
              (cond
                ((not f) (render-error 404 "File not found."))
                (else
                 (let* ((data (read-blob files-dir (row-field f "sha256")))
                        (mime (row-field f "mime"))
                        (name (row-field f "name")))
                   (make-http-response 200
                     (list (cons "Content-Type" (safe-content-type mime name))
                           (cons "Content-Disposition"
                                 (content-disposition mime name)))
                     data))))))))

      ;; ----- delete -----
      (router-add! router "POST" "/files/:id/delete"
        (require-auth auth
          (lambda (req params)
            (let* ((id (string->number (params-ref params "id")))
                   (f  (and id (find-file-by-id cfg id))))
              (when f
                (let ((sha (row-field f "sha256")))
                  (delete-file-row! cfg id)
                  ;; If no row references the blob anymore, drop it.
                  (when (= 0 (refcount-blob cfg sha))
                    (let ((path (blob-path files-dir sha)))
                      (when (file-exists? path)
                        (guard (exn (#t #f)) (delete-file path)))))))
              (make-http-response 302
                (list (cons "Location" "/files")) "")))))

      ;; ----- toggle visibility -----
      (router-add! router "POST" "/files/:id/visibility"
        (require-auth auth
          (lambda (req params)
            (let ((id (string->number (params-ref params "id"))))
              (when id
                (guard (exn (#t #f))
                  (toggle-file-visibility! cfg id)))
              (make-http-response 302
                (list (cons "Location" "/files")) "")))))

      ;; ----- public hosting: /f/<name> -----
      (router-add! router "GET" "/f/:name"
        (lambda (req params)
          (let* ((raw  (params-ref params "name"))
                 (name (and raw (percent-decode raw)))
                 (safe (and name (safe-public-name name))))
            (cond
              ((not safe) (render-error 404 "Not found."))
              (else
               (let ((f (find-file-by-public-name cfg safe)))
                 (cond
                   ((not f) (render-error 404 "Not found."))
                   (else
                    (let* ((data (read-blob files-dir
                                             (row-field f "sha256")))
                           (mime (row-field f "mime"))
                           (name (row-field f "name")))
                      (make-http-response 200
                        (list (cons "Content-Type" (safe-content-type mime name))
                              (cons "Content-Disposition"
                                    (content-disposition mime name))
                              (cons "Cache-Control" "public, max-age=3600"))
                        data)))))))))))

))
