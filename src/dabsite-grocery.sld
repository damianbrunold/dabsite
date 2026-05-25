(define-library (dabsite grocery)
  (import (scheme base)
          (scheme write)
          (srfi 1)
          (srfi 13)
          (scm database postgres)
          (scm net http request)
          (scm net http response)
          (scm net http route)
          (scm net http forms)
          (scm html builder)
          (dabsite db)
          (dabsite util)
          (dabsite auth)
          (dabsite views))
  (export install-grocery-routes!)
  (begin

    ;; ============================================================
    ;; Model:
    ;;   items     — name-only catalog
    ;;   shops     — named, with a curated, ordered subset of items
    ;;   lists     — one per shopping trip, bound to a shop (or NULL =
    ;;               default shop: all items, alphabetical)
    ;;   entries   — qty + bought flag, unique (list, item)
    ;; ============================================================

    (define (string-split-comma s)
      (let ((n (string-length s)))
        (let loop ((i 0) (start 0) (acc '()))
          (cond
            ((= i n)
             (reverse (cons (substring s start n) acc)))
            ((char=? (string-ref s i) #\,)
             (loop (+ i 1) (+ i 1)
                   (cons (substring s start i) acc)))
            (else (loop (+ i 1) start acc))))))

    ;; ---- items ----

    (define (list-items cfg)
      (alist-rows cfg
        "SELECT id::text AS id, name FROM grocery_items ORDER BY lower(name)"))

    (define (create-item! cfg name)
      ;; Inserts the item and (atomically) appends it to every existing
      ;; shop's order list. A duplicate name is a silent no-op.
      (exec cfg
        (string-append
          "WITH ins AS ("
          "  INSERT INTO grocery_items (name) VALUES ($1) "
          "  ON CONFLICT (name) DO NOTHING RETURNING id) "
          "INSERT INTO grocery_shop_items (shop_id, item_id, position) "
          "SELECT s.id, i.id, "
          "  COALESCE((SELECT MAX(position) FROM grocery_shop_items "
          "            WHERE shop_id = s.id), 0) + 1 "
          "FROM grocery_shops s, ins i")
        (list name)))

    (define (delete-item! cfg id)
      (exec cfg "DELETE FROM grocery_items WHERE id = $1" (list id)))

    ;; ---- shops ----

    (define (list-shops cfg)
      (alist-rows cfg
        "SELECT id::text AS id, name FROM grocery_shops ORDER BY lower(name)"))

    (define (find-shop cfg id)
      (let ((rs (alist-rows cfg
                  "SELECT id::text AS id, name FROM grocery_shops WHERE id = $1"
                  (list id))))
        (and (pair? rs) (car rs))))

    (define (create-shop! cfg name)
      ;; Inserts the shop and (atomically) seeds its order list with
      ;; every existing item, alphabetically.
      (exec cfg
        (string-append
          "WITH ins AS ("
          "  INSERT INTO grocery_shops (name) VALUES ($1) "
          "  ON CONFLICT (name) DO NOTHING RETURNING id) "
          "INSERT INTO grocery_shop_items (shop_id, item_id, position) "
          "SELECT s.id, i.id, "
          "       row_number() OVER (ORDER BY lower(i.name)) "
          "FROM ins s, grocery_items i")
        (list name)))

    (define (delete-shop! cfg id)
      (exec cfg "DELETE FROM grocery_shops WHERE id = $1" (list id)))

    (define (rename-shop! cfg id name)
      (exec cfg "UPDATE grocery_shops SET name = $1 WHERE id = $2"
            (list name id)))

    ;; Items in a shop, in their saved position. Default shop (id #f)
    ;; returns every item alphabetically.
    (define (shop-items cfg shop-id)
      (cond
        ((not shop-id)
         (list-items cfg))
        (else
         (alist-rows cfg
           (string-append
             "SELECT i.id::text AS id, i.name, "
             "       si.position::text AS position "
             "FROM grocery_shop_items si "
             "JOIN grocery_items i ON i.id = si.item_id "
             "WHERE si.shop_id = $1 "
             "ORDER BY si.position, lower(i.name)")
           (list shop-id)))))

    (define (items-not-in-shop cfg shop-id)
      (alist-rows cfg
        (string-append
          "SELECT i.id::text AS id, i.name FROM grocery_items i "
          "WHERE NOT EXISTS (SELECT 1 FROM grocery_shop_items si "
          "  WHERE si.shop_id = $1 AND si.item_id = i.id) "
          "ORDER BY lower(i.name)")
        (list shop-id)))

    (define (add-item-to-shop! cfg shop-id item-id)
      ;; Append at end: position = max(pos)+1.
      (exec cfg
        (string-append
          "INSERT INTO grocery_shop_items (shop_id, item_id, position) "
          "VALUES ($1, $2, "
          "(SELECT COALESCE(MAX(position), 0) + 1 "
          " FROM grocery_shop_items WHERE shop_id = $1)) "
          "ON CONFLICT DO NOTHING")
        (list shop-id item-id)))

    (define (remove-item-from-shop! cfg shop-id item-id)
      (exec cfg
            "DELETE FROM grocery_shop_items WHERE shop_id = $1 AND item_id = $2"
            (list shop-id item-id)))

    (define (set-shop-order! cfg shop-id item-ids)
      ;; Replaces positions of all listed items with 1..N. Items not in
      ;; the list are left untouched (so a stale ordering payload won't
      ;; nuke the order).
      (with-db cfg
        (lambda (c)
          (pg-exec c "BEGIN")
          (guard (exn (#t
                       (guard (e (#t #f)) (pg-exec c "ROLLBACK"))
                       (raise exn)))
            (let loop ((ids item-ids) (pos 1))
              (cond
                ((null? ids) #t)
                (else
                 (pg-exec c
                   "UPDATE grocery_shop_items SET position = $1 WHERE shop_id = $2 AND item_id = $3"
                   pos shop-id (car ids))
                 (loop (cdr ids) (+ pos 1)))))
            (pg-exec c "COMMIT")))))

    (define (swap-shop-positions! cfg shop-id item-id direction)
      ;; direction: 'up or 'down. Swaps position with the neighbour.
      (let* ((op  (case direction ((up) "<") (else ">")))
             (ord (case direction ((up) "DESC") (else "ASC")))
             (rs  (rows cfg
                    (string-append
                      "WITH me AS (SELECT position FROM grocery_shop_items "
                      "            WHERE shop_id = $1 AND item_id = $2), "
                      "  nbr AS (SELECT item_id, position FROM grocery_shop_items "
                      "          WHERE shop_id = $1 "
                      "          AND position " op " (SELECT position FROM me) "
                      "          ORDER BY position " ord " LIMIT 1) "
                      "SELECT (SELECT position FROM me)::text, "
                      "       (SELECT item_id::text FROM nbr), "
                      "       (SELECT position FROM nbr)::text")
                    (list shop-id item-id))))
        (cond
          ((null? rs) #f)
          (else
           (let* ((row (car rs))
                  (my-pos    (vector-ref row 0))
                  (nbr-id    (vector-ref row 1))
                  (nbr-pos   (vector-ref row 2)))
             (when (and my-pos nbr-id nbr-pos)
               ;; Use a temporary -1 sentinel to avoid colliding with
               ;; UNIQUE(shop_id, position) would be — but we have no
               ;; uniqueness on position, so just swap.
               (exec cfg
                 (string-append
                   "UPDATE grocery_shop_items SET position = $1 "
                   " WHERE shop_id = $2 AND item_id = $3; "
                   "UPDATE grocery_shop_items SET position = $4 "
                   " WHERE shop_id = $2 AND item_id = $5")
                 (list (string->number nbr-pos) shop-id item-id
                       (string->number my-pos) (string->number nbr-id)))))))))

    ;; ---- lists ----

    (define (list-lists cfg)
      (alist-rows cfg
        (string-append
          "SELECT l.id::text AS id, "
          "       COALESCE(s.name, '(default)') AS shop_name, "
          "       l.shop_id::text AS shop_id, "
          "       to_char(l.created_at, 'YYYY-MM-DD HH24:MI') AS created, "
          "       (SELECT COUNT(*)::text FROM grocery_list_entries e "
          "        WHERE e.list_id = l.id) AS n_entries, "
          "       (SELECT COUNT(*)::text FROM grocery_list_entries e "
          "        WHERE e.list_id = l.id AND NOT e.bought) AS n_open "
          "FROM grocery_lists l LEFT JOIN grocery_shops s ON s.id = l.shop_id "
          "ORDER BY l.created_at DESC")))

    (define (find-list cfg id)
      (let ((rs (alist-rows cfg
                  (string-append
                    "SELECT l.id::text AS id, l.shop_id::text AS shop_id, "
                    "       COALESCE(s.name, '(default)') AS shop_name, "
                    "       to_char(l.created_at, 'YYYY-MM-DD HH24:MI') AS created "
                    "FROM grocery_lists l "
                    "LEFT JOIN grocery_shops s ON s.id = l.shop_id "
                    "WHERE l.id = $1")
                  (list id))))
        (and (pair? rs) (car rs))))

    (define (create-list! cfg shop-id)
      ;; shop-id may be #f (default shop). pg-format-sql now renders
      ;; #f as FALSE, so coerce missing shop-id to the 'null sentinel
      ;; that produces SQL NULL.
      (with-db cfg
        (lambda (c)
          (let ((res (pg-query c
                       "INSERT INTO grocery_lists (shop_id) VALUES ($1) RETURNING id"
                       (or shop-id 'null))))
            (string->number (vector-ref (car (pg-result-rows res)) 0))))))

    (define (delete-list! cfg id)
      (exec cfg "DELETE FROM grocery_lists WHERE id = $1" (list id)))

    ;; Entries on a list, ordered per the list's shop. For the default
    ;; shop, alphabetical.
    (define (list-entries cfg list-id shop-id)
      (cond
        (shop-id
         (alist-rows cfg
           (string-append
             "SELECT e.id::text AS id, i.id::text AS item_id, i.name, "
             "       e.qty::text AS qty, "
             "       CASE WHEN e.bought THEN 'yes' ELSE 'no' END AS bought "
             "FROM grocery_list_entries e "
             "JOIN grocery_items i ON i.id = e.item_id "
             "WHERE e.list_id = $1 "
             "ORDER BY (SELECT si.position FROM grocery_shop_items si "
             "          WHERE si.shop_id = $2 AND si.item_id = e.item_id), "
             "         lower(i.name)")
           (list list-id shop-id)))
        (else
         (alist-rows cfg
           (string-append
             "SELECT e.id::text AS id, i.id::text AS item_id, i.name, "
             "       e.qty::text AS qty, "
             "       CASE WHEN e.bought THEN 'yes' ELSE 'no' END AS bought "
             "FROM grocery_list_entries e "
             "JOIN grocery_items i ON i.id = e.item_id "
             "WHERE e.list_id = $1 "
             "ORDER BY lower(i.name)")
           (list list-id)))))

    (define (add-or-inc! cfg list-id item-id)
      (exec cfg
        (string-append
          "INSERT INTO grocery_list_entries (list_id, item_id, qty) "
          "VALUES ($1, $2, 1) "
          "ON CONFLICT (list_id, item_id) "
          "DO UPDATE SET qty = grocery_list_entries.qty + 1, bought = false")
        (list list-id item-id)))

    (define (dec-entry! cfg entry-id)
      ;; -1; if it reaches 0, delete the row.
      (exec cfg
        (string-append
          "WITH upd AS (UPDATE grocery_list_entries SET qty = qty - 1 "
          "             WHERE id = $1 "
          "             RETURNING id, qty) "
          "DELETE FROM grocery_list_entries WHERE id IN "
          "(SELECT id FROM upd WHERE qty <= 0)")
        (list entry-id)))

    (define (toggle-bought! cfg entry-id)
      (exec cfg
            "UPDATE grocery_list_entries SET bought = NOT bought WHERE id = $1"
            (list entry-id)))

    (define (delete-entry! cfg entry-id)
      (exec cfg "DELETE FROM grocery_list_entries WHERE id = $1"
            (list entry-id)))

    (define (clear-bought! cfg list-id)
      (exec cfg
            "DELETE FROM grocery_list_entries WHERE list_id = $1 AND bought = true"
            (list list-id)))

    ;; ============================================================
    ;; Views
    ;; ============================================================

    (define (page-sxml req auth title active body)
      (html-response
        (render-page req auth
                     (list (cons 'title title)
                           (cons 'active active)
                           (cons 'body-class "feeds-page"))
                     (html->string body))))

    ;; ---- landing ----

    (define (list-row-sxml l)
      (let ((id     (row-field l "id"))
            (shop   (row-field l "shop_name"))
            (n-all  (row-field l "n_entries"))
            (n-open (row-field l "n_open"))
            (when-s (row-field l "created")))
        `(li
           (a (@ (class "row")
                 (href ,(string-append "/grocery/lists/" id)))
              (span (@ (class "name")) ,shop)
              (span (@ (class "meta"))
                    ,n-open " open · " ,n-all " total · " ,when-s))
           " "
           (form (@ (method "post")
                    (action ,(string-append "/grocery/lists/" id "/delete"))
                    (class "inline rm")
                    (data-confirm "Delete this list?"))
             (button (@ (class "linkish danger")) "delete")))))

    (define (render-landing req auth cfg)
      (let* ((lists (list-lists cfg))
             (shops (list-shops cfg))
             (body
               `((header (@ (class "feeds-head"))
                   (h1 "Grocery")
                   (a (@ (class "admin-link") (href "/grocery/items")) "items")
                   " "
                   (a (@ (class "admin-link") (href "/grocery/shops")) "shops"))
                 (form (@ (method "post") (action "/grocery/lists")
                          (class "grocery-newlist"))
                   (label "Shop "
                     (select (@ (name "shop"))
                       (option (@ (value "")) "default (alphabetical)")
                       ,@(map (lambda (s)
                                `(option (@ (value ,(row-field s "id")))
                                         ,(row-field s "name")))
                              shops)))
                   (button (@ (type "submit")) "New list"))
                 ,(if (null? lists)
                      `(p (@ (class "empty")) "No shopping lists yet.")
                      `(ul (@ (class "grocery-lists"))
                           ,@(map list-row-sxml lists))))))
        (page-sxml req auth "Grocery" 'grocery body)))

    ;; ---- one list ----

    (define (entry-sxml list-id e)
      (let* ((eid    (row-field e "id"))
             (name   (row-field e "name"))
             (qty    (or (string->number (row-field e "qty")) 1))
             (bought (string=? (row-field e "bought") "yes"))
             (lid    (number->string list-id))
             (action-base (string-append "/grocery/lists/" lid "/entries/" eid)))
        `(li (@ (class ,(if bought "bought" "open")))
           (form (@ (method "post")
                    (action ,(string-append action-base "/toggle"))
                    (class "inline toggle"))
             (button (@ (type "submit") (class "shop-toggle"))
               (span (@ (class "check") (aria-hidden "true"))
                     ,(raw (if bought "&#x2714;" "&nbsp;")))
               (span (@ (class "name"))
                     ,name
                     ,@(if (> qty 1)
                           `(" " (span (@ (class "qty"))
                                       ,(string-append
                                          "×" (number->string qty))))
                           '()))))
           (form (@ (method "post")
                    (action ,(string-append action-base "/dec"))
                    (class "inline rm"))
             (button (@ (class "linkish") (title "one less")) "−"))
           (form (@ (method "post")
                    (action ,(string-append action-base "/delete"))
                    (class "inline rm"))
             (button (@ (class "linkish") (title "remove")) "×")))))

    (define (entries-sxml list-id entries)
      `(section (@ (class "grocery-list"))
         (h2 "Shopping list")
         ,(if (null? entries)
              `(p (@ (class "empty")) "Empty. Tap items below to add.")
              `(ul (@ (class "grocery-shopping"))
                   ,@(map (lambda (e) (entry-sxml list-id e)) entries)))))

    (define (catalog-item-sxml list-id entry-by-item i)
      (let* ((id   (row-field i "id"))
             (name (row-field i "name"))
             (e    (assoc id entry-by-item))
             (qty  (if e (or (string->number (row-field (cdr e) "qty")) 0) 0))
             (lid  (number->string list-id)))
        `(li (form (@ (method "post")
                      (action ,(string-append "/grocery/lists/" lid "/add/" id))
                      (class "inline add"))
               (button (@ (type "submit") (class "add-btn"))
                 "+ " (span (@ (class "name")) ,name)
                 ,@(if (> qty 0)
                       `(" " (span (@ (class "qty"))
                                   ,(string-append "·" (number->string qty))))
                       '()))))))

    (define (catalog-sxml list-id catalog entry-by-item)
      `(section (@ (class "grocery-catalog"))
         (h2 "Add items")
         ,(if (null? catalog)
              `(p (@ (class "empty"))
                  "This shop has no items yet. "
                  "Add some on the "
                  (a (@ (href "/grocery/shops")) "shops")
                  " page.")
              `(ul (@ (class "grocery-items"))
                   ,@(map (lambda (i)
                            (catalog-item-sxml list-id entry-by-item i))
                          catalog)))))

    (define (render-list req auth cfg list-id)
      (let ((l (find-list cfg list-id)))
        (cond
          ((not l) (render-error 404 "List not found."))
          (else
           (let* ((shop-id-str (row-field l "shop_id"))
                  (shop-id (and (string? shop-id-str)
                                (not (string=? shop-id-str ""))
                                (string->number shop-id-str)))
                  (entries (list-entries cfg list-id shop-id))
                  (catalog (shop-items cfg shop-id))
                  (entry-by-item-id
                    (map (lambda (e) (cons (row-field e "item_id") e))
                         entries))
                  (lid (number->string list-id))
                  (body
                    `((header (@ (class "feeds-head"))
                        (h1 ,(row-field l "shop_name"))
                        " "
                        (a (@ (class "admin-link") (href "/grocery"))
                           ,(raw "← lists"))
                        " "
                        (form (@ (method "post")
                                 (action ,(string-append "/grocery/lists/" lid "/clear-bought"))
                                 (class "inline"))
                          (button (@ (class "linkish")) "remove bought"))
                        " "
                        (form (@ (method "post")
                                 (action ,(string-append "/grocery/lists/" lid "/delete"))
                                 (class "inline")
                                 (data-confirm "Delete this list?"))
                          (button (@ (class "linkish danger")) "delete list")))
                      ,(entries-sxml list-id entries)
                      ,(catalog-sxml list-id catalog entry-by-item-id))))
             (page-sxml req auth "Grocery" 'grocery body))))))

    ;; ---- items admin ----

    (define (item-row-sxml i)
      (let ((id   (row-field i "id"))
            (name (row-field i "name")))
        `(li (span (@ (class "add-btn static")) ,name)
             (form (@ (method "post")
                      (action ,(string-append "/grocery/items/" id "/delete"))
                      (class "inline rm")
                      (data-confirm "Delete this item from the catalog?"))
               (button (@ (class "linkish") (title "delete")) "×")))))

    (define (render-items req auth cfg)
      (let* ((items (list-items cfg))
             (body
               `((header (@ (class "feeds-head"))
                   (h1 "Items")
                   (a (@ (class "admin-link") (href "/grocery"))
                      ,(raw "← back")))
                 (form (@ (method "post") (action "/grocery/items")
                          (class "grocery-new"))
                   (input (@ (type "text") (name "name") (required #t)
                             (maxlength "100") (placeholder "new item")
                             (autofocus #t)))
                   (button (@ (type "submit")) "Add"))
                 ,(cond
                    ((null? items)
                     `(p (@ (class "empty")) "No items yet."))
                    (else
                     `(ul (@ (class "grocery-items"))
                          ,@(map item-row-sxml items)))))))
        (page-sxml req auth "Items" 'grocery body)))

    ;; ---- shops admin ----

    (define (shop-row-sxml s)
      (let ((id   (row-field s "id"))
            (name (row-field s "name")))
        `(li (a (@ (class "add-btn")
                   (href ,(string-append "/grocery/shops/" id)))
                ,name " — edit order")
             (form (@ (method "post")
                      (action ,(string-append "/grocery/shops/" id "/delete"))
                      (class "inline rm")
                      (data-confirm "Delete this shop and its lists?"))
               (button (@ (class "linkish") (title "delete")) "×")))))

    (define (render-shops req auth cfg)
      (let* ((shops (list-shops cfg))
             (body
               `((header (@ (class "feeds-head"))
                   (h1 "Shops")
                   (a (@ (class "admin-link") (href "/grocery"))
                      ,(raw "← back")))
                 (form (@ (method "post") (action "/grocery/shops")
                          (class "grocery-new"))
                   (input (@ (type "text") (name "name") (required #t)
                             (maxlength "60") (placeholder "new shop")))
                   (button (@ (type "submit")) "Add"))
                 (ul (@ (class "grocery-items"))
                   (li (span (@ (class "add-btn static")) "(default)")
                       (span (@ (class "hint"))
                             "all items, alphabetical"))
                   ,@(map shop-row-sxml shops)))))
        (page-sxml req auth "Shops" 'grocery body)))

    ;; ---- one shop's order ----

    (define (shop-item-row-sxml sid iid name)
      (let ((base (string-append "/grocery/shops/" sid "/items/" iid)))
        `(li (@ (data-item-id ,iid))
           (button (@ (type "button") (class "drag-handle")
                      (aria-label "drag to reorder")) "⠿")
           (span (@ (class "name")) ,name)
           (form (@ (method "post") (action ,(string-append base "/up"))
                    (class "inline no-drag"))
             (button (@ (class "linkish") (title "up")) "↑"))
           (form (@ (method "post") (action ,(string-append base "/down"))
                    (class "inline no-drag"))
             (button (@ (class "linkish") (title "down")) "↓"))
           (form (@ (method "post") (action ,(string-append base "/remove"))
                    (class "inline rm"))
             (button (@ (class "linkish") (title "remove")) "×")))))

    (define (shop-available-row-sxml sid i)
      (let ((iid  (row-field i "id"))
            (name (row-field i "name")))
        `(li (form (@ (method "post")
                      (action ,(string-append "/grocery/shops/" sid
                                              "/items/" iid "/add"))
                      (class "inline add"))
               (button (@ (type "submit") (class "add-btn"))
                 "+ " (span (@ (class "name")) ,name))))))

    (define (render-shop req auth cfg shop-id)
      (let ((s (find-shop cfg shop-id)))
        (cond
          ((not s) (render-error 404 "Shop not found."))
          (else
           (let* ((in-shop (shop-items cfg shop-id))
                  (others  (items-not-in-shop cfg shop-id))
                  (sid     (number->string shop-id))
                  (body
                    `((header (@ (class "feeds-head"))
                        (h1 ,(row-field s "name"))
                        " "
                        (a (@ (class "admin-link") (href "/grocery/shops"))
                           ,(raw "← shops")))
                      (form (@ (method "post")
                               (action ,(string-append "/grocery/shops/" sid "/rename"))
                               (class "grocery-new"))
                        (input (@ (type "text") (name "name") (required #t)
                                  (maxlength "60") (value ,(row-field s "name"))))
                        (button (@ (type "submit")) "Rename"))
                      (section (@ (class "grocery-list"))
                        (h2 "Items in shop order")
                        ,(cond
                           ((null? in-shop)
                            `(p (@ (class "empty"))
                                "No items yet. Add from below."))
                           (else
                            `(ul (@ (class "grocery-order") (data-shop-id ,sid))
                                 ,@(map (lambda (i)
                                          (shop-item-row-sxml
                                            sid
                                            (row-field i "id")
                                            (row-field i "name")))
                                        in-shop)))))
                      (section (@ (class "grocery-catalog"))
                        (h2 "Available items")
                        ,(cond
                           ((null? others)
                            `(p (@ (class "empty"))
                                "All catalog items are in this shop."))
                           (else
                            `(ul (@ (class "grocery-items"))
                                 ,@(map (lambda (i)
                                          (shop-available-row-sxml sid i))
                                        others))))))))
             (page-sxml req auth "Shop order" 'grocery body))))))

    ;; ============================================================
    ;; Routes
    ;; ============================================================

    (define (redirect to)
      (make-http-response 302 (list (cons "Location" to)) ""))

    (define (install-grocery-routes! router cfg auth)

      ;; -- landing --
      (router-add! router "GET" "/grocery"
        (require-auth auth (lambda (req params) (render-landing req auth cfg))))

      ;; -- items admin --
      (router-add! router "GET" "/grocery/items"
        (require-auth auth (lambda (req params) (render-items req auth cfg))))

      (router-add! router "POST" "/grocery/items"
        (require-auth auth
          (lambda (req params)
            (let* ((form (parse-www-form (or (http-request-body req) "")))
                   (name (string-trim-both (form-ref form "name" ""))))
              (unless (string=? name "") (create-item! cfg name))
              (redirect "/grocery/items")))))

      (router-add! router "POST" "/grocery/items/:id/delete"
        (require-auth auth
          (lambda (req params)
            (let ((id (string->number (params-ref params "id"))))
              (when id (delete-item! cfg id))
              (redirect "/grocery/items")))))

      ;; -- shops admin --
      (router-add! router "GET" "/grocery/shops"
        (require-auth auth (lambda (req params) (render-shops req auth cfg))))

      (router-add! router "POST" "/grocery/shops"
        (require-auth auth
          (lambda (req params)
            (let* ((form (parse-www-form (or (http-request-body req) "")))
                   (name (string-trim-both (form-ref form "name" ""))))
              (unless (string=? name "") (create-shop! cfg name))
              (redirect "/grocery/shops")))))

      (router-add! router "POST" "/grocery/shops/:id/delete"
        (require-auth auth
          (lambda (req params)
            (let ((id (string->number (params-ref params "id"))))
              (when id (delete-shop! cfg id))
              (redirect "/grocery/shops")))))

      (router-add! router "POST" "/grocery/shops/:id/rename"
        (require-auth auth
          (lambda (req params)
            (let* ((id (string->number (params-ref params "id")))
                   (form (parse-www-form (or (http-request-body req) "")))
                   (name (string-trim-both (form-ref form "name" ""))))
              (when (and id (not (string=? name "")))
                (rename-shop! cfg id name))
              (redirect (string-append "/grocery/shops/"
                                       (number->string id)))))))

      ;; -- shop order --
      (router-add! router "GET" "/grocery/shops/:id"
        (require-auth auth
          (lambda (req params)
            (let ((id (string->number (params-ref params "id"))))
              (if id
                  (render-shop req auth cfg id)
                  (render-error 404 "Shop not found."))))))

      (router-add! router "POST" "/grocery/shops/:id/items/:item_id/add"
        (require-auth auth
          (lambda (req params)
            (let ((sid (string->number (params-ref params "id")))
                  (iid (string->number (params-ref params "item_id"))))
              (when (and sid iid) (add-item-to-shop! cfg sid iid))
              (redirect (string-append "/grocery/shops/"
                                       (number->string sid)))))))

      (router-add! router "POST" "/grocery/shops/:id/items/:item_id/remove"
        (require-auth auth
          (lambda (req params)
            (let ((sid (string->number (params-ref params "id")))
                  (iid (string->number (params-ref params "item_id"))))
              (when (and sid iid) (remove-item-from-shop! cfg sid iid))
              (redirect (string-append "/grocery/shops/"
                                       (number->string sid)))))))

      (router-add! router "POST" "/grocery/shops/:id/items/:item_id/up"
        (require-auth auth
          (lambda (req params)
            (let ((sid (string->number (params-ref params "id")))
                  (iid (string->number (params-ref params "item_id"))))
              (when (and sid iid) (swap-shop-positions! cfg sid iid 'up))
              (redirect (string-append "/grocery/shops/"
                                       (number->string sid)))))))

      (router-add! router "POST" "/grocery/shops/:id/reorder"
        (require-auth auth
          (lambda (req params)
            (let* ((sid (string->number (params-ref params "id")))
                   (form (parse-www-form (or (http-request-body req) "")))
                   (raw  (form-ref form "order" ""))
                   (parts (filter
                            (lambda (s) (not (string=? s "")))
                            (map string-trim-both (string-split-comma raw))))
                   (ids  (filter-map string->number parts)))
              (when (and sid (pair? ids)) (set-shop-order! cfg sid ids))
              (make-http-response 204 '() "")))))

      (router-add! router "POST" "/grocery/shops/:id/items/:item_id/down"
        (require-auth auth
          (lambda (req params)
            (let ((sid (string->number (params-ref params "id")))
                  (iid (string->number (params-ref params "item_id"))))
              (when (and sid iid) (swap-shop-positions! cfg sid iid 'down))
              (redirect (string-append "/grocery/shops/"
                                       (number->string sid)))))))

      ;; -- lists --
      (router-add! router "POST" "/grocery/lists"
        (require-auth auth
          (lambda (req params)
            (let* ((form (parse-www-form (or (http-request-body req) "")))
                   (raw  (form-ref form "shop" ""))
                   (sid  (and (not (string=? raw "")) (string->number raw)))
                   (id   (create-list! cfg sid)))
              (redirect (string-append "/grocery/lists/"
                                       (number->string id)))))))

      (router-add! router "GET" "/grocery/lists/:id"
        (require-auth auth
          (lambda (req params)
            (let ((id (string->number (params-ref params "id"))))
              (if id
                  (render-list req auth cfg id)
                  (render-error 404 "List not found."))))))

      (router-add! router "POST" "/grocery/lists/:id/delete"
        (require-auth auth
          (lambda (req params)
            (let ((id (string->number (params-ref params "id"))))
              (when id (delete-list! cfg id))
              (redirect "/grocery")))))

      (router-add! router "POST" "/grocery/lists/:id/clear-bought"
        (require-auth auth
          (lambda (req params)
            (let ((id (string->number (params-ref params "id"))))
              (when id (clear-bought! cfg id))
              (redirect (string-append "/grocery/lists/"
                                       (number->string id)))))))

      (router-add! router "POST" "/grocery/lists/:id/add/:item_id"
        (require-auth auth
          (lambda (req params)
            (let ((lid (string->number (params-ref params "id")))
                  (iid (string->number (params-ref params "item_id"))))
              (when (and lid iid) (add-or-inc! cfg lid iid))
              (redirect (string-append "/grocery/lists/"
                                       (number->string lid)))))))

      (router-add! router "POST" "/grocery/lists/:id/entries/:eid/toggle"
        (require-auth auth
          (lambda (req params)
            (let ((lid (string->number (params-ref params "id")))
                  (eid (string->number (params-ref params "eid"))))
              (when eid (toggle-bought! cfg eid))
              (redirect (string-append "/grocery/lists/"
                                       (number->string lid)))))))

      (router-add! router "POST" "/grocery/lists/:id/entries/:eid/dec"
        (require-auth auth
          (lambda (req params)
            (let ((lid (string->number (params-ref params "id")))
                  (eid (string->number (params-ref params "eid"))))
              (when eid (dec-entry! cfg eid))
              (redirect (string-append "/grocery/lists/"
                                       (number->string lid)))))))

      (router-add! router "POST" "/grocery/lists/:id/entries/:eid/delete"
        (require-auth auth
          (lambda (req params)
            (let ((lid (string->number (params-ref params "id")))
                  (eid (string->number (params-ref params "eid"))))
              (when eid (delete-entry! cfg eid))
              (redirect (string-append "/grocery/lists/"
                                       (number->string lid))))))))

))
