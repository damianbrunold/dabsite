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
          (scm html)
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

    (define (alist-rows cfg sql)
      (with-db cfg (lambda (c) (pg-result->alist-list (pg-query c sql)))))
    (define (exec cfg sql)
      (with-db cfg (lambda (c) (pg-exec c sql))))
    (define (rows cfg sql)
      (with-db cfg (lambda (c) (pg-result-rows (pg-query c sql)))))

    (define (row-field r k) (let ((p (assoc k r))) (if p (cdr p) "")))

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
          "  INSERT INTO grocery_items (name) VALUES ("
          (sql-quote-literal name)
          ")  ON CONFLICT (name) DO NOTHING RETURNING id) "
          "INSERT INTO grocery_shop_items (shop_id, item_id, position) "
          "SELECT s.id, i.id, "
          "  COALESCE((SELECT MAX(position) FROM grocery_shop_items "
          "            WHERE shop_id = s.id), 0) + 1 "
          "FROM grocery_shops s, ins i")))

    (define (delete-item! cfg id)
      (exec cfg (string-append
                  "DELETE FROM grocery_items WHERE id = "
                  (sql-quote-int id))))

    ;; ---- shops ----

    (define (list-shops cfg)
      (alist-rows cfg
        "SELECT id::text AS id, name FROM grocery_shops ORDER BY lower(name)"))

    (define (find-shop cfg id)
      (let ((rs (alist-rows cfg
                  (string-append
                    "SELECT id::text AS id, name FROM grocery_shops "
                    "WHERE id = " (sql-quote-int id)))))
        (cond ((pair? rs) (car rs)) (else #f))))

    (define (create-shop! cfg name)
      ;; Inserts the shop and (atomically) seeds its order list with
      ;; every existing item, alphabetically.
      (exec cfg
        (string-append
          "WITH ins AS ("
          "  INSERT INTO grocery_shops (name) VALUES ("
          (sql-quote-literal name)
          ")  ON CONFLICT (name) DO NOTHING RETURNING id) "
          "INSERT INTO grocery_shop_items (shop_id, item_id, position) "
          "SELECT s.id, i.id, "
          "       row_number() OVER (ORDER BY lower(i.name)) "
          "FROM ins s, grocery_items i")))

    (define (delete-shop! cfg id)
      (exec cfg (string-append
                  "DELETE FROM grocery_shops WHERE id = "
                  (sql-quote-int id))))

    (define (rename-shop! cfg id name)
      (exec cfg (string-append
                  "UPDATE grocery_shops SET name = "
                  (sql-quote-literal name)
                  " WHERE id = " (sql-quote-int id))))

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
             "WHERE si.shop_id = " (sql-quote-int shop-id) " "
             "ORDER BY si.position, lower(i.name)")))))

    (define (items-not-in-shop cfg shop-id)
      (alist-rows cfg
        (string-append
          "SELECT i.id::text AS id, i.name FROM grocery_items i "
          "WHERE NOT EXISTS (SELECT 1 FROM grocery_shop_items si "
          "  WHERE si.shop_id = " (sql-quote-int shop-id)
          "  AND si.item_id = i.id) "
          "ORDER BY lower(i.name)")))

    (define (add-item-to-shop! cfg shop-id item-id)
      ;; Append at end: position = max(pos)+1.
      (exec cfg
        (string-append
          "INSERT INTO grocery_shop_items (shop_id, item_id, position) "
          "VALUES (" (sql-quote-int shop-id) ", "
          (sql-quote-int item-id) ", "
          "(SELECT COALESCE(MAX(position), 0) + 1 "
          " FROM grocery_shop_items WHERE shop_id = "
          (sql-quote-int shop-id) ")) ON CONFLICT DO NOTHING")))

    (define (remove-item-from-shop! cfg shop-id item-id)
      (exec cfg (string-append
                  "DELETE FROM grocery_shop_items WHERE shop_id = "
                  (sql-quote-int shop-id)
                  " AND item_id = " (sql-quote-int item-id))))

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
                   (string-append
                     "UPDATE grocery_shop_items SET position = "
                     (number->string pos)
                     " WHERE shop_id = " (sql-quote-int shop-id)
                     " AND item_id = " (sql-quote-int (car ids))))
                 (loop (cdr ids) (+ pos 1)))))
            (pg-exec c "COMMIT")))))

    (define (swap-shop-positions! cfg shop-id item-id direction)
      ;; direction: 'up or 'down. Swaps position with the neighbour.
      (let* ((op  (case direction ((up) "<") (else ">")))
             (ord (case direction ((up) "DESC") (else "ASC")))
             (rs  (rows cfg
                    (string-append
                      "WITH me AS (SELECT position FROM grocery_shop_items "
                      "            WHERE shop_id = " (sql-quote-int shop-id)
                      "            AND item_id = " (sql-quote-int item-id) "), "
                      "  nbr AS (SELECT item_id, position FROM grocery_shop_items "
                      "          WHERE shop_id = " (sql-quote-int shop-id)
                      "          AND position " op " (SELECT position FROM me) "
                      "          ORDER BY position " ord " LIMIT 1) "
                      "SELECT (SELECT position FROM me)::text, "
                      "       (SELECT item_id::text FROM nbr), "
                      "       (SELECT position FROM nbr)::text"))))
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
                   "UPDATE grocery_shop_items SET position = " nbr-pos
                   " WHERE shop_id = " (sql-quote-int shop-id)
                   " AND item_id = " (sql-quote-int item-id) "; "
                   "UPDATE grocery_shop_items SET position = " my-pos
                   " WHERE shop_id = " (sql-quote-int shop-id)
                   " AND item_id = " nbr-id))))))))

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
                    "WHERE l.id = " (sql-quote-int id)))))
        (cond ((pair? rs) (car rs)) (else #f))))

    (define (create-list! cfg shop-id)
      ;; shop-id may be #f (default shop).
      (with-db cfg
        (lambda (c)
          (let ((res (pg-query c
                       (string-append
                         "INSERT INTO grocery_lists (shop_id) VALUES ("
                         (cond (shop-id (sql-quote-int shop-id))
                               (else "NULL"))
                         ") RETURNING id"))))
            (string->number (vector-ref (car (pg-result-rows res)) 0))))))

    (define (delete-list! cfg id)
      (exec cfg (string-append
                  "DELETE FROM grocery_lists WHERE id = "
                  (sql-quote-int id))))

    ;; Entries on a list, ordered per the list's shop. For the default
    ;; shop, alphabetical.
    (define (list-entries cfg list-id shop-id)
      (let ((order (cond
                     (shop-id
                      (string-append
                        "ORDER BY (SELECT si.position FROM grocery_shop_items si "
                        "          WHERE si.shop_id = " (sql-quote-int shop-id)
                        "          AND si.item_id = e.item_id), "
                        "         lower(i.name)"))
                     (else "ORDER BY lower(i.name)"))))
        (alist-rows cfg
          (string-append
            "SELECT e.id::text AS id, i.id::text AS item_id, i.name, "
            "       e.qty::text AS qty, "
            "       CASE WHEN e.bought THEN 'yes' ELSE 'no' END AS bought "
            "FROM grocery_list_entries e "
            "JOIN grocery_items i ON i.id = e.item_id "
            "WHERE e.list_id = " (sql-quote-int list-id) " "
            order))))

    (define (add-or-inc! cfg list-id item-id)
      (exec cfg
        (string-append
          "INSERT INTO grocery_list_entries (list_id, item_id, qty) VALUES ("
          (sql-quote-int list-id) ", "
          (sql-quote-int item-id) ", 1) "
          "ON CONFLICT (list_id, item_id) "
          "DO UPDATE SET qty = grocery_list_entries.qty + 1, bought = false")))

    (define (dec-entry! cfg entry-id)
      ;; -1; if it reaches 0, delete the row.
      (exec cfg
        (string-append
          "WITH upd AS (UPDATE grocery_list_entries SET qty = qty - 1 "
          "             WHERE id = " (sql-quote-int entry-id) " "
          "             RETURNING id, qty) "
          "DELETE FROM grocery_list_entries WHERE id IN "
          "(SELECT id FROM upd WHERE qty <= 0)")))

    (define (toggle-bought! cfg entry-id)
      (exec cfg (string-append
                  "UPDATE grocery_list_entries SET bought = NOT bought "
                  "WHERE id = " (sql-quote-int entry-id))))

    (define (delete-entry! cfg entry-id)
      (exec cfg (string-append
                  "DELETE FROM grocery_list_entries WHERE id = "
                  (sql-quote-int entry-id))))

    (define (clear-bought! cfg list-id)
      (exec cfg (string-append
                  "DELETE FROM grocery_list_entries WHERE list_id = "
                  (sql-quote-int list-id) " AND bought = true")))

    ;; ============================================================
    ;; Views
    ;; ============================================================

    (define (page out req auth title active body-html)
      (html-response
        (render-page req auth
                     (list (cons 'title title)
                           (cons 'active active)
                           (cons 'body-class "feeds-page"))
                     body-html)))

    ;; ---- landing ----

    (define (render-landing req auth cfg)
      (let ((lists (list-lists cfg))
            (shops (list-shops cfg))
            (out   (open-output-string)))
        (out! out "<header class=\"feeds-head\"><h1>Grocery</h1>"
                  "<a class=\"admin-link\" href=\"/grocery/items\">items</a> "
                  "<a class=\"admin-link\" href=\"/grocery/shops\">shops</a>"
                  "</header>")
        ;; new list
        (out! out "<form method=\"post\" action=\"/grocery/lists\" "
                  "class=\"grocery-newlist\">"
                  "<label>Shop "
                  "<select name=\"shop\">"
                  "<option value=\"\">default (alphabetical)</option>")
        (for-each
          (lambda (s)
            (out! out "<option value=\"" (html-attr-escape (row-field s "id"))
                      "\">" (html-escape (row-field s "name")) "</option>"))
          shops)
        (out! out "</select></label>"
                  "<button type=\"submit\">New list</button></form>")

        (cond
          ((null? lists)
           (out! out "<p class=\"empty\">No shopping lists yet.</p>"))
          (else
           (out! out "<ul class=\"grocery-lists\">")
           (for-each
             (lambda (l)
               (let ((id     (row-field l "id"))
                     (shop   (row-field l "shop_name"))
                     (n-all  (row-field l "n_entries"))
                     (n-open (row-field l "n_open"))
                     (when-s (row-field l "created")))
                 (out! out "<li><a class=\"row\" href=\"/grocery/lists/"
                           (html-attr-escape id) "\">"
                           "<span class=\"name\">" (html-escape shop) "</span>"
                           "<span class=\"meta\">"
                           (html-escape n-open) " open · "
                           (html-escape n-all) " total · "
                           (html-escape when-s)
                           "</span></a>"
                           " <form method=\"post\" action=\"/grocery/lists/"
                           (html-attr-escape id)
                           "/delete\" class=\"inline rm\" "
                           "data-confirm=\"Delete this list?\">"
                           "<button class=\"linkish danger\">delete</button>"
                           "</form></li>")))
             lists)
           (out! out "</ul>")))
        (page out req auth "Grocery" 'grocery (get-output-string out))))

    ;; ---- one list ----

    (define (render-list req auth cfg list-id)
      (let* ((l (find-list cfg list-id)))
        (cond
          ((not l) (render-error 404 "List not found."))
          (else
           (let* ((shop-id-str (row-field l "shop_id"))
                  (shop-id (string->number shop-id-str))
                  (entries (list-entries cfg list-id shop-id))
                  (catalog (shop-items cfg shop-id))
                  (entry-by-item-id
                    (let ((h '()))
                      (for-each
                        (lambda (e)
                          (set! h (cons (cons (row-field e "item_id") e) h)))
                        entries)
                      h))
                  (out (open-output-string)))
             (out! out "<header class=\"feeds-head\">"
                       "<h1>" (html-escape (row-field l "shop_name")) "</h1>"
                       " <a class=\"admin-link\" href=\"/grocery\">← lists</a>"
                       " <form method=\"post\" action=\"/grocery/lists/"
                       (html-attr-escape (number->string list-id))
                       "/clear-bought\" class=\"inline\">"
                       "<button class=\"linkish\">remove bought</button>"
                       "</form>"
                       " <form method=\"post\" action=\"/grocery/lists/"
                       (html-attr-escape (number->string list-id))
                       "/delete\" class=\"inline\" "
                       "data-confirm=\"Delete this list?\">"
                       "<button class=\"linkish danger\">delete list</button>"
                       "</form></header>")
             (render-entries out list-id entries)
             (render-catalog out list-id catalog entry-by-item-id)
             (page out req auth "Grocery" 'grocery (get-output-string out)))))))

    (define (render-entries out list-id entries)
      (out! out "<section class=\"grocery-list\">"
                "<h2>Shopping list</h2>")
      (cond
        ((null? entries)
         (out! out "<p class=\"empty\">Empty. Tap items below to add.</p>"))
        (else
         (out! out "<ul class=\"grocery-shopping\">")
         (for-each
           (lambda (e)
             (let* ((eid    (row-field e "id"))
                    (name   (row-field e "name"))
                    (qty    (or (string->number (row-field e "qty")) 1))
                    (bought (string=? (row-field e "bought") "yes")))
               (out! out "<li class=\""
                         (cond (bought "bought") (else "open")) "\">"
                         "<form method=\"post\" action=\"/grocery/lists/"
                         (html-attr-escape (number->string list-id))
                         "/entries/" (html-attr-escape eid)
                         "/toggle\" class=\"inline toggle\">"
                         "<button type=\"submit\" class=\"shop-toggle\">"
                         "<span class=\"check\" aria-hidden=\"true\">"
                         (cond (bought "&#x2714;") (else "&nbsp;")) "</span>"
                         "<span class=\"name\">" (html-escape name)
                         (cond ((> qty 1)
                                (string-append " <span class=\"qty\">×"
                                               (number->string qty)
                                               "</span>"))
                               (else ""))
                         "</span></button></form>"
                         "<form method=\"post\" action=\"/grocery/lists/"
                         (html-attr-escape (number->string list-id))
                         "/entries/" (html-attr-escape eid)
                         "/dec\" class=\"inline rm\">"
                         "<button class=\"linkish\" title=\"one less\">−</button>"
                         "</form>"
                         "<form method=\"post\" action=\"/grocery/lists/"
                         (html-attr-escape (number->string list-id))
                         "/entries/" (html-attr-escape eid)
                         "/delete\" class=\"inline rm\">"
                         "<button class=\"linkish\" title=\"remove\">×</button>"
                         "</form>"
                         "</li>")))
           entries)
         (out! out "</ul>")))
      (out! out "</section>"))

    (define (render-catalog out list-id catalog entry-by-item)
      (out! out "<section class=\"grocery-catalog\">"
                "<h2>Add items</h2>")
      (cond
        ((null? catalog)
         (out! out "<p class=\"empty\">This shop has no items yet. "
                   "Add some on the <a href=\"/grocery/shops\">shops</a> page.</p>"))
        (else
         (out! out "<ul class=\"grocery-items\">")
         (for-each
           (lambda (i)
             (let* ((id   (row-field i "id"))
                    (name (row-field i "name"))
                    (e    (assoc id entry-by-item))
                    (qty  (cond
                            (e (or (string->number
                                     (row-field (cdr e) "qty")) 0))
                            (else 0))))
               (out! out "<li>"
                         "<form method=\"post\" action=\"/grocery/lists/"
                         (html-attr-escape (number->string list-id))
                         "/add/" (html-attr-escape id)
                         "\" class=\"inline add\">"
                         "<button type=\"submit\" class=\"add-btn\">"
                         "+ <span class=\"name\">" (html-escape name) "</span>"
                         (cond ((> qty 0)
                                (string-append
                                  " <span class=\"qty\">·" (number->string qty)
                                  "</span>"))
                               (else ""))
                         "</button></form>"
                         "</li>")))
           catalog)
         (out! out "</ul>")))
      (out! out "</section>"))

    ;; ---- items admin ----

    (define (render-items req auth cfg)
      (let ((items (list-items cfg))
            (out   (open-output-string)))
        (out! out "<header class=\"feeds-head\"><h1>Items</h1>"
                  "<a class=\"admin-link\" href=\"/grocery\">← back</a>"
                  "</header>"
                  "<form method=\"post\" action=\"/grocery/items\" "
                  "class=\"grocery-new\">"
                  "<input type=\"text\" name=\"name\" required maxlength=\"100\" "
                  "placeholder=\"new item\" autofocus>"
                  "<button type=\"submit\">Add</button>"
                  "</form>")
        (cond
          ((null? items)
           (out! out "<p class=\"empty\">No items yet.</p>"))
          (else
           (out! out "<ul class=\"grocery-items\">")
           (for-each
             (lambda (i)
               (let ((id   (row-field i "id"))
                     (name (row-field i "name")))
                 (out! out "<li><span class=\"add-btn static\">"
                           (html-escape name) "</span>"
                           "<form method=\"post\" action=\"/grocery/items/"
                           (html-attr-escape id) "/delete\" class=\"inline rm\" "
                           "data-confirm=\"Delete this item from the catalog?\">"
                           "<button class=\"linkish\" title=\"delete\">×</button>"
                           "</form></li>")))
             items)
           (out! out "</ul>")))
        (page out req auth "Items" 'grocery (get-output-string out))))

    ;; ---- shops admin ----

    (define (render-shops req auth cfg)
      (let ((shops (list-shops cfg))
            (out   (open-output-string)))
        (out! out "<header class=\"feeds-head\"><h1>Shops</h1>"
                  "<a class=\"admin-link\" href=\"/grocery\">← back</a>"
                  "</header>"
                  "<form method=\"post\" action=\"/grocery/shops\" "
                  "class=\"grocery-new\">"
                  "<input type=\"text\" name=\"name\" required maxlength=\"60\" "
                  "placeholder=\"new shop\">"
                  "<button type=\"submit\">Add</button>"
                  "</form>"
                  "<ul class=\"grocery-items\">")
        (out! out "<li><span class=\"add-btn static\">(default)</span>"
                  "<span class=\"hint\">all items, alphabetical</span></li>")
        (for-each
          (lambda (s)
            (let ((id   (row-field s "id"))
                  (name (row-field s "name")))
              (out! out "<li><a class=\"add-btn\" href=\"/grocery/shops/"
                        (html-attr-escape id) "\">"
                        (html-escape name) " — edit order</a>"
                        "<form method=\"post\" action=\"/grocery/shops/"
                        (html-attr-escape id) "/delete\" class=\"inline rm\" "
                        "data-confirm=\"Delete this shop and its lists?\">"
                        "<button class=\"linkish\" title=\"delete\">×</button>"
                        "</form></li>")))
          shops)
        (out! out "</ul>")
        (page out req auth "Shops" 'grocery (get-output-string out))))

    ;; ---- one shop's order ----

    (define (render-shop req auth cfg shop-id)
      (let ((s (find-shop cfg shop-id)))
        (cond
          ((not s) (render-error 404 "Shop not found."))
          (else
           (let* ((in-shop (shop-items cfg shop-id))
                  (others  (items-not-in-shop cfg shop-id))
                  (out     (open-output-string)))
             (out! out "<header class=\"feeds-head\">"
                       "<h1>" (html-escape (row-field s "name")) "</h1>"
                       " <a class=\"admin-link\" href=\"/grocery/shops\">← shops</a>"
                       "</header>"
                       "<form method=\"post\" action=\"/grocery/shops/"
                       (html-attr-escape (number->string shop-id))
                       "/rename\" class=\"grocery-new\">"
                       "<input type=\"text\" name=\"name\" required "
                       "maxlength=\"60\" value=\""
                       (html-attr-escape (row-field s "name"))
                       "\"><button type=\"submit\">Rename</button></form>")
             ;; ordered items
             (out! out "<section class=\"grocery-list\"><h2>Items in shop order</h2>")
             (cond
               ((null? in-shop)
                (out! out "<p class=\"empty\">No items yet. Add from below.</p>"))
               (else
                (out! out "<ul class=\"grocery-order\" data-shop-id=\""
                          (html-attr-escape (number->string shop-id)) "\">")
                (for-each
                  (lambda (i)
                    (let ((iid  (row-field i "id"))
                          (name (row-field i "name")))
                      (out! out "<li data-item-id=\""
                                (html-attr-escape iid) "\">"
                                "<button type=\"button\" class=\"drag-handle\" "
                                "aria-label=\"drag to reorder\">⠿</button>"
                                "<span class=\"name\">"
                                (html-escape name) "</span>"
                                "<form method=\"post\" action=\"/grocery/shops/"
                                (html-attr-escape (number->string shop-id))
                                "/items/" (html-attr-escape iid)
                                "/up\" class=\"inline no-drag\">"
                                "<button class=\"linkish\" title=\"up\">↑</button>"
                                "</form>"
                                "<form method=\"post\" action=\"/grocery/shops/"
                                (html-attr-escape (number->string shop-id))
                                "/items/" (html-attr-escape iid)
                                "/down\" class=\"inline no-drag\">"
                                "<button class=\"linkish\" title=\"down\">↓</button>"
                                "</form>"
                                "<form method=\"post\" action=\"/grocery/shops/"
                                (html-attr-escape (number->string shop-id))
                                "/items/" (html-attr-escape iid)
                                "/remove\" class=\"inline rm\">"
                                "<button class=\"linkish\" title=\"remove\">×</button>"
                                "</form>"
                                "</li>")))
                  in-shop)
                (out! out "</ul>")))
             (out! out "</section>")
             ;; available items to add
             (out! out "<section class=\"grocery-catalog\"><h2>Available items</h2>")
             (cond
               ((null? others)
                (out! out "<p class=\"empty\">All catalog items are in this shop.</p>"))
               (else
                (out! out "<ul class=\"grocery-items\">")
                (for-each
                  (lambda (i)
                    (let ((iid  (row-field i "id"))
                          (name (row-field i "name")))
                      (out! out "<li>"
                                "<form method=\"post\" action=\"/grocery/shops/"
                                (html-attr-escape (number->string shop-id))
                                "/items/" (html-attr-escape iid)
                                "/add\" class=\"inline add\">"
                                "<button type=\"submit\" class=\"add-btn\">"
                                "+ <span class=\"name\">"
                                (html-escape name) "</span></button></form>"
                                "</li>")))
                  others)
                (out! out "</ul>")))
             (out! out "</section>")
             (page out req auth "Shop order" 'grocery
                   (get-output-string out)))))))

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
              (when (not (string=? name "")) (create-item! cfg name))
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
              (when (not (string=? name "")) (create-shop! cfg name))
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
              (cond (id (render-shop req auth cfg id))
                    (else (render-error 404 "Shop not found.")))))))

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
                   (sid  (cond ((string=? raw "") #f)
                               (else (string->number raw))))
                   (id   (create-list! cfg sid)))
              (redirect (string-append "/grocery/lists/"
                                       (number->string id)))))))

      (router-add! router "GET" "/grocery/lists/:id"
        (require-auth auth
          (lambda (req params)
            (let ((id (string->number (params-ref params "id"))))
              (cond (id (render-list req auth cfg id))
                    (else (render-error 404 "List not found.")))))))

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
