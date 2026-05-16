-- Grocery, redesign: shops with per-shop item ordering, multiple
-- shopping lists, per-entry quantity. The single grocery_list from
-- migration 0012 is replaced by grocery_lists + grocery_list_entries.

DROP TABLE IF EXISTS grocery_list;

CREATE TABLE grocery_shops (
    id          bigserial PRIMARY KEY,
    name        text NOT NULL UNIQUE,
    created_at  timestamptz NOT NULL DEFAULT now(),
    CONSTRAINT grocery_shops_name_chk CHECK (length(trim(name)) > 0)
);

-- Per-shop curated list of items in a specific order. An item must be
-- a member of a shop before it can be added to a list for that shop.
CREATE TABLE grocery_shop_items (
    shop_id  bigint NOT NULL REFERENCES grocery_shops(id) ON DELETE CASCADE,
    item_id  bigint NOT NULL REFERENCES grocery_items(id) ON DELETE CASCADE,
    position integer NOT NULL,
    PRIMARY KEY (shop_id, item_id)
);
CREATE INDEX grocery_shop_items_order_idx
    ON grocery_shop_items (shop_id, position);

-- A shopping list. shop_id NULL means the virtual default shop
-- (all items, alphabetical).
CREATE TABLE grocery_lists (
    id          bigserial PRIMARY KEY,
    shop_id     bigint REFERENCES grocery_shops(id) ON DELETE CASCADE,
    created_at  timestamptz NOT NULL DEFAULT now()
);
CREATE INDEX grocery_lists_created_idx ON grocery_lists (created_at DESC);

-- Each line on a shopping list. qty > 1 means "I want N of this".
CREATE TABLE grocery_list_entries (
    id       bigserial PRIMARY KEY,
    list_id  bigint NOT NULL REFERENCES grocery_lists(id) ON DELETE CASCADE,
    item_id  bigint NOT NULL REFERENCES grocery_items(id) ON DELETE CASCADE,
    qty      integer NOT NULL DEFAULT 1 CHECK (qty >= 1),
    bought   boolean NOT NULL DEFAULT false,
    UNIQUE (list_id, item_id)
);
