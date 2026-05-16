-- Grocery: a persistent catalog of items, plus an ongoing shopping list
-- that references catalog rows. Single-user app, so one shopping list.

CREATE TABLE grocery_items (
    id          bigserial PRIMARY KEY,
    name        text NOT NULL UNIQUE,
    created_at  timestamptz NOT NULL DEFAULT now(),
    CONSTRAINT grocery_items_name_chk CHECK (length(trim(name)) > 0)
);

CREATE TABLE grocery_list (
    id          bigserial PRIMARY KEY,
    item_id     bigint NOT NULL REFERENCES grocery_items(id) ON DELETE CASCADE,
    bought      boolean NOT NULL DEFAULT false,
    added_at    timestamptz NOT NULL DEFAULT now()
);

CREATE INDEX grocery_list_added_idx ON grocery_list (added_at);
