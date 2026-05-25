-- Grocery: per-list manual ordering and ad-hoc entries.
--   * grocery_list_entries.position — explicit slot in the list.
--   * grocery_list_entries.name     — populated for ad-hoc entries
--                                     that aren't backed by a catalog row.
--   * item_id becomes nullable. An entry is either a catalog reference
--     (item_id set, name NULL) or ad-hoc (item_id NULL, name set).

ALTER TABLE grocery_list_entries
    ADD COLUMN position integer,
    ADD COLUMN name     text,
    ALTER COLUMN item_id DROP NOT NULL;

-- Backfill position from the current implicit ordering: the list's shop
-- order if any, falling back to alphabetical.
WITH ranked AS (
    SELECT e.id,
           row_number() OVER (
             PARTITION BY e.list_id
             ORDER BY COALESCE(
                       (SELECT si.position FROM grocery_shop_items si
                        WHERE si.shop_id = l.shop_id
                          AND si.item_id = e.item_id),
                       2147483000),
                      lower(i.name)
           ) AS rn
    FROM grocery_list_entries e
    JOIN grocery_lists l ON l.id = e.list_id
    JOIN grocery_items i ON i.id = e.item_id
)
UPDATE grocery_list_entries e
SET position = r.rn
FROM ranked r
WHERE r.id = e.id;

ALTER TABLE grocery_list_entries ALTER COLUMN position SET NOT NULL;

-- Old uniqueness (list_id, item_id) was a full UNIQUE constraint that
-- would reject multiple NULL item_id rows on some configurations and,
-- more importantly, treats ad-hoc entries (which never collide) the
-- same as catalog references. Replace with a partial unique index.
ALTER TABLE grocery_list_entries
    DROP CONSTRAINT grocery_list_entries_list_id_item_id_key;

CREATE UNIQUE INDEX grocery_list_entries_list_item_uniq
    ON grocery_list_entries (list_id, item_id)
    WHERE item_id IS NOT NULL;

ALTER TABLE grocery_list_entries
    ADD CONSTRAINT grocery_list_entries_shape_chk
    CHECK (
        (item_id IS NOT NULL AND name IS NULL)
        OR
        (item_id IS NULL AND name IS NOT NULL
         AND length(trim(name)) > 0)
    );

CREATE INDEX grocery_list_entries_order_idx
    ON grocery_list_entries (list_id, position);
