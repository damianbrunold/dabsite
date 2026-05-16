-- Stage 3 refinements:
--
-- 1. categories table with explicit sort_order so the UI can display
--    News / Tech / Devel in the same order as the old static page,
--    and the user can reorder via admin.
--
-- 2. feeds.failure_count to back off failing feeds. The effective refresh
--    interval is multiplied by 2^min(failure_count, 6).
--
-- 3. feed_entries.title_key + global recent-titles index for cross-feed
--    de-duplication of republished entries.

CREATE TABLE categories (
    name       text PRIMARY KEY,
    sort_order integer NOT NULL DEFAULT 100
);

INSERT INTO categories (name, sort_order) VALUES
    ('News',  10),
    ('Tech',  20),
    ('Devel', 30)
ON CONFLICT (name) DO NOTHING;

-- Any pre-existing category referenced by a feed that isn't in the table
-- gets a default sort_order so it still appears.
INSERT INTO categories (name, sort_order)
    SELECT DISTINCT f.category, 100
    FROM feeds f
    WHERE f.category NOT IN (SELECT name FROM categories)
ON CONFLICT (name) DO NOTHING;

ALTER TABLE feeds
    ADD COLUMN IF NOT EXISTS failure_count integer NOT NULL DEFAULT 0;

ALTER TABLE feed_entries
    ADD COLUMN IF NOT EXISTS title_key text GENERATED ALWAYS AS
        (lower(regexp_replace(coalesce(title, ''), '\s+', ' ', 'g'))) STORED;

-- Partial index for cheap "is this title-key already in the last 30 days"
-- lookups; we don't index empty title-keys.
CREATE INDEX feed_entries_title_key_recent_idx
    ON feed_entries (title_key, fetched_at DESC)
    WHERE title_key <> '';
