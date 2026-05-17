-- Soft-delete for feed entries.
--
-- Previously prune-read-entries! issued a hard DELETE on read entries
-- beyond a per-label cap. That destroyed the dedup state: on the next
-- refresh the same items (still served upstream) re-inserted as new,
-- because both the (feed_id, guid) unique row and the title_key row
-- they would have collided with were gone.
--
-- Now the pruner sets deleted_at instead. User-facing queries hide
-- soft-deleted rows; dedup queries (ON CONFLICT and the title_key
-- recency scan) still see them. A second-stage hard prune removes
-- rows whose deleted_at is past the dedup window.

ALTER TABLE feed_entries
    ADD COLUMN IF NOT EXISTS deleted_at timestamptz;

-- The unread/recent index is the hot path for the rendered page; make
-- it partial so soft-deleted rows don't slow it down.
DROP INDEX IF EXISTS feed_entries_read_pub_idx;
CREATE INDEX feed_entries_read_pub_idx
    ON feed_entries (read_at, COALESCE(published_at, fetched_at) DESC)
    WHERE deleted_at IS NULL;
