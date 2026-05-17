-- Pinned feed entries.
--
-- A pinned entry is one the user wants to keep around indefinitely:
-- it is marked read but never soft- or hard-pruned, and shows up in
-- the dedicated /feeds/pinned view with its own filters.
--
-- Two related capabilities live on the same row:
--
--   1. Link health: link_checked_at + link_status + link_failure_count.
--      A scheduled job HEAD-pings each pinned entry's link; two
--      consecutive failures flip link_status to 'stale'. Any success
--      resets to 'ok' and zeroes the failure counter.
--
--   2. Manually added pins: feed_id becomes nullable so the user can
--      add a one-off URL that isn't part of any subscribed feed.
--      manual_category / manual_label let those rows carry their own
--      category/label (since there is no feeds row to read them from).
--      For normal feed-sourced entries these columns stay NULL and
--      the feeds row supplies the values via COALESCE in queries.

ALTER TABLE feed_entries
    ADD COLUMN IF NOT EXISTS pinned_at           timestamptz,
    ADD COLUMN IF NOT EXISTS link_checked_at     timestamptz,
    ADD COLUMN IF NOT EXISTS link_status         text NOT NULL DEFAULT 'ok',
    ADD COLUMN IF NOT EXISTS link_failure_count  integer NOT NULL DEFAULT 0,
    ADD COLUMN IF NOT EXISTS manual_category     text,
    ADD COLUMN IF NOT EXISTS manual_label        text;

ALTER TABLE feed_entries
    DROP CONSTRAINT IF EXISTS feed_entries_feed_id_fkey;

ALTER TABLE feed_entries
    ALTER COLUMN feed_id DROP NOT NULL;

-- Manually added entries have feed_id = NULL, so don't cascade-delete
-- them when a (different) feed disappears. SET NULL is the natural
-- behavior; for feed-sourced entries it just means they survive a feed
-- deletion as orphans (visible only if pinned — the unread view JOINs
-- against feeds and so would hide them, which is fine).
ALTER TABLE feed_entries
    ADD CONSTRAINT feed_entries_feed_id_fkey
        FOREIGN KEY (feed_id) REFERENCES feeds(id) ON DELETE SET NULL;

ALTER TABLE feed_entries
    ADD CONSTRAINT feed_entries_link_status_chk
        CHECK (link_status IN ('ok', 'stale'));

-- Manually added rows still need a non-null guid (UNIQUE (feed_id, guid)
-- treats NULLs as distinct in Postgres, so collisions across manuals
-- are not enforced — that's intentional, the user might intentionally
-- add the same URL twice with different notes later).
CREATE INDEX IF NOT EXISTS feed_entries_pinned_idx
    ON feed_entries (pinned_at DESC)
    WHERE pinned_at IS NOT NULL;

-- Picks the oldest-checked pinned entries for the link health scanner.
CREATE INDEX IF NOT EXISTS feed_entries_pinned_check_idx
    ON feed_entries (link_checked_at NULLS FIRST)
    WHERE pinned_at IS NOT NULL AND link <> '';
