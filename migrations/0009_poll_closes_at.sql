-- Optional automatic close: when closes_at is set and in the past, the
-- poll behaves as closed even if the manual `closed` flag is false.
-- Setting `closed = true` is independent and overrides regardless.
ALTER TABLE polls
    ADD COLUMN IF NOT EXISTS closes_at timestamptz;
