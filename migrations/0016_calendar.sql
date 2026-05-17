-- Calendar: single-user personal events.
--
-- One row per event (or per recurring series). Recurrence is stored as
-- a small RFC-5545 RRULE subset string parsed in the app:
--   FREQ=DAILY|WEEKLY|MONTHLY|YEARLY
--   INTERVAL=<int>           (default 1)
--   BYDAY=MO,TU,...          (weekly only)
--   COUNT=<int>  |  UNTIL=<YYYYMMDDTHHMMSSZ>
-- exdates holds occurrence start times to skip (used when the user
-- deletes/edits a single occurrence of a series).

CREATE TABLE calendar_events (
    id           bigserial PRIMARY KEY,
    title        text NOT NULL,
    notes        text NOT NULL DEFAULT '',
    starts_at    timestamptz NOT NULL,
    ends_at      timestamptz,
    all_day      boolean NOT NULL DEFAULT false,
    location     text NOT NULL DEFAULT '',
    category     text NOT NULL DEFAULT '',
    rrule        text NOT NULL DEFAULT '',
    -- Skipped occurrences for the series: comma-separated YYYY-MM-DD
    -- strings (day-level only; matches our minimal recurrence model).
    exdates      text NOT NULL DEFAULT '',
    created_at   timestamptz NOT NULL DEFAULT now(),
    updated_at   timestamptz NOT NULL DEFAULT now(),
    CONSTRAINT calendar_events_title_chk CHECK (length(trim(title)) > 0),
    CONSTRAINT calendar_events_ends_chk
      CHECK (ends_at IS NULL OR ends_at >= starts_at)
);

CREATE INDEX calendar_events_starts_idx ON calendar_events (starts_at);
CREATE INDEX calendar_events_rrule_idx  ON calendar_events (starts_at)
    WHERE rrule <> '';

-- Optional, lightweight: categories the user has named, with a colour.
-- The events table just stores the category string; this table is a
-- side-table of "known" categories that the admin page edits. Joining
-- on (lower(category) = lower(name)) gives the colour at render time.
CREATE TABLE calendar_categories (
    id      bigserial PRIMARY KEY,
    name    text NOT NULL UNIQUE,
    colour  text NOT NULL DEFAULT '#888888',
    CONSTRAINT calendar_categories_name_chk CHECK (length(trim(name)) > 0)
);
