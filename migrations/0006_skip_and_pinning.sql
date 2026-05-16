-- Two small additions:
--
-- 1) feed_skip_patterns: a list of patterns matched against entry titles
--    at upsert time so noise like "Anzeige:" or "Live chat with" is
--    dropped before it ever hits the user-facing list.
--
-- 2) feeds.min_entries: each feed contributes at least this many of its
--    own entries to the rendered page regardless of category caps. Set
--    it to e.g. 6 for low-volume feeds (Republik, Novaya Gazeta) that
--    would otherwise get squeezed out by high-volume neighbours.

CREATE TABLE IF NOT EXISTS feed_skip_patterns (
    id         bigserial PRIMARY KEY,
    pattern    text NOT NULL,
    kind       text NOT NULL DEFAULT 'prefix',  -- 'prefix' | 'contains'
    created_at timestamptz NOT NULL DEFAULT now(),
    CONSTRAINT feed_skip_patterns_kind_chk
        CHECK (kind IN ('prefix', 'contains'))
);

CREATE UNIQUE INDEX IF NOT EXISTS feed_skip_patterns_unique
    ON feed_skip_patterns (kind, pattern);

-- Seed the old hard-coded list from make-homepage.py.
INSERT INTO feed_skip_patterns (pattern, kind) VALUES
    ('Anzeige',          'prefix'),
    ('ComPost Live',     'prefix'),
    ('Opinions Live',    'prefix'),
    ('Pop Culture Live', 'prefix'),
    ('Chat with',        'prefix'),
    ('Need a dose of humor', 'prefix'),
    ('Ask Eugene Robinson',  'prefix')
ON CONFLICT (kind, pattern) DO NOTHING;

ALTER TABLE feeds
    ADD COLUMN IF NOT EXISTS min_entries integer NOT NULL DEFAULT 0;
