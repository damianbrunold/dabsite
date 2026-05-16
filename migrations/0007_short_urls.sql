-- URL shortener.
--
-- Public reads (the redirect at /s/<code>) only touch this table, so
-- giving it its own narrow indexes keeps lookups cheap. The hit counter
-- is best-effort: a missed UPDATE under contention costs at most one tick
-- of accuracy, which doesn't matter here.

CREATE TABLE short_urls (
    code       text PRIMARY KEY,
    target     text NOT NULL,
    note       text NOT NULL DEFAULT '',
    hits       bigint NOT NULL DEFAULT 0,
    created_at timestamptz NOT NULL DEFAULT now()
);

-- Allowed characters in a code: alphanumerics, '-', '_'. Length 1..32.
ALTER TABLE short_urls
    ADD CONSTRAINT short_urls_code_chk
    CHECK (code ~ '^[A-Za-z0-9_-]{1,32}$');
