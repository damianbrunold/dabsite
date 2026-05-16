-- Personal notepad. Each note has a unique slug-style name, a body, and
-- timestamps. Names auto-generated as three random dictionary words when
-- the user doesn't supply one.
CREATE TABLE notes (
    id         bigserial PRIMARY KEY,
    name       text NOT NULL UNIQUE,
    body       text NOT NULL DEFAULT '',
    created_at timestamptz NOT NULL DEFAULT now(),
    updated_at timestamptz NOT NULL DEFAULT now()
);

-- Full-text search over name + body. We use the 'simple' config so the
-- index doesn't apply English stemming (it shouldn't change behaviour
-- for short personal notes).
CREATE INDEX notes_search_idx
    ON notes
    USING gin (to_tsvector('simple', name || ' ' || body));

-- Touch updated_at on UPDATE.
CREATE OR REPLACE FUNCTION notes_touch_updated()
RETURNS trigger AS $$
BEGIN
    NEW.updated_at = now();
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER notes_touch_updated_trg
BEFORE UPDATE ON notes
FOR EACH ROW EXECUTE FUNCTION notes_touch_updated();
