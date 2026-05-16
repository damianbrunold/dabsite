-- Private + public file storage.
--
-- The content lives on disk under data/files/<sha256> (content-addressed,
-- so duplicate uploads dedupe automatically). The DB row carries the
-- metadata and is the source of truth for visibility and naming.

CREATE TABLE files (
    id          bigserial PRIMARY KEY,
    name        text NOT NULL,
    mime        text NOT NULL,
    size        bigint NOT NULL,
    sha256      text NOT NULL,
    visibility  text NOT NULL DEFAULT 'private'
                CHECK (visibility IN ('public', 'private')),
    note        text NOT NULL DEFAULT '',
    created_at  timestamptz NOT NULL DEFAULT now(),
    CONSTRAINT files_name_chk CHECK (length(trim(name)) > 0),
    CONSTRAINT files_size_chk CHECK (size >= 0)
);

-- Public files must have unique names so /f/<name> is unambiguous.
-- Private files may collide; their UI link uses /files/<id> anyway.
CREATE UNIQUE INDEX files_public_name_idx
    ON files (name) WHERE visibility = 'public';

CREATE INDEX files_visibility_idx ON files (visibility);
CREATE INDEX files_created_idx    ON files (created_at DESC);
