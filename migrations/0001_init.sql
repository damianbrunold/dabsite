-- Initial schema. Stage 1 has no domain tables yet; this migration only
-- exists to verify the migration runner against a fresh database.
--
-- The schema_migrations table is created by the runner before any migration
-- file is applied, so we don't create it here.

-- Marker table so we can detect a successful first migration in tests.
CREATE TABLE IF NOT EXISTS app_info (
    key   text PRIMARY KEY,
    value text NOT NULL
);

INSERT INTO app_info (key, value) VALUES ('initialized', 'true')
ON CONFLICT (key) DO NOTHING;
