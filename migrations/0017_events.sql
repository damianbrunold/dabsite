-- Personal events: training, joggen, wandern, spazieren, migräne, blutdruck, gewicht.
--
-- A single row per recorded occurrence. Indicator kinds (training,
-- joggen, wandern, spazieren, migraene) carry no numeric values.
-- Blutdruck stores systolic in v1, diastolic in v2, pulse in v3.
-- Gewicht stores kilograms in v1. The kinds list is fixed in app
-- code, not in a check constraint, so adding a new kind later is a
-- single-file change.

CREATE TABLE events (
    id          bigserial PRIMARY KEY,
    kind        text        NOT NULL,
    recorded_at timestamptz NOT NULL DEFAULT now(),
    v1          numeric,
    v2          numeric,
    v3          numeric,
    notes       text        NOT NULL DEFAULT '',
    created_at  timestamptz NOT NULL DEFAULT now(),
    CONSTRAINT events_kind_chk CHECK (length(trim(kind)) > 0)
);

CREATE INDEX events_recorded_idx ON events (recorded_at DESC);
CREATE INDEX events_kind_recorded_idx ON events (kind, recorded_at DESC);
