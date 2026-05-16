-- Doodle-style scheduling polls.
--
-- A poll has a fixed list of options (e.g. proposed dates). Every
-- participant marks each option yes / no / maybe. Anyone with the
-- public URL can vote; later edits are bound to the browser via the
-- poll_owner cookie.

CREATE TABLE polls (
    id          bigserial PRIMARY KEY,
    slug        text UNIQUE NOT NULL,
    title       text NOT NULL,
    description text NOT NULL DEFAULT '',
    closed      boolean NOT NULL DEFAULT false,
    created_at  timestamptz NOT NULL DEFAULT now(),
    CONSTRAINT polls_slug_chk CHECK (slug ~ '^[a-z0-9_-]{1,64}$')
);

CREATE TABLE poll_options (
    id         bigserial PRIMARY KEY,
    poll_id    bigint NOT NULL REFERENCES polls(id) ON DELETE CASCADE,
    sort_order integer NOT NULL DEFAULT 0,
    label      text NOT NULL
);

CREATE INDEX poll_options_poll_idx ON poll_options (poll_id, sort_order);

CREATE TABLE poll_responses (
    id           bigserial PRIMARY KEY,
    poll_id      bigint NOT NULL REFERENCES polls(id) ON DELETE CASCADE,
    name         text NOT NULL,
    owner_cookie text NOT NULL,
    created_at   timestamptz NOT NULL DEFAULT now(),
    updated_at   timestamptz NOT NULL DEFAULT now(),
    UNIQUE (poll_id, owner_cookie)
);

CREATE INDEX poll_responses_poll_idx ON poll_responses (poll_id, created_at);

CREATE TABLE poll_choices (
    response_id bigint NOT NULL REFERENCES poll_responses(id) ON DELETE CASCADE,
    option_id   bigint NOT NULL REFERENCES poll_options(id)   ON DELETE CASCADE,
    value       text NOT NULL,
    PRIMARY KEY (response_id, option_id),
    CONSTRAINT poll_choices_value_chk CHECK (value IN ('yes','no','maybe'))
);

-- updated_at trigger so the timestamp tracks edits.
CREATE OR REPLACE FUNCTION poll_responses_touch_updated()
RETURNS trigger AS $$
BEGIN
    NEW.updated_at = now();
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER poll_responses_touch_updated_trg
BEFORE UPDATE ON poll_responses
FOR EACH ROW EXECUTE FUNCTION poll_responses_touch_updated();
