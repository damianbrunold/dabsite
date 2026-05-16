-- Task/done tracker port from dabplanner.
--
-- An "entry" is something I did: a free-text description + duration in
-- minutes + a completed-at timestamp. Each entry is tagged with one or
-- more topics. Topics live in their own small table; archiving instead
-- of deleting keeps historical entries readable.

CREATE TABLE tracker_topics (
    id         bigserial PRIMARY KEY,
    name       text UNIQUE NOT NULL,
    archived   boolean NOT NULL DEFAULT false,
    created_at timestamptz NOT NULL DEFAULT now(),
    CONSTRAINT tracker_topics_name_chk CHECK (length(trim(name)) > 0)
);

CREATE TABLE tracker_done (
    id          bigserial PRIMARY KEY,
    text        text NOT NULL,
    minutes     integer NOT NULL DEFAULT 0,
    completed   timestamptz NOT NULL DEFAULT now(),
    created_at  timestamptz NOT NULL DEFAULT now(),
    CONSTRAINT tracker_done_text_chk    CHECK (length(trim(text)) > 0),
    CONSTRAINT tracker_done_minutes_chk CHECK (minutes >= 0)
);

CREATE INDEX tracker_done_completed_idx ON tracker_done (completed DESC);
CREATE INDEX tracker_done_search_idx
    ON tracker_done USING gin (to_tsvector('simple', text));

CREATE TABLE tracker_done_topics (
    done_id  bigint NOT NULL REFERENCES tracker_done   (id) ON DELETE CASCADE,
    topic_id bigint NOT NULL REFERENCES tracker_topics (id) ON DELETE RESTRICT,
    PRIMARY KEY (done_id, topic_id)
);

CREATE INDEX tracker_done_topics_topic_idx
    ON tracker_done_topics (topic_id, done_id);
