-- Topic names are matched case-insensitively and stored lowercase (this
-- instance's convention). Merge topics that differ only by case into one,
-- lowercase the rest, then enforce uniqueness with an index.
--
-- Survivor per lower(name): the all-lowercase spelling if there is one,
-- otherwise the oldest row. Its entries keep their links; it stays
-- active if any duplicate was active.

CREATE TEMP TABLE tracker_topic_merge AS
SELECT t.id AS old_id, k.keep_id
FROM tracker_topics t
JOIN (SELECT DISTINCT ON (lower(name))
             lower(name) AS lname, id AS keep_id
      FROM tracker_topics
      ORDER BY lower(name), (name = lower(name)) DESC, id) k
  ON k.lname = lower(t.name)
WHERE t.id <> k.keep_id;

UPDATE tracker_topics k SET archived = false
FROM tracker_topic_merge m
JOIN tracker_topics o ON o.id = m.old_id
WHERE k.id = m.keep_id AND NOT o.archived;

INSERT INTO tracker_done_topics (done_id, topic_id)
SELECT dt.done_id, m.keep_id
FROM tracker_done_topics dt
JOIN tracker_topic_merge m ON m.old_id = dt.topic_id
ON CONFLICT DO NOTHING;

DELETE FROM tracker_done_topics
WHERE topic_id IN (SELECT old_id FROM tracker_topic_merge);
DELETE FROM tracker_topics
WHERE id IN (SELECT old_id FROM tracker_topic_merge);

DROP TABLE tracker_topic_merge;

UPDATE tracker_topics SET name = lower(name) WHERE name <> lower(name);

CREATE UNIQUE INDEX tracker_topics_lower_name_key
    ON tracker_topics (lower(name));
