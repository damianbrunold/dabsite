-- Feed aggregator: feeds and their entries. Entries are stored
-- indefinitely; read_at being non-null means the user has dismissed them
-- from the default view.

CREATE TABLE feeds (
    id              bigserial PRIMARY KEY,
    url             text NOT NULL UNIQUE,
    title           text NOT NULL,
    label           text NOT NULL DEFAULT '',
    category        text NOT NULL DEFAULT 'misc',
    refresh_seconds integer NOT NULL DEFAULT 3600,
    enabled         boolean NOT NULL DEFAULT true,
    last_fetched_at timestamptz,
    last_error      text,
    created_at      timestamptz NOT NULL DEFAULT now()
);

CREATE TABLE feed_entries (
    id           bigserial PRIMARY KEY,
    feed_id      bigint NOT NULL REFERENCES feeds(id) ON DELETE CASCADE,
    guid         text NOT NULL,
    title        text NOT NULL DEFAULT '',
    link         text NOT NULL DEFAULT '',
    summary      text NOT NULL DEFAULT '',
    published_at timestamptz,
    fetched_at   timestamptz NOT NULL DEFAULT now(),
    read_at      timestamptz,
    UNIQUE (feed_id, guid)
);

CREATE INDEX feed_entries_read_pub_idx
    ON feed_entries (read_at, COALESCE(published_at, fetched_at) DESC);

CREATE INDEX feed_entries_feed_idx
    ON feed_entries (feed_id);

CREATE INDEX feed_entries_search_idx
    ON feed_entries
    USING gin (to_tsvector('simple', title || ' ' || summary));

-- Seed the public feeds from the previous make-homepage.py setup.
-- Private feeds (substack tokens) deliberately excluded — add them
-- through /feeds/admin after first boot.
INSERT INTO feeds (url, title, label, category, refresh_seconds) VALUES
    ('https://www.republik.ch/feed.xml',                                                  'Republik',     'R',   'News',  3600),
    ('https://novayagazeta.eu/feed/rss',                                                  'Novaya Gazeta','NG',  'News',  3600),
    ('https://www.srf.ch/news/bnf/rss/19032223',                                          'SRF',          'SRF', 'News',  1800),
    ('https://www.theguardian.com/international/rss',                                     'Guardian Intl','G',   'News',  1800),
    ('https://www.theguardian.com/europe/rss',                                            'Guardian EU',  'G',   'News',  1800),
    ('https://www.theguardian.com/uk/commentisfree/rss',                                  'Guardian Cmt', 'G',   'News',  3600),
    ('https://www.liberation.fr/arc/outboundfeeds/rss-all/category/economie/?outputType=xml',     'Libération éco',  'LIB', 'News', 3600),
    ('https://www.liberation.fr/arc/outboundfeeds/rss-all/category/politique/?outputType=xml',    'Libération pol',  'LIB', 'News', 3600),
    ('https://www.liberation.fr/arc/outboundfeeds/rss-all/category/international/?outputType=xml','Libération intl', 'LIB', 'News', 3600),
    ('https://www.messageboxnews.com/feed',                                               'Message Box',  'MB',  'News',  3600),
    ('http://www.heise.de/newsticker/heise-atom.xml',                                     'Heise',        'H',   'Tech',  3600),
    ('http://rss.golem.de/rss.php?feed=RSS1.0',                                           'Golem',        'G',   'Tech',  3600),
    ('https://www.theregister.com/headlines.atom',                                        'The Register', 'TR',  'Tech',  3600),
    ('https://devclass.com/feed/',                                                        'DevClass',     'DC',  'Tech',  3600),
    ('https://www.nextplatform.com/feed/',                                                'NextPlatform', 'NP',  'Tech',  3600),
    ('https://planet.scheme.org/atom.xml',                                                'Planet Scheme','SCM', 'Devel', 7200),
    ('https://planet.lisp.org/rss20.xml',                                                 'Planet Lisp',  'LSP', 'Devel', 7200),
    ('https://planetpython.org/rss20.xml',                                                'Planet Python','PY',  'Devel', 7200)
ON CONFLICT (url) DO NOTHING;
