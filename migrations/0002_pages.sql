-- Landing page content. One row per slug; the rendered HTML is cached
-- alongside the source so reads are a single SELECT.
CREATE TABLE pages (
    slug       text PRIMARY KEY,
    title      text NOT NULL,
    format     text NOT NULL DEFAULT 'markdown',  -- 'markdown' | 'html'
    source     text NOT NULL,
    html_cache text NOT NULL,
    updated_at timestamptz NOT NULL DEFAULT now()
);

-- Seed home page. The HTML is left empty so the first request will render
-- and persist it; alternatively, run the editor to update.
INSERT INTO pages (slug, title, format, source, html_cache) VALUES
('home', 'Damian Brunold', 'markdown', $seed$
# Damian Brunold

I'm a developer based in Switzerland. Most of my code lives on
[GitHub](https://github.com/damianbrunold) and
[Codeberg](https://codeberg.org/damianbrunold).

## brunoldsoftware.ch

Long-running project: a small business-software shop selling targeted
desktop tools to a niche customer base. The catalog and ordering flow
both run there. See [brunoldsoftware.ch](https://www.brunoldsoftware.ch).

## textil-plattform.ch

A modern, web-based version of the same toolset: browser-based, multi-user,
designed to replace the legacy desktop installs over time. See
[textil-plattform.ch](https://www.textil-plattform.ch).

## This site

This website is itself a small scheme webapp. It will grow over time to
host a few personal tools.
$seed$, '')
ON CONFLICT (slug) DO NOTHING;
