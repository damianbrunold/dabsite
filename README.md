# dabsite

The source code that runs my personal website. Released under MIT in case
anything in here is useful to someone else, but **this is not a general
purpose web application** — it's wired around how I use it, and it has
no multi-user notion (one passphrase, one owner). If you want to run it
yourself, you'll be reading the code, not configuration toggles.

It's a small scheme webapp on top of the `(scm net ...)` libraries from
[splg/scm](https://github.com/dabrunold/splg), with PostgreSQL for
state. The features it carries are the ones I use day to day:

* an editable home page plus additional pages under `/p/<slug>`
* a feed aggregator with per-entry read/unread state and a background
  fetcher
* a personal notepad (plain text or, with `!markdown` on the first
  line, markdown)
* an URL shortener (`/s/<code>` → 302)
* scheduling polls (`/poll/<slug>`, anonymous voting)
* a "what did I do today" tracker with topics and CSV export
* private + public file hosting (uploads, content-addressed on disk;
  public hosting at `/f/<name>`)
* a grocery list with multiple shops and per-shop item ordering
  (drag-to-reorder on touch + mouse)
* a personal calendar (month / week / agenda views, quick-add with
  natural-language parsing in English and German, minimal recurrence,
  ICS export)

Everything except a few public surfaces (`/`, `/p/<slug>`, `/login`,
`/f/<name>`, `/s/<code>`, `/poll/<slug>`, `/healthz`, `/static/*`,
`/robots.txt`, `/favicon.ico`) lives behind a single-passphrase login.


## Project layout

```
bin/         entry points (server.scm, gen-passphrase.scm, ...)
src/         scheme libraries (dabsite-app.sld, dabsite-db.sld, ...)
migrations/  numbered SQL files; the server applies pending ones at startup
static/      assets served at /static/  (CSS, JS, icon, robots.txt)
tests/       srfi-64 tests; pure-scheme, no DB needed
deploy/      apache + nginx vhost samples, systemd units
```

Library naming follows the scm convention: library `(dabsite app)` is
the file `src/dabsite-app.sld` — segments joined with `-`.


## Local bootstrap

1. **Install dependencies.** You need the `scm` binary on `PATH`
   ([splg/scm](https://github.com/dabrunold/splg), java or C# build)
   and a running PostgreSQL on localhost.

   ```
   sudo apt install postgresql
   sudo -u postgres createuser -P dabsite      # set a password
   sudo -u postgres createdb -O dabsite dabsite
   ```

2. **Create `config.scm`.** Copy the template and fill it in:

   ```
   cp config.example.scm config.scm
   scm bin/gen-secret.scm                          # -> cookie-secret
   scm bin/hash-passphrase.scm 'your passphrase'   # -> auth-passphrase-hash
   ```

   Don't have a passphrase ready? `bin/gen-passphrase.scm` makes one
   and the hash in a single step:

   ```
   scm bin/gen-passphrase.scm           # 5 random words
   scm bin/gen-passphrase.scm words 6   # 6 random words
   scm bin/gen-passphrase.scm chars 24  # 24 random alphanumerics
   ```

   Edit `config.scm`, paste in both values, and set `db-password` to
   what you configured for the postgres role. The file holds the cookie
   secret, the passphrase hash, and the DB password — restrict it:

   ```
   chmod 600 config.scm
   ```

3. **Run.**

   ```
   scm bin/server.scm
   ```

   The migration runner applies anything pending in `migrations/`,
   then the server binds `127.0.0.1:8088` by default. Visit
   <http://127.0.0.1:8088/login>, sign in with your passphrase, and
   the rest of the nav opens up.


## Dev server with auto-reload

For editing, `bin/dev-server.scm` is a small supervisor that runs the
unmodified `bin/server.scm` as a child and restarts it whenever a
watched file changes (every `*.sld` under `src/`, plus `bin/server.scm`
and the config file). Modeled on Flask's reloader.

```
scm bin/dev-server.scm                 # uses ./config.scm
scm bin/dev-server.scm path/to/cfg.scm
```

Child stdout/stderr are inherited, so server logs appear in the same
terminal alongside `[dev-server]` restart notices. In-flight requests
are dropped on restart. Use `bin/server.scm` directly in production.


## Tests

Unit tests cover the util library, the markdown renderer, the auth
layer (passphrase + cookie token), the multipart parser, the tracker
and grocery helpers, and a DB-free HTTP smoke test of the static-asset
path. Postgres is **not** required.

```
tests/run_all.sh          # runs every tests/test_*.scm
scm tests/test_util.scm   # just one file
```

Exit status is 0 on success, 1 if any assertion failed.


## Deployment

Samples in `deploy/`:

* `deploy/apache.conf.example` and `deploy/nginx.conf.example` —
  reverse-proxy vhosts that forward everything to
  `http://127.0.0.1:8088/`. TLS is the proxy's job; the app itself
  binds loopback.
* `deploy/systemd/dabsite.service` — main unit. Runs as a
  non-privileged user, restarts on failure, gives the scheme HTTP
  server its 10 s graceful drain before SIGKILL.
* `deploy/systemd/dabsite-health.{service,timer}` — minute-by-minute
  probe against `/healthz`. Two consecutive failures triggers a
  restart, so a single slow request can't bounce the daemon.
* `deploy/systemd/dabsite-restart.{service,timer}` — daily restart
  at 04:00 (randomized ±2m). Cheap insurance against slow leaks.

### Installing the units

```
sudo cp deploy/systemd/dabsite*.{service,timer} /etc/systemd/system/
sudo systemctl daemon-reload
sudo systemctl enable --now dabsite.service
sudo systemctl enable --now dabsite-health.timer
sudo systemctl enable --now dabsite-restart.timer
```

### Day-to-day

| command                                            | what it does                |
| -------------------------------------------------- | --------------------------- |
| `sudo systemctl status dabsite`                 | health summary              |
| `sudo systemctl start dabsite`                  | start the daemon            |
| `sudo systemctl stop dabsite`                   | stop the daemon             |
| `sudo systemctl restart dabsite`                | restart now                 |
| `journalctl -u dabsite -f`                      | tail the live logs          |
| `systemctl list-timers 'dabsite*'`              | see when the timers fire    |
| `sudo systemctl start dabsite-health.service`   | run a one-off health probe  |

`Restart=always` on the main unit plus the health timer means the
daemon recovers on its own from crashes or hangs. The daily restart is
belt-and-braces.

### Logs

The app writes structured lines to stderr; systemd captures them in
journald. Format:

```
2026-05-16 18:23:14 INFO  [http]    GET /tracker -> 200 (4ms)
2026-05-16 18:23:14 INFO  [feeds]   ok Heise
2026-05-16 18:23:15 ERROR [files]   upload photo.jpg failed: ...
```

Useful queries:

```
journalctl -u dabsite -f                        # live tail
journalctl -u dabsite --since '1 hour ago'      # recent history
journalctl -u dabsite -p err                    # only errors
journalctl -u dabsite -g 'GET /tracker'         # grep
```

Rotation is handled by journald itself — no `logrotate` config needed.
Typical `/etc/systemd/journald.conf` settings:

```
[Journal]
SystemMaxUse=200M        # cap the journal at 200 MB
SystemKeepFree=1G        # always leave 1 GB free on the partition
MaxRetentionSec=30day    # purge entries older than 30 days
```

After editing: `sudo systemctl restart systemd-journald`.


## License

MIT — see [LICENSE](LICENSE).
