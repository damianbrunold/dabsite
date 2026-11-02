;; Builds dist/dabsite_<version>_all.deb (see deploy/debian/README.Debian):
;;
;;   scm package-deb.scm [--out DIR] [--dabscm DIR | --jar SCM.JAR] [--allow-dirty]
;;
;; The package bundles the scm.jar of the dabscm checkout next to this repo
;; (../dabscm; build it first with `scm build-scm-java.scm`). The version
;; comes from the last commit; uncommitted changes need --allow-dirty.

(import (scheme base)
        (scheme process-context)
        (scm deb webapp))

(webapp-deb-main
  '((pkg . "dabsite")
    (maintainer . "Damian Brunold <damian.brunold@bluewin.ch>")
    (summary . "dabsite personal website (dabscm web application)")
    (description
      "Personal website: pages, feed reader, notepad, URL shortener, polls,"
      "tracker, file hosting, grocery list and calendar. Needs PostgreSQL.")
    (trees "src" "migrations" "static")
    (bin-exclude "dev-server.scm" "load-test.scm")
    (data-subdirs "files")
    (config-settings
      ("files-dir" . "\"/var/lib/dabsite/files\"")
      ("secure-cookies?" . "#t"))
    (setup-hints
      "create the PostgreSQL user and database (see /usr/share/doc/dabsite/README.Debian),"
      "set db-password, and auth-passphrase-hash from: dabsite-admin hash-passphrase 'passphrase'")
    (recommends "postgresql")
    (services "dabsite.service")
    (timers "dabsite-health.timer" "dabsite-restart.timer")
    (default-port . 8088))
  (cdr (command-line)))
