// (Theme bootstrap moved to /static/theme.js so it can run before
//  first paint; this file is `defer`red.)

// Confirm-before-submit: any <form> carrying data-confirm="<msg>" pops
// a confirmation dialog before submitting. Replaces inline
// onsubmit="return confirm(...)" handlers, which are blocked by our
// strict CSP (script-src 'self', no unsafe-inline).
(function () {
    document.addEventListener('submit', function (ev) {
        var form = ev.target;
        if (!form || form.tagName !== 'FORM') return;
        var msg = form.getAttribute('data-confirm');
        if (!msg) return;
        if (!window.confirm(msg)) ev.preventDefault();
    }, true);
})();

// Theme toggle: cycles the user's explicit preference between "light" and
// "dark", persisted in localStorage. Initial state, before any click, comes
// from the OS via the prefers-color-scheme media query (handled in CSS); we
// only set data-theme on <html> once the user has chosen explicitly.
(function () {
    var btn = document.getElementById('theme-toggle');
    if (!btn) return;

    function currentEffective() {
        var t = document.documentElement.getAttribute('data-theme');
        if (t === 'dark' || t === 'light') return t;
        return window.matchMedia('(prefers-color-scheme: dark)').matches
            ? 'dark' : 'light';
    }

    btn.addEventListener('click', function () {
        var next = currentEffective() === 'dark' ? 'light' : 'dark';
        document.documentElement.setAttribute('data-theme', next);
        try { localStorage.setItem('theme', next); } catch (e) { /* ignore */ }
    });
})();

// Feed entries: intercept the per-entry mark-read POST so the entry vanishes
// in place. Without JS this still works via a full-page reload.
(function () {
    function bind(form) {
        form.addEventListener('submit', function (ev) {
            // Only handle the "read" action — leaving "unread" as a normal
            // reload keeps the entry visible immediately.
            if (!form.action.endsWith('/read')) return;
            ev.preventDefault();
            fetch(form.action, { method: 'POST', credentials: 'same-origin' })
                .then(function () {
                    var li = form.closest('li.feed-entry');
                    if (li) {
                        li.style.transition = 'opacity 200ms';
                        li.style.opacity = '0';
                        setTimeout(function () { li.remove(); }, 220);
                    }
                })
                .catch(function () { form.submit(); });
        });
    }
    document.querySelectorAll('li.feed-entry form.mark').forEach(bind);

    // Clicking the entry link opens the article in a new tab; treat that as
    // having read it. Mark the entry read and remove it in place, same as
    // clicking the ✓ button. Without JS the link still works (the entry
    // simply stays unread until marked explicitly).
    // Long-press on touch devices opens a small sheet showing the
    // entry's date + summary (the same content as the desktop title
    // tooltip). A successful long-press suppresses the click so the
    // link doesn't navigate and the entry isn't marked read.
    var LONG_PRESS_MS = 500;
    var MOVE_TOLERANCE_PX = 10;

    function closeTipSheet() {
        var b = document.querySelector('.tip-sheet-backdrop');
        var s = document.querySelector('.tip-sheet');
        if (b) b.remove();
        if (s) s.remove();
    }
    function showTipSheet(text) {
        closeTipSheet();
        if (!text) return;
        var backdrop = document.createElement('div');
        backdrop.className = 'tip-sheet-backdrop';
        var sheet = document.createElement('div');
        sheet.className = 'tip-sheet';
        var close = document.createElement('button');
        close.type = 'button';
        close.className = 'tip-close';
        close.textContent = 'close';
        var body = document.createElement('div');
        body.textContent = text;
        sheet.appendChild(close);
        sheet.appendChild(body);
        document.body.appendChild(backdrop);
        document.body.appendChild(sheet);
        backdrop.addEventListener('click', closeTipSheet);
        close.addEventListener('click', closeTipSheet);
    }

    document.querySelectorAll('li.feed-entry a.entry-link').forEach(
        function (a) {
            var timer = null;
            var longPressed = false;
            var startX = 0, startY = 0;

            function cancel() {
                if (timer !== null) { clearTimeout(timer); timer = null; }
            }
            a.addEventListener('touchstart', function (ev) {
                longPressed = false;
                var t = ev.touches[0];
                startX = t.clientX; startY = t.clientY;
                cancel();
                timer = setTimeout(function () {
                    longPressed = true;
                    timer = null;
                    showTipSheet(a.getAttribute('title') || '');
                }, LONG_PRESS_MS);
            }, { passive: true });
            a.addEventListener('touchmove', function (ev) {
                var t = ev.touches[0];
                if (Math.abs(t.clientX - startX) > MOVE_TOLERANCE_PX ||
                    Math.abs(t.clientY - startY) > MOVE_TOLERANCE_PX) {
                    cancel();
                }
            }, { passive: true });
            a.addEventListener('touchend', cancel, { passive: true });
            a.addEventListener('touchcancel', cancel, { passive: true });

            a.addEventListener('click', function (ev) {
                if (longPressed) {
                    // Long-press fired: swallow the synthetic click so the
                    // link doesn't navigate and the mark-read below is
                    // skipped. Reset for the next interaction.
                    ev.preventDefault();
                    ev.stopImmediatePropagation();
                    longPressed = false;
                    return;
                }
                var li = a.closest('li.feed-entry');
                if (!li || li.classList.contains('is-read')) return;
                var id = li.getAttribute('data-id');
                if (!id) return;
                fetch('/feeds/entry/' + encodeURIComponent(id) + '/read',
                      { method: 'POST', credentials: 'same-origin',
                        keepalive: true });
                li.style.transition = 'opacity 200ms';
                li.style.opacity = '0';
                setTimeout(function () { li.remove(); }, 220);
            });
        });
})();

// Tracker quick-add: prefix parsing inside the "what" textarea.
//
// Leading tokens (separated by whitespace) are stripped and applied to the
// other fields as the user types:
//   +topic        -> add "topic" to the topics field
//   -topic        -> remove "topic" from the topics field
//   !90 / !1:30   -> set duration field
//   !45m / !1h30  -> set duration field
//   YYYYMMDD-HHMM -> set the "when" datetime-local field
(function () {
    var form = document.querySelector('form[data-tracker-add]');
    if (!form) return;
    var what = form.querySelector('textarea[name="text"]');
    var dur  = form.querySelector('input[name="minutes"]');
    var tops = form.querySelector('input[name="topics"]');
    var when = form.querySelector('input[name="completed"]');
    if (!what) return;

    function topicList() {
        return tops.value.split(',').map(function (s) {
            return s.trim();
        }).filter(function (s) { return s.length > 0; });
    }
    function setTopics(arr) {
        tops.value = arr.join(', ');
    }
    function addTopic(name) {
        var list = topicList();
        for (var i = 0; i < list.length; i++) {
            if (list[i].toLowerCase() === name.toLowerCase()) return;
        }
        list.push(name);
        setTopics(list);
    }
    function removeTopic(name) {
        setTopics(topicList().filter(function (t) {
            return t.toLowerCase() !== name.toLowerCase();
        }));
    }

    // Known topics, read from the datalist the form ships with.
    function knownTopics() {
        var opts = document.querySelectorAll(
            '#tracker-topics-list option');
        var out = [];
        opts.forEach(function (o) {
            var v = o.getAttribute('value');
            if (v) out.push(v);
        });
        return out;
    }
    function topicKnown(name) {
        var lower = name.toLowerCase();
        return knownTopics().some(function (t) {
            return t.toLowerCase() === lower;
        });
    }

    // Mirror of (parse-minutes) in damian-tracker.sld — used to gate the !
    // prefix so a bare "!" followed by an unrelated word doesn't get eaten.
    function looksLikeDuration(s) {
        return /^(\d+:[0-5]?\d|\d+(\.\d+)?h(\d{1,2})?|\d+m|\d+)$/.test(s);
    }

    // Tokens the user declined to turn into new topics. We remember them so
    // typing past the rejection doesn't keep firing confirm() on every input.
    var rejectedTopics = Object.create(null);

    function tryDate(text) {
        var m = text.match(/^(\d{4})(\d{2})(\d{2})-(\d{2})(\d{2})\s+/);
        if (!m) return 0;
        when.value = m[1] + '-' + m[2] + '-' + m[3]
            + 'T' + m[4] + ':' + m[5];
        return m[0].length;
    }
    function tryDuration(text) {
        var m = text.match(/^!(\S+)\s+/);
        if (!m) return 0;
        if (!looksLikeDuration(m[1])) return 0;
        dur.value = m[1];
        return m[0].length;
    }
    function tryTopic(text, sign, action) {
        var re = sign === '+'
            ? /^\+([^\s,]+)\s+/
            : /^-([^\s,]+)\s+/;
        var m = text.match(re);
        if (!m) return 0;
        var name = m[1];
        if (!topicKnown(name)) {
            var key = sign + name.toLowerCase();
            if (rejectedTopics[key]) return 0;
            var ok = window.confirm(
                "Add new topic '" + name + "'?");
            if (!ok) { rejectedTopics[key] = true; return 0; }
        }
        action(name);
        return m[0].length;
    }

    function parse() {
        var text = what.value;
        var changed = false;
        outer: while (true) {
            var n;
            n = tryDate(text);     if (n) { text = text.slice(n); changed = true; continue outer; }
            n = tryDuration(text); if (n) { text = text.slice(n); changed = true; continue outer; }
            n = tryTopic(text, '+', addTopic);
            if (n) { text = text.slice(n); changed = true; continue outer; }
            n = tryTopic(text, '-', removeTopic);
            if (n) { text = text.slice(n); changed = true; continue outer; }
            break;
        }
        if (changed) {
            what.value = text;
            what.setSelectionRange(0, 0);
        }
    }
    what.addEventListener('input', parse);

    // Ctrl/Cmd+Enter from any field inside the add form submits it.
    form.addEventListener('keydown', function (ev) {
        if (ev.key === 'Enter' && (ev.ctrlKey || ev.metaKey)) {
            ev.preventDefault();
            if (typeof form.requestSubmit === 'function') {
                form.requestSubmit();
            } else {
                form.submit();
            }
        }
    });
})();

// Grocery shop-order: drag-to-reorder using Pointer Events (works on
// mouse and touch). The drag handle is the leading ⠿ button; tapping
// elsewhere on the row is unaffected.
(function () {
    var list = document.querySelector('ul.grocery-order[data-shop-id]');
    if (!list) return;
    var shopId = list.getAttribute('data-shop-id');

    function currentOrder() {
        var ids = [];
        list.querySelectorAll('li').forEach(function (li) {
            var id = li.getAttribute('data-item-id');
            if (id) ids.push(id);
        });
        return ids;
    }

    function commit() {
        fetch('/grocery/shops/' + shopId + '/reorder', {
            method: 'POST',
            headers: { 'Content-Type': 'application/x-www-form-urlencoded' },
            credentials: 'same-origin',
            body: 'order=' + encodeURIComponent(currentOrder().join(','))
        });
    }

    function bind(handle) {
        handle.addEventListener('pointerdown', function (ev) {
            var dragged = handle.closest('li');
            if (!dragged) return;
            dragged.classList.add('dragging');
            ev.preventDefault();

            function onMove(e2) {
                var items = list.querySelectorAll('li:not(.dragging)');
                var placed = false;
                for (var i = 0; i < items.length; i++) {
                    var sib = items[i];
                    var r = sib.getBoundingClientRect();
                    if (e2.clientY < r.top + r.height / 2) {
                        list.insertBefore(dragged, sib);
                        placed = true;
                        break;
                    }
                }
                if (!placed) list.appendChild(dragged);
            }
            function onEnd() {
                dragged.classList.remove('dragging');
                document.removeEventListener('pointermove', onMove);
                document.removeEventListener('pointerup', onEnd);
                document.removeEventListener('pointercancel', onEnd);
                commit();
            }
            document.addEventListener('pointermove', onMove);
            document.addEventListener('pointerup', onEnd);
            document.addEventListener('pointercancel', onEnd);
        });
    }

    list.querySelectorAll('.drag-handle').forEach(bind);
})();
