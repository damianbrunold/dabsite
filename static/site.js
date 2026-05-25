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

// Grocery list page: preserve scroll position across the
// submit-redirect-reload cycle so adding an item doesn't bounce the
// viewport to the top. Scoped to the per-list shopping view.
(function () {
    if (!document.querySelector('ul.grocery-shopping[data-list-id]')) return;
    var key = 'grocery-scroll:' + location.pathname;
    try {
        var saved = sessionStorage.getItem(key);
        if (saved !== null) {
            sessionStorage.removeItem(key);
            window.scrollTo(0, parseInt(saved, 10) || 0);
        }
    } catch (e) { /* private mode, ignore */ }
    document.addEventListener('submit', function () {
        try { sessionStorage.setItem(key, String(window.scrollY)); }
        catch (e) { /* ignore */ }
    }, true);
})();

// Grocery drag-to-reorder using Pointer Events (mouse + touch). Used
// by both the shop-order admin page and the per-list shopping view.
// The drag handle is the leading ⠿ button; tapping elsewhere on the
// row is unaffected.
(function () {
    function setup(list, idAttr, endpoint) {
        function currentOrder() {
            var ids = [];
            list.querySelectorAll('li').forEach(function (li) {
                var id = li.getAttribute(idAttr);
                if (id) ids.push(id);
            });
            return ids;
        }
        function commit() {
            fetch(endpoint, {
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
    }

    var shopList = document.querySelector('ul.grocery-order[data-shop-id]');
    if (shopList) {
        setup(shopList, 'data-item-id',
              '/grocery/shops/' + shopList.getAttribute('data-shop-id') + '/reorder');
    }
    var listList = document.querySelector('ul.grocery-shopping[data-list-id]');
    if (listList) {
        setup(listList, 'data-entry-id',
              '/grocery/lists/' + listList.getAttribute('data-list-id') + '/reorder');
    }
})();

// ===========================================================
// Calendar
// ===========================================================

// Mirror of (dabsite calendar) parse-quick-add. Kept intentionally
// loose; the server is authoritative. This only powers the live
// preview chip so the user sees the parse before they submit.
(function () {
    var input = document.querySelector('[data-cal-input]');
    var preview = document.querySelector('[data-cal-preview]');
    if (!input || !preview) return;

    var WEEKDAYS = {
        mon:1, monday:1, mo:1, montag:1,
        tue:2, tuesday:2, tu:2, di:2, dienstag:2,
        wed:3, wednesday:3, we:3, mi:3, mittwoch:3,
        thu:4, thursday:4, th:4, do:4, donnerstag:4,
        fri:5, friday:5, fr:5, freitag:5,
        sat:6, saturday:6, sa:6, samstag:6, sonnabend:6,
        sun:7, sunday:7, su:7, so:7, sonntag:7
    };
    var WD_CODE = ['MO','TU','WE','TH','FR','SA','SU'];
    var DAILY   = ['daily','täglich','taglich'];
    var WEEKLY  = ['weekly','wöchentlich','wochentlich'];
    var MONTHLY = ['monthly','monatlich'];
    var YEARLY  = ['yearly','annually','jährlich','jahrlich'];
    var ALLDAY  = ['allday','all-day','ganztägig','ganztagig'];
    var EVERY   = ['every','jeden','jede'];
    var FOR     = ['for','für','fur'];
    var TODAY_W = ['today','heute'];
    var TOMORROW_W = ['tomorrow','morgen'];
    var DAYAFT  = ['übermorgen','uebermorgen'];
    var TONIGHT = ['tonight','heute-abend'];
    var YESTER  = ['yesterday','gestern'];
    var IN_W    = ['in'];
    var U_DAY   = ['day','days','tag','tage','tagen'];
    var U_WEEK  = ['week','weeks','woche','wochen'];
    var U_MONTH = ['month','months','monat','monate','monaten'];
    var U_YEAR  = ['year','years','jahr','jahre','jahren'];
    var NEXT_W  = ['next','nächsten','naechsten','nächste','naechste'];

    function inArr(s, arr) { return arr.indexOf(s.toLowerCase()) !== -1; }
    function pad2(n) { return n < 10 ? '0' + n : '' + n; }

    function addDays(d, n) {
        var x = new Date(d.getFullYear(), d.getMonth(), d.getDate() + n);
        return x;
    }
    function addMonths(d, n) {
        var y = d.getFullYear(), m = d.getMonth() + n, day = d.getDate();
        var ny = y + Math.floor(m / 12), nm = ((m % 12) + 12) % 12;
        var lastDay = new Date(ny, nm + 1, 0).getDate();
        return new Date(ny, nm, Math.min(day, lastDay));
    }
    function addYears(d, n) {
        return new Date(d.getFullYear() + n, d.getMonth(), d.getDate());
    }
    function isoWeekday(d) {
        var w = d.getDay(); // 0=Sun..6=Sat
        return w === 0 ? 7 : w;
    }
    function fmtDate(d) {
        return d.getFullYear() + '-' + pad2(d.getMonth() + 1) + '-' + pad2(d.getDate());
    }
    function fmtTime(h, m) { return pad2(h) + ':' + pad2(m); }

    function nextWdOnOrAfter(today, wd) {
        var cur = isoWeekday(today);
        var delta = ((wd - cur) % 7 + 7) % 7;
        return addDays(today, delta);
    }
    function nextWdStrict(today, wd) {
        var cur = isoWeekday(today);
        var delta = ((wd - cur) % 7 + 7) % 7;
        return addDays(today, delta === 0 ? 7 : delta);
    }

    function parseClock(s) {
        var p = s.split(':');
        if (p.length !== 2) return null;
        if (!/^\d+$/.test(p[0]) || !/^\d+$/.test(p[1])) return null;
        var h = +p[0], m = +p[1];
        if (h < 0 || h > 23 || m < 0 || m > 59) return null;
        return [h, m];
    }
    function tryHHMM(s) {
        if (!/^\d{4}$/.test(s)) return null;
        var h = +s.slice(0, 2), m = +s.slice(2, 4);
        if (h > 23 || m > 59) return null;
        return [h, m];
    }
    function tryRange(s) {
        var p = s.split('-');
        if (p.length !== 2) return null;
        var a = parseClock(p[0]), b = parseClock(p[1]);
        return (a && b) ? { time: a, end: b } : null;
    }
    function tryTime(s) {
        var r = tryRange(s);
        if (r) return r;
        var c = parseClock(s);
        if (c) return { time: c };
        var h = tryHHMM(s);
        if (h) return { time: h };
        return null;
    }
    function tryDuration(s) {
        if (s.length < 2) return null;
        var last = s.charAt(s.length - 1);
        var head = s.slice(0, -1);
        if (last === 'h') {
            if (head.indexOf(':') >= 0) {
                var p = head.split(':');
                if (p.length === 2 && /^\d+$/.test(p[0]) && /^\d+$/.test(p[1])) {
                    var hh = +p[0], mm = +p[1];
                    if (mm < 60) return hh * 60 + mm;
                }
                return null;
            }
            var v = parseFloat(head);
            if (!isNaN(v) && v >= 0) return Math.round(v * 60);
            return null;
        }
        if (last === 'm') {
            var v2 = parseInt(head, 10);
            if (!isNaN(v2) && v2 >= 0 && /^\d+$/.test(head)) return v2;
            return null;
        }
        return null;
    }
    function tryIsoDate(s) {
        var m = /^(\d{4})-(\d{2})-(\d{2})$/.exec(s);
        if (!m) return null;
        var d = new Date(+m[1], +m[2] - 1, +m[3]);
        if (isNaN(d.getTime())) return null;
        return d;
    }
    function tryDotted(s, today) {
        var sep = s.indexOf('.') >= 0 ? '.' : (s.indexOf('/') >= 0 ? '/' : null);
        if (!sep) return null;
        var p = s.split(sep).filter(function (x) { return x !== ''; });
        if (!(p.length === 2 || p.length === 3)) return null;
        if (!p.every(function (x) { return /^\d+$/.test(x); })) return null;
        var d = +p[0], m = +p[1];
        var y = p.length === 3 ? +p[2] : today.getFullYear();
        var dt = new Date(y, m - 1, d);
        if (isNaN(dt.getTime()) || dt.getMonth() !== m - 1) return null;
        return dt;
    }
    function tryDateKW(t, today) {
        if (inArr(t, TODAY_W))    return { date: today };
        if (inArr(t, TOMORROW_W)) return { date: addDays(today, 1) };
        if (inArr(t, DAYAFT))     return { date: addDays(today, 2) };
        if (inArr(t, YESTER))     return { date: addDays(today, -1) };
        if (inArr(t, TONIGHT))    return { date: today, time: [20, 0] };
        var wd = WEEKDAYS[t.toLowerCase()];
        if (wd) return { date: nextWdOnOrAfter(today, wd) };
        return null;
    }
    function unitFreq(t) {
        if (inArr(t, U_DAY))   return 'DAILY';
        if (inArr(t, U_WEEK))  return 'WEEKLY';
        if (inArr(t, U_MONTH)) return 'MONTHLY';
        if (inArr(t, U_YEAR))  return 'YEARLY';
        return null;
    }
    function tryRecur(t) {
        if (inArr(t, DAILY))   return 'FREQ=DAILY';
        if (inArr(t, WEEKLY))  return 'FREQ=WEEKLY';
        if (inArr(t, MONTHLY)) return 'FREQ=MONTHLY';
        if (inArr(t, YEARLY))  return 'FREQ=YEARLY';
        return null;
    }

    function parse(text) {
        text = text || '';
        var atIdx = text.indexOf('@');
        var location = atIdx >= 0 ? text.slice(atIdx + 1).trim() : '';
        var rest = atIdx >= 0 ? text.slice(0, atIdx) : text;
        var tokens = rest.split(/\s+/).filter(function (x) { return x !== ''; });
        var today = new Date(); today.setHours(0, 0, 0, 0);

        var R = { allDay: false };
        var titleTokens = [];

        function set(k, v) { if (R[k] == null) R[k] = v; }

        var i = 0;
        while (i < tokens.length) {
            var t = tokens[i];
            var t2 = tokens[i + 1], t3 = tokens[i + 2];

            if (t.charAt(0) === '#') {
                set('category', t.slice(1));
                i++; continue;
            }
            if (inArr(t, ALLDAY)) { R.allDay = true; i++; continue; }

            // every <weekday>, every <N> <unit>, every <unit>
            if (inArr(t, EVERY)) {
                var wd = t2 && WEEKDAYS[t2.toLowerCase()];
                if (wd) {
                    set('rrule', 'FREQ=WEEKLY;BYDAY=' + WD_CODE[wd - 1]);
                    set('date', nextWdOnOrAfter(today, wd));
                    i += 2; continue;
                }
                if (t2 && t3 && /^\d+$/.test(t2)) {
                    var f = unitFreq(t3);
                    if (f) {
                        set('rrule', 'FREQ=' + f + ';INTERVAL=' + t2);
                        i += 3; continue;
                    }
                }
                if (t2) {
                    var f2 = unitFreq(t2);
                    if (f2) { set('rrule', 'FREQ=' + f2); i += 2; continue; }
                }
            }

            // in N <unit>
            if (inArr(t, IN_W) && t2 && t3 && /^\d+$/.test(t2)) {
                var n = +t2;
                if (inArr(t3, U_DAY))   { set('date', addDays(today, n));      i += 3; continue; }
                if (inArr(t3, U_WEEK))  { set('date', addDays(today, 7 * n));  i += 3; continue; }
                if (inArr(t3, U_MONTH)) { set('date', addMonths(today, n));    i += 3; continue; }
                if (inArr(t3, U_YEAR))  { set('date', addYears(today, n));     i += 3; continue; }
            }

            // for N
            if (inArr(t, FOR) && t2) {
                var d = tryDuration(t2);
                if (d != null) { set('duration', d); i += 2; continue; }
            }

            // next <weekday>
            if (inArr(t, NEXT_W) && t2 && WEEKDAYS[t2.toLowerCase()]) {
                set('date', nextWdStrict(today, WEEKDAYS[t2.toLowerCase()]));
                i += 2; continue;
            }

            var r = tryRecur(t);
            if (r) { set('rrule', r); i++; continue; }

            var dk = tryDateKW(t, today);
            if (dk) {
                if (dk.date) set('date', dk.date);
                if (dk.time) set('time', dk.time);
                i++; continue;
            }
            var iso = tryIsoDate(t);
            if (iso) { set('date', iso); i++; continue; }
            var dot = tryDotted(t, today);
            if (dot) { set('date', dot); i++; continue; }
            var tm = tryTime(t);
            if (tm) {
                if (tm.time) set('time', tm.time);
                if (tm.end)  set('endTime', tm.end);
                i++; continue;
            }
            var du = tryDuration(t);
            if (du != null) { set('duration', du); i++; continue; }

            titleTokens.push(t);
            i++;
        }

        R.title    = titleTokens.join(' ').trim();
        R.location = location;
        return R;
    }

    function render(r) {
        if (!r.title && !r.date && !r.time) { preview.innerHTML = ''; return; }
        var parts = [];
        if (r.title)    parts.push('<span class="pv-chip">' + escapeHtml(r.title) + '</span>');
        if (r.date) {
            parts.push('<span class="pv-chip">' + fmtDate(r.date) + '</span>');
        }
        if (r.allDay) {
            parts.push('<span class="pv-chip">all day</span>');
        } else if (r.time) {
            var s = fmtTime(r.time[0], r.time[1]);
            if (r.endTime) s += '–' + fmtTime(r.endTime[0], r.endTime[1]);
            else if (r.duration) s += ' +' + r.duration + 'm';
            parts.push('<span class="pv-chip">' + s + '</span>');
        }
        if (r.rrule)    parts.push('<span class="pv-chip">' + escapeHtml(r.rrule) + '</span>');
        if (r.category) parts.push('<span class="pv-chip">#' + escapeHtml(r.category) + '</span>');
        if (r.location) parts.push('<span class="pv-chip">@ ' + escapeHtml(r.location) + '</span>');
        preview.innerHTML = parts.join(' ');
    }
    function escapeHtml(s) {
        return String(s).replace(/[&<>"']/g, function (c) {
            return { '&':'&amp;','<':'&lt;','>':'&gt;','"':'&quot;',"'":'&#39;' }[c];
        });
    }

    var timer = null;
    function schedule() {
        if (timer) clearTimeout(timer);
        timer = setTimeout(function () { render(parse(input.value)); }, 60);
    }
    input.addEventListener('input', schedule);
    schedule();
})();

// FAB + bottom-sheet promotion on narrow viewports. With JS off, the
// quick-add form is just inline at the top of the page — fully usable.
(function () {
    var form = document.querySelector('form.cal-add');
    if (!form) return;

    // Only promote on small viewports.
    var mq = window.matchMedia('(max-width: 40rem)');
    if (!mq.matches) return;

    var input = form.querySelector('[data-cal-input]');

    var fab = document.createElement('button');
    fab.type = 'button';
    fab.className = 'cal-fab';
    fab.setAttribute('aria-label', 'Add event');
    fab.textContent = '+';
    document.body.appendChild(fab);

    // Hide the inline form; it reappears as a sheet.
    form.style.display = 'none';

    function open() {
        form.style.display = '';
        document.body.classList.add('cal-sheet-open', 'cal-sheet-backdrop');
        setTimeout(function () { if (input) input.focus(); }, 50);
    }
    function close() {
        document.body.classList.remove('cal-sheet-open', 'cal-sheet-backdrop');
        form.style.display = 'none';
    }
    fab.addEventListener('click', open);
    document.addEventListener('click', function (ev) {
        if (!document.body.classList.contains('cal-sheet-open')) return;
        var t = ev.target;
        if (form.contains(t) || t === fab) return;
        close();
    });
    document.addEventListener('keydown', function (ev) {
        if (ev.key === 'Escape' &&
            document.body.classList.contains('cal-sheet-open')) close();
    });
})();

// Keyboard nav on the calendar page: j = prev, k/l = next, t = today,
// m / w / a = switch view. Ignored when typing in an input.
(function () {
    if (!document.body.classList.contains('calendar-page')) return;

    function curView() {
        var m = /[?&]view=([a-z]+)/.exec(window.location.search);
        return m ? m[1] : 'agenda';
    }
    function go(href) { window.location.href = href; }
    function navLink(rel) {
        // Reuse the prev/today/next anchors already in the page.
        var nav = document.querySelector('nav.cal-nav');
        if (!nav) return null;
        var as = nav.querySelectorAll('a');
        if (rel === 'prev')  return as[0];
        if (rel === 'today') return as[1];
        if (rel === 'next')  return as[2];
        return null;
    }

    document.addEventListener('keydown', function (ev) {
        var tag = (ev.target && ev.target.tagName) || '';
        if (tag === 'INPUT' || tag === 'TEXTAREA' || tag === 'SELECT') return;
        if (ev.ctrlKey || ev.metaKey || ev.altKey) return;
        var a;
        switch (ev.key) {
            case 'j': a = navLink('prev');  break;
            case 'k':
            case 'l': a = navLink('next');  break;
            case 't': a = navLink('today'); break;
            case 'm': go('/calendar?view=month');  return;
            case 'w': go('/calendar?view=week');   return;
            case 'a': go('/calendar?view=agenda'); return;
            default: return;
        }
        if (a) { ev.preventDefault(); go(a.getAttribute('href')); }
    });
})();
