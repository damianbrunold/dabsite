// Loaded synchronously in <head> so the user's chosen theme is applied
// before first paint. Kept tiny so blocking the head is cheap. The
// rest of the JS lives in site.js (deferred).
(function () {
    try {
        var t = localStorage.getItem('theme');
        if (t === 'dark' || t === 'light') {
            document.documentElement.setAttribute('data-theme', t);
        }
    } catch (e) { /* ignore */ }
})();
