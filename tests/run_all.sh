#!/bin/bash
# Runs every test_*.scm file in this directory, summarises pass/fail.
set -u
cd "$(dirname "$0")/.."

fail=0
total=0
for f in tests/test_*.scm; do
    total=$((total + 1))
    printf '%-30s' "$(basename "$f")"
    if scm "$f" > /tmp/_test_out.$$ 2>&1; then
        # Last "  name  N OK" line summary printed by scm-test goes through;
        # keep our line short.
        tail -1 /tmp/_test_out.$$ | sed 's/^ *//'
    else
        echo "FAIL"
        cat /tmp/_test_out.$$
        fail=$((fail + 1))
    fi
    rm -f /tmp/_test_out.$$
done

echo
echo "------------------------------------------------------------"
if [ "$fail" -eq 0 ]; then
    echo "All $total test files passed."
    exit 0
else
    echo "$fail of $total test files FAILED."
    exit 1
fi
