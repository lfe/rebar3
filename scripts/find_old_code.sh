#!/bin/bash
# Find all old rebar3_lfe files (excluding rebar3_lfe.app.src which is preserved)

echo "=== Finding old rebar3_lfe files ==="
echo

echo "Source files:"
find src -name "rebar3_lfe*.erl" -type f 2>/dev/null | sort

echo
echo "Include files:"
find include -name "rebar3_lfe*" -type f 2>/dev/null | sort
find src -name "rebar3_lfe*.hrl" -type f 2>/dev/null | sort

echo
echo "Test files:"
find test -name "rebar3_lfe*" -type f 2>/dev/null | sort

echo
echo "Preserved files (NOT to be removed):"
find . -name "rebar3_lfe.app.src" -type f 2>/dev/null

echo
echo "=== Summary ==="
OLD_ERLS=$(find src -name "rebar3_lfe*.erl" -type f 2>/dev/null | wc -l | tr -d ' ')
OLD_HRLS=$(find src include -name "rebar3_lfe*.hrl" -type f 2>/dev/null | wc -l | tr -d ' ')
OLD_TESTS=$(find test -name "rebar3_lfe*" -type f 2>/dev/null | wc -l | tr -d ' ')
TOTAL=$((OLD_ERLS + OLD_HRLS + OLD_TESTS))
echo "Total old files to remove: $TOTAL"
echo "  - Source files (.erl): $OLD_ERLS"
echo "  - Header files (.hrl): $OLD_HRLS"
echo "  - Test files: $OLD_TESTS"
