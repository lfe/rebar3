#!/bin/bash
# Verify all old files have new replacements
# NOTE: rebar3_lfe.app.src is preserved and NOT replaced

RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m' # No Color

echo "=== Verifying replacements for old files ==="
echo

# List of old files that should have replacements
# Format: "old_file:new_file"
OLD_FILES=(
    "src/rebar3_lfe.erl:src/r3lfe.erl"
    "src/rebar3_lfe.hrl:include/r3lfe.hrl"
    "src/rebar3_lfe_prv_compile.erl:src/r3lfe_prv_compile.erl"
    "src/rebar3_lfe_prv_clean.erl:src/r3lfe_prv_clean.erl"
    "src/rebar3_lfe_prv_repl.erl:src/r3lfe_prv_repl.erl"
    "src/rebar3_lfe_prv_ltest.erl:src/r3lfe_prv_ltest.erl"
    "src/rebar3_lfe_prv_release.erl:src/r3lfe_prv_release.erl"
    "src/rebar3_lfe_prv_versions.erl:src/r3lfe_prv_versions.erl"
    "src/rebar3_lfe_prv_run.erl:src/r3lfe_prv_run.erl"
    "src/rebar3_lfe_prv_escriptize.erl:src/r3lfe_prv_escriptize.erl"
    "src/rebar3_lfe_prv_run_escript.erl:src/r3lfe_prv_run_escript.erl"
    "src/rebar3_lfe_prv_run_release.erl:src/r3lfe_prv_run_release.erl"
    "src/rebar3_lfe_prv_confabulate.erl:src/r3lfe_prv_confabulate.erl"
    "src/rebar3_lfe_utils.erl:SPLIT"
    "src/rebar3_lfe_package.erl:src/r3lfe_package.erl"
    "src/rebar3_lfe_compiler.erl:src/r3lfe_compiler.erl"
)

MISSING=0

for pair in "${OLD_FILES[@]}"; do
    OLD="${pair%%:*}"
    NEW="${pair##*:}"

    if [ -f "$OLD" ]; then
        if [ "$NEW" = "SPLIT" ]; then
            echo -e "${GREEN}✓${NC} $OLD (split into multiple r3lfe_* modules)"
        elif [ -f "$NEW" ]; then
            echo -e "${GREEN}✓${NC} $OLD → $NEW"
        else
            echo -e "${RED}✗${NC} $OLD exists but $NEW is missing!"
            MISSING=$((MISSING + 1))
        fi
    fi
done

echo
echo "Preserved files (not being replaced):"
if [ -f "src/rebar3_lfe.app.src" ]; then
    echo -e "${GREEN}✓${NC} src/rebar3_lfe.app.src (preserved)"
fi

echo
if [ $MISSING -eq 0 ]; then
    echo -e "${GREEN}All old files have replacements!${NC}"
    exit 0
else
    echo -e "${RED}ERROR: $MISSING files are missing replacements!${NC}"
    exit 1
fi
