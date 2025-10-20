#!/bin/bash
# Remove old rebar3_lfe files
# IMPORTANT: rebar3_lfe.app.src is PRESERVED and NOT removed

set -e  # Exit on error

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

echo "=== Phase 8: Removing Old Code ==="
echo
echo -e "${YELLOW}NOTE: rebar3_lfe.app.src will be PRESERVED (not removed)${NC}"
echo

# Confirm with user
read -p "This will DELETE old rebar3_lfe files. Continue? (yes/no): " confirm
if [ "$confirm" != "yes" ]; then
    echo "Aborted."
    exit 1
fi

echo
echo "Removing old files..."

# List of old source files to remove
# IMPORTANT: Excludes rebar3_lfe.app.src which must be preserved
OLD_SRC_FILES=(
    "src/rebar3_lfe.erl"
    "src/rebar3_lfe_clean.erl"
    "src/rebar3_lfe_package.erl"
    "src/rebar3_lfe_prv_clean_all.erl"
    "src/rebar3_lfe_prv_clean_build.erl"
    "src/rebar3_lfe_prv_clean_cache.erl"
    "src/rebar3_lfe_prv_clean.erl"
    "src/rebar3_lfe_prv_compile.erl"
    "src/rebar3_lfe_prv_confabulate.erl"
    "src/rebar3_lfe_prv_escriptize.erl"
    "src/rebar3_lfe_prv_ltest.erl"
    "src/rebar3_lfe_prv_release.erl"
    "src/rebar3_lfe_prv_repl.erl"
    "src/rebar3_lfe_prv_run_escript.erl"
    "src/rebar3_lfe_prv_run_release.erl"
    "src/rebar3_lfe_prv_run.erl"
    "src/rebar3_lfe_prv_versions.erl"
    "src/rebar3_lfe_prv_xrepl.erl"
    "src/rebar3_lfe_repl.erl"
    "src/rebar3_lfe_utils.erl"
    "src/rebar3_lfe_version.erl"
)

for file in "${OLD_SRC_FILES[@]}"; do
    if [ -f "$file" ]; then
        echo -e "  ${RED}Removing${NC} $file"
        git rm -f "$file" 2>/dev/null || rm -f "$file"
    fi
done

# Remove old header files
OLD_HEADER_FILES=(
    "src/rebar3_lfe.hrl"
    "include/rebar3_lfe.hrl"
)

for file in "${OLD_HEADER_FILES[@]}"; do
    if [ -f "$file" ]; then
        echo -e "  ${RED}Removing${NC} $file"
        git rm -f "$file" 2>/dev/null || rm -f "$file"
    fi
done

# Remove old test files (if any)
# Check first to avoid errors
OLD_TESTS=$(find test -name "rebar3_lfe*.erl" -type f 2>/dev/null)
if [ -n "$OLD_TESTS" ]; then
    echo "$OLD_TESTS" | while read file; do
        echo -e "  ${RED}Removing${NC} $file"
        git rm -f "$file" 2>/dev/null || rm -f "$file"
    done
fi

echo
echo -e "${GREEN}Old files removed!${NC}"

echo
echo -e "${YELLOW}Preserved files:${NC}"
if [ -f "src/rebar3_lfe.app.src" ]; then
    echo -e "  ${GREEN}✓${NC} src/rebar3_lfe.app.src"
fi

echo
echo "Remaining rebar3_lfe source files:"
find src -name "rebar3_lfe*.erl" -type f 2>/dev/null | while read file; do
    echo -e "  ${RED}WARNING: $file still exists${NC}"
done || echo -e "  ${GREEN}(none - all removed)${NC}"

echo
echo "Next steps:"
echo "  1. Verify everything compiles: rebar3 compile"
echo "  2. Run tests: rebar3 ct"
echo "  3. Check xref: rebar3 xref"
echo "  4. Commit: git commit -m 'Remove old rebar3_lfe code (Phase 8)'"
