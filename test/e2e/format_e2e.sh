#!/usr/bin/env bash
# End-to-end test for `rebar3 lfe format` through the real plugin machinery.
# Creates a temp consuming project, exercises all CLI modes, then cleans up.
# Usage: bash test/e2e/format_e2e.sh [--keep-tmp]
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
REBAR3="${REBAR3:-rebar3}"
KEEP_TMP="${1:-}"

E2E_TMP="$(mktemp -d)"

cleanup() {
    if [ "$KEEP_TMP" = "--keep-tmp" ]; then
        echo "(temp dir kept at $E2E_TMP)"
    else
        rm -rf "$E2E_TMP"
    fi
}
trap cleanup EXIT

# ---- Assertion helpers -------------------------------------------------------

OUT="$E2E_TMP/cmd_out.txt"

# Run command; capture stdout+stderr to $OUT; return exit code.
capture() {
    local ec=0
    "$@" >"$OUT" 2>&1 || ec=$?
    return $ec
}

pass() { printf '  PASS  %s\n' "$*"; }

fail() {
    printf '  FAIL  %s\n' "$*" >&2
    if [ -s "$OUT" ]; then
        echo "--- output ---" >&2
        cat "$OUT" >&2
        echo "--- end ---" >&2
    fi
    exit 1
}

assert_zero() {
    local label="$1"; shift
    local ec=0; capture "$@" || ec=$?
    [ "$ec" -eq 0 ] || fail "$label: exit $ec (expected 0)"
    pass "$label"
}

assert_nonzero() {
    local label="$1"; shift
    local ec=0; capture "$@" || ec=$?
    [ "$ec" -ne 0 ] || fail "$label: exit 0 (expected non-zero)"
    pass "$label"
}

# Asserts the last capture() output contains a fixed string.
assert_out_has() {
    local label="$1" pattern="$2"
    grep -qF "$pattern" "$OUT" || fail "$label: pattern '$pattern' not in output"
    pass "$label"
}

# Asserts the last capture() output does NOT contain a fixed string.
assert_out_not_has() {
    local label="$1" pattern="$2"
    if grep -qF "$pattern" "$OUT" 2>/dev/null; then
        fail "$label: unexpected pattern '$pattern' found in output"
    fi
    pass "$label"
}

assert_same() {
    local label="$1" a="$2" b="$3"
    if ! diff -q "$a" "$b" >/dev/null 2>&1; then
        fail "$label: $a and $b differ (expected same)"
    fi
    pass "$label"
}

assert_diff() {
    local label="$1" a="$2" b="$3"
    if diff -q "$a" "$b" >/dev/null 2>&1; then
        fail "$label: $a and $b are identical (expected different)"
    fi
    pass "$label"
}

# ---- Create fixture project --------------------------------------------------

echo "==> Creating fixture project in $E2E_TMP"
mkdir -p "$E2E_TMP/src"

cat > "$E2E_TMP/src/e2efmtapp.app.src" <<'APPSRC'
{application, e2efmtapp, [
    {description, "e2e format test fixture"},
    {vsn, "0.1.0"},
    {modules, []},
    {applications, [kernel, stdlib]}
]}.
APPSRC

# Load the plugin from the local repo via _checkouts (works in rebar3 3.27+
# where {path,...} plugin loading is not supported).
# The checkout contains only the compiled ebin so it does not conflict with
# the consuming project's own src/ when rebar3 discovers apps.
CHECKOUT="$E2E_TMP/_checkouts/rebar3_lfe"
mkdir -p "$CHECKOUT/ebin" "$CHECKOUT/src"
cp -rL "$REPO_ROOT/_build/default/lib/rebar3_lfe/ebin/." "$CHECKOUT/ebin/"
cat > "$CHECKOUT/src/rebar3_lfe.app.src" <<'APPSRC'
{application, rebar3_lfe, [
    {description, "rebar3 LFE plugin (local checkout for e2e)"},
    {vsn, "0.5.5"},
    {modules, []},
    {applications, [kernel, stdlib]}
]}.
APPSRC

cat > "$E2E_TMP/rebar.config" <<'REBAR'
{plugins, [rebar3_lfe]}.
REBAR

# Deliberately unformatted: defmodule on one line, defun bodies inline,
# flat let, no blank lines between top-level forms.
cat > "$E2E_TMP/src/messy.lfe" <<'MESSY'
(defmodule messy (export (add 2) (greet 1)))
(defun add (x y) (+ x y))
; a comment
(defun greet (name) (let ((greeting "hello")) (io:format "~ts ~ts~n" (list greeting name))))
MESSY

cp "$E2E_TMP/src/messy.lfe" "$E2E_TMP/src/messy.lfe.orig"
cd "$E2E_TMP"

echo "==> Fixture ready."
echo ""

echo "==> Running e2e assertions"
echo ""

# ---- [1] --check on unformatted file → non-zero + names messy.lfe ----------
echo "--- [1] --check on unformatted file ---"
assert_nonzero "[1a] --check exits non-zero on unformatted file"  \
    "$REBAR3" lfe format --check
assert_out_has "[1b] --check output names messy.lfe"  "messy.lfe"
# Verify the lfe format command was found (not "Command lfe not found").
assert_out_not_has "[1c] lfe command resolved"  "Command lfe not found"

# ---- [2] --dry-run → exit 0, formatted output, disk unchanged --------------
echo ""
echo "--- [2] --dry-run ---"
assert_zero    "[2a] --dry-run exits 0"  "$REBAR3" lfe format --dry-run
assert_out_has "[2b] --dry-run output contains formatted LFE"  "(defmodule"
assert_same    "[2c] messy.lfe unchanged on disk after --dry-run"  \
    "$E2E_TMP/src/messy.lfe" "$E2E_TMP/src/messy.lfe.orig"

# ---- [3] in-place format → exit 0, messy.lfe rewritten --------------------
echo ""
echo "--- [3] in-place format ---"
assert_zero "[3a] format exits 0"    "$REBAR3" lfe format
assert_diff "[3b] messy.lfe rewritten"  \
    "$E2E_TMP/src/messy.lfe" "$E2E_TMP/src/messy.lfe.orig"

# ---- [4] idempotency: second run produces no change -----------------------
echo ""
echo "--- [4] idempotency ---"
cp "$E2E_TMP/src/messy.lfe" "$E2E_TMP/src/messy.lfe.fmt1"
assert_zero "[4a] second format exits 0"       "$REBAR3" lfe format
assert_same "[4b] second format is a no-op"    \
    "$E2E_TMP/src/messy.lfe" "$E2E_TMP/src/messy.lfe.fmt1"

# ---- [5] --check after format → exit 0 ------------------------------------
echo ""
echo "--- [5] --check after in-place format ---"
assert_zero "[5] --check exits 0 after format"  "$REBAR3" lfe format --check

# ---- [6] --path scoping ----------------------------------------------------
echo ""
echo "--- [6] --path scoping ---"

# Create two extra unformatted files.
cat > "$E2E_TMP/src/other1.lfe" <<'LFE'
(defmodule other1 (export (f 1)))
(defun f (x) (+ x 1))
LFE
cat > "$E2E_TMP/src/other2.lfe" <<'LFE'
(defmodule other2 (export (g 1)))
(defun g (x) (* x 2))
LFE
cp "$E2E_TMP/src/other1.lfe" "$E2E_TMP/src/other1.lfe.orig"
cp "$E2E_TMP/src/other2.lfe" "$E2E_TMP/src/other2.lfe.orig"

# --path <file>: formats only other1.lfe, not other2.lfe.
assert_zero "[6a] --path file exits 0"  \
    "$REBAR3" lfe format --path src/other1.lfe
assert_diff "[6b] targeted file (other1.lfe) was formatted"  \
    "$E2E_TMP/src/other1.lfe" "$E2E_TMP/src/other1.lfe.orig"
assert_same "[6c] non-targeted file (other2.lfe) was not touched"  \
    "$E2E_TMP/src/other2.lfe" "$E2E_TMP/src/other2.lfe.orig"

# --path <dir>: formats other2.lfe (and already-formatted files are no-op).
assert_zero "[6d] --path dir exits 0"   \
    "$REBAR3" lfe format --path src/
assert_diff "[6e] directory-scoped format reached other2.lfe"  \
    "$E2E_TMP/src/other2.lfe" "$E2E_TMP/src/other2.lfe.orig"

# ---- [7] syntax error: non-zero exit; other files still processed ----------
echo ""
echo "--- [7] syntax error handling ---"

# Re-unformat other1.lfe so we can verify it gets processed despite broken.lfe.
# Save snapshot outside src/ so it isn't discovered and formatted by the provider.
cp "$E2E_TMP/src/other1.lfe.orig" "$E2E_TMP/src/other1.lfe"
cp "$E2E_TMP/src/other1.lfe"      "$E2E_TMP/other1_before.lfe"

cat > "$E2E_TMP/src/broken.lfe" <<'LFE'
(defun broken (x)
  (+ x
LFE

assert_nonzero "[7a] format with broken.lfe exits non-zero"  \
    "$REBAR3" lfe format
assert_diff    "[7b] other1.lfe was still formatted despite broken.lfe"  \
    "$E2E_TMP/src/other1.lfe" "$E2E_TMP/other1_before.lfe"

# ---- [8] rebar3 lfe clean: standalone app-discovery fix ---------------------
echo ""
echo "--- [8] rebar3 lfe clean (standalone app-discovery) ---"

# Remove broken.lfe and relocate remaining .lfe files out of src/.
# rebar3_lfe hooks into `rebar3 compile` and calls lfe_comp on any .lfe files
# it finds — but the fixture has no `lfe` dep, so lfe_comp is undefined.
# We only need the Erlang compiler to produce a .beam, so clear .lfe files
# from src/ first.  All format assertions are already done by this point.
rm -f "$E2E_TMP/src/broken.lfe"
mv "$E2E_TMP/src/"*.lfe "$E2E_TMP/" 2>/dev/null || true

# Add an Erlang stub so rebar3 compile produces a .beam via the standard
# Erlang compiler.
cat > "$E2E_TMP/src/e2efmtapp_stub.erl" <<'ERL'
-module(e2efmtapp_stub).
-export([hello/0]).
hello() -> world.
ERL

assert_zero "[8a] rebar3 compile exits 0"  "$REBAR3" compile

EBIN="$E2E_TMP/_build/default/lib/e2efmtapp/ebin"
if [ -z "$(ls "$EBIN"/*.beam 2>/dev/null)" ]; then
    fail "[8b] rebar3 compile produced no .beam files"
fi
pass "[8b] rebar3 compile produced .beam files"

# rebar3 lfe clean with no args must discover the project app and remove beams.
# Pre-fix: project_apps=[] (bare provider ran before app_discovery) → nothing removed.
# Post-fix: app_discovery dep ensures project_apps is populated → beams removed.
assert_zero    "[8c] rebar3 lfe clean exits 0"  "$REBAR3" lfe clean
assert_out_not_has "[8d] lfe clean command resolved"  "Command lfe not found"

if [ -n "$(ls "$EBIN"/*.beam 2>/dev/null)" ]; then
    fail "[8e] rebar3 lfe clean left .beam files behind (app-discovery not working)"
fi
pass "[8e] rebar3 lfe clean removed all .beam files"

# ---- Done -------------------------------------------------------------------
echo ""
echo "==> All e2e assertions passed."
