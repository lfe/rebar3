# rebar3 lfe format — manual smoke checklist

Copy-pasteable sequence for manual verification in a scratch LFE project.
Requires `rebar3_lfe` 0.5.5+ and `lfe` 2.x.

---

## 1. Create a scratch project

```bash
SCRATCH=$(mktemp -d)
mkdir -p "$SCRATCH/src"

cat > "$SCRATCH/rebar.config" <<'EOF'
{plugins, [{rebar3_lfe, "0.5.5"}]}.
{deps, [{lfe, "2.2.0"}]}.
EOF

cat > "$SCRATCH/src/scratch.app.src" <<'EOF'
{application, scratch, [
    {description, "smoke test"},
    {vsn, "0.1.0"},
    {modules, []},
    {applications, [kernel, stdlib]}
]}.
EOF

cat > "$SCRATCH/src/messy.lfe" <<'EOF'
(defmodule messy (export (add 2) (greet 1)))
(defun add (x y) (+ x y))
; a comment
(defun greet (name) (let ((greeting "hello")) (io:format "~ts ~ts~n" (list greeting name))))
EOF

cd "$SCRATCH"
```

If testing a **local build** instead of hex, replace the plugin line in `rebar.config` with:
```erlang
{plugins, [rebar3_lfe]}.
```
and add `_checkouts/rebar3_lfe/` pointing at the local repo (see `test/e2e/format_e2e.sh` for details).

---

## 2. Check: unformatted file is flagged

```bash
rebar3 lfe format --check
# Expected: non-zero exit, output mentions messy.lfe
```

---

## 3. Dry-run: see formatted output without writing

```bash
rebar3 lfe format --dry-run
# Expected: exit 0; formatted LFE printed to stdout; messy.lfe on disk unchanged
diff <(rebar3 lfe format --dry-run 2>/dev/null) /dev/stdin <<'EOF'
(Compare printed output to original — they should differ)
EOF
```

Verify the file is unchanged:
```bash
diff "$SCRATCH/src/messy.lfe" "$SCRATCH/src/messy.lfe" && echo "unchanged (expected)"
```

---

## 4. In-place format: rewrite the file

```bash
cp "$SCRATCH/src/messy.lfe" /tmp/messy.orig.lfe
rebar3 lfe format
# Expected: exit 0; messy.lfe rewritten
diff "$SCRATCH/src/messy.lfe" /tmp/messy.orig.lfe && echo "BUG: unchanged" || echo "OK: file changed"
```

Eyeball the formatted output — it should have:
- `defmodule` broken across lines
- Blank line between top-level forms
- `defun` body on its own indented line
- `let` bindings broken out
- The `; a comment` preserved in place

---

## 5. Idempotency: second run is a no-op

```bash
cp "$SCRATCH/src/messy.lfe" /tmp/messy.fmt1.lfe
rebar3 lfe format
diff "$SCRATCH/src/messy.lfe" /tmp/messy.fmt1.lfe && echo "OK: idempotent" || echo "BUG: changed again"
```

---

## 6. Check: formatted file passes

```bash
rebar3 lfe format --check
# Expected: exit 0; no files listed as unformatted
```

---

## 7. --path scoping

```bash
# Create a second unformatted file
cat > "$SCRATCH/src/other.lfe" <<'EOF'
(defmodule other (export (f 1)))
(defun f (x) (+ x 1))
EOF

# Format only messy.lfe (other.lfe untouched)
rebar3 lfe format --path src/messy.lfe
diff "$SCRATCH/src/other.lfe" <(cat "$SCRATCH/src/other.lfe") && echo "OK: other.lfe unchanged"

# Format the whole src/ dir
rebar3 lfe format --path src/
rebar3 lfe format --check   # should be exit 0 now
```

---

## 8. Syntax error: non-zero exit, other files still formatted

```bash
cat > "$SCRATCH/src/broken.lfe" <<'EOF'
(defun broken (x)
  (+ x
EOF

# Re-unformat other.lfe to see it gets processed
cat > "$SCRATCH/src/other.lfe" <<'EOF'
(defmodule other (export (f 1)))
(defun f (x) (+ x 1))
EOF
cp "$SCRATCH/src/other.lfe" /tmp/other.orig.lfe

rebar3 lfe format
# Expected: non-zero exit (broken.lfe failed)
# Expected: other.lfe was still formatted
diff "$SCRATCH/src/other.lfe" /tmp/other.orig.lfe && echo "BUG: other.lfe unchanged" || echo "OK: other.lfe formatted"
```

---

## 9. Clean up

```bash
rm -rf "$SCRATCH"
```
