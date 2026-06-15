# 022 · Arc A6 · S0 — end-to-end CLI test (the real `rebar3 lfe format`)

> Target: Sonnet 4.6 + `erlang-guidelines`. **Do this FIRST in A6** — highest-
> signal pre-release check. Everything so far tests `r3lfe_prv_format:do/1`
> directly; nothing exercises the actual `rebar3 lfe format` command through
> rebar3's plugin machinery. **Stop and report** if blocked.
>
> **Output discipline:** write the script/fixture with Write; don't reprint large
> output; run it; report the transcript tersely.

## 0. Why this slice exists

`r3lfe_prv_format:do/1` unit tests **cannot** catch: a wrong/missing provider
registration in `rebar3_lfe.erl`, a typo in the `opts` list, the command not
resolving (`===> Command lfe not found`), getopt mis-parsing `--dry-run`/
`--check`/`--path`, or exit codes not propagating. Only running the **real**
command against a project that **loads the plugin** exercises that path.

Running `rebar3 lfe format` inside this repo fails (`Command lfe not found`) —
correct: the repo is the plugin *source*; it doesn't load itself as a plugin. The
command only exists in a **consuming** project.

## 1. Deliverable — a scripted, repeatable e2e

Create `test/e2e/format_e2e.sh` (bash, `set -euo pipefail`) and a Makefile target
`test-format-e2e` that runs it. The script:

1. Make a temp consuming project (e.g. under `$(mktemp -d)`), with:
   - `rebar.config` that loads the plugin **by local path** (not Hex):
     ```erlang
     {plugins, [{rebar3_lfe, {path, "<ABS_PATH_TO_THIS_REPO>"}}]}.
     {deps, [{lfe, "2.2.0"}]}.
     ```
     (pass the repo path in; resolve it from the script's location).
   - `src/<app>.app.src` and a deliberately **unformatted** `src/messy.lfe`
     (bad indentation, a wide call, a flat `let`/`case`, a comment) that is valid
     LFE.
2. Run and **assert** (fail loudly with a clear message on any mismatch):
   - `rebar3 lfe format --check` → **non-zero** exit, and stdout/stderr names
     `messy.lfe`. (Also asserts the command is *found* — no "Command lfe not
     found".)
   - `rebar3 lfe format --dry-run` → exit 0; stdout contains the formatted result;
     `messy.lfe` on disk is **unchanged** (compare to a saved copy).
   - `rebar3 lfe format` (in place) → exit 0; `messy.lfe` is now rewritten and
     **differs** from the original.
   - `rebar3 lfe format` again → **idempotent** (no further change); and
     `rebar3 lfe format --check` now → exit 0.
   - `rebar3 lfe format --path src/messy.lfe` and `--path src/` → scope works
     (touches only the target).
   - a `src/broken.lfe` with a syntax error → `format` reports it and exits
     non-zero, while other files still format.
3. Clean up the temp dir on exit.

If the environment lacks network to fetch `lfe`, document that the script needs
deps available (or point `lfe` at a local checkout) — but the script itself is the
committed artifact regardless.

## 2. Also: a manual smoke checklist (for Duncan)

Add `docs/design/022-lfe-format/SMOKE.md` — a copy-pasteable sequence Duncan can
run by hand in a scratch LFE project (create project, add the path plugin, run
each of the modes, eyeball output). Short and concrete.

## 3. If the e2e surfaces a bug

A registration/opts/command-resolution bug found here is **real and
release-blocking** — fix it minimally (in `r3lfe_prv_format.erl` /
`rebar3_lfe.erl`), keep the provider CT suite green, and note the fix. If it's
behavior-changing beyond wiring, stop and report.

## 4. Constraints

No publishing actions. Don't weaken any rebar3 safety gates. The script must be
deterministic and self-cleaning. `warnings_as_errors` unaffected (this is a
script + Makefile target, not src).

## 5. Ledger

```
Arc A6·S0 — e2e CLI
[ ] test/e2e/format_e2e.sh + Makefile `test-format-e2e` target created
[ ] fixture loads plugin by local path; command resolves (no "Command lfe not found")
[ ] asserts: --check (non-zero+lists), --dry-run (stdout + no write), in-place
    (rewrites), idempotent re-run, --check-after (zero), --path scoping, syntax-error
[ ] script run; transcript pasted (exit codes shown); all assertions pass
    — OR environment limitation documented with the exact failing step
[ ] any wiring bug found → fixed minimally; provider CT suite still green
[ ] docs/design/022-lfe-format/SMOKE.md manual checklist added
[ ] files changed + one-line rationale; deviations named — or "none"
```

Run this before A6·S1/S2. A green e2e here is what lets us trust the README's
promises end-to-end.
