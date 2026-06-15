# 022 · sidecar — fix `r3lfe_prv_clean` app-discovery bug (bundle into 0.5.5)

> Target: Sonnet 4.6 + `erlang-guidelines`. **Small, standalone bug fix** — a
> pre-existing plugin bug (NOT part of the formatter feature), surfaced by analogy
> during the A6·S0 e2e audit. Bundling into 0.5.5 since we're in this code and the
> e2e harness already exists. **Stop and report** if blocked.
>
> **Output discipline:** Edit in place; terse prose; run the e2e + suite; report
> briefly.

## 0. The bug (same class as the one A6·S0 fixed for `format`)

`src/r3lfe_prv_clean.erl` declares `-define(DEPS, [])` and selects apps via
`current_app` / `project_apps` — exactly the pattern that made `rebar3 lfe format`
silently find **0 apps** when run standalone (a bare provider runs *before* app
discovery, so `project_apps/1` is empty). So `rebar3 lfe clean`, run on its own,
very likely **discovers nothing and cleans nothing**. `format` was fixed with
`{default, app_discovery}`; `clean` needs the same.

(`r3lfe_prv_clean_SUITE` calls `do/1` with a hand-built state, so it cannot catch
this — same blind spot as before. The real proof is an e2e run.)

## 1. The fix

In `src/r3lfe_prv_clean.erl`: change `-define(DEPS, []).` to
`-define(DEPS, [{default, app_discovery}]).` (matching `r3lfe_prv_format`). No
other change to clean's logic.

## 2. Prove it via the e2e harness

Extend `test/e2e/format_e2e.sh` (the A6·S0 script) with a `clean` section:
1. `rebar3 lfe compile` in the fixture project → assert `ebin/` now contains
   `*.beam` (compilation produced artifacts).
2. `rebar3 lfe clean` (standalone, **no** `--path`/args) → assert the `*.beam`
   files are **gone** from `ebin/`. Without the fix this fails (0 apps discovered →
   nothing removed); with the fix it passes. Also assert the command resolved
   (no `Command lfe not found`).

Keep the section self-contained and self-cleaning like the rest of the script.

## 3. Verify

- The existing `r3lfe_prv_clean_SUITE` still passes (do/1-level behavior unchanged
  for the explicit-app case).
- Full `rebar3 ct` green.
- `make test-format-e2e` (now covering clean too) passes end to end.

## 4. Constraints

`warnings_as_errors`; `xref`/`dialyzer` at standing level. No publishing actions.
Don't weaken rebar3 safety gates. This is a one-line src change + an e2e assertion.

## 5. Ledger

```
sidecar — clean app-discovery fix
[ ] r3lfe_prv_clean ?DEPS: [] → [{default, app_discovery}]
[ ] e2e: compile produces .beam; standalone `rebar3 lfe clean` removes them
    (would fail pre-fix); command resolves
[ ] r3lfe_prv_clean_SUITE still green; full ct green; make test-format-e2e green
[ ] files changed + one-line rationale; deviations named — or "none"
[ ] CHANGELOG (in A6·S2): note the clean standalone-discovery fix under 0.5.5
```
