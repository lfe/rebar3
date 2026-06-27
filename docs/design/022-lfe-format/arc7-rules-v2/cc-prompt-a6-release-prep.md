# 022 · A6 — release prep → 0.5.5 readiness (CC spec)

> Target: Sonnet 4.6 + `erlang-guidelines`. Release-prep (docs + verification).
> **CC does NOT publish, tag, branch, commit, or push** — produce the docs and a
> readiness report; Duncan executes the release. **Stop and report** if a
> verification step fails.
>
> **Output discipline:** Edit in place; don't reprint files; terse prose; run the
> checks; report briefly.

## 0. Orientation

1. `CLAUDE.md`; `erlang-guidelines`. **Safety:** never auto-pass skip-gate flags;
   never publish/push (that is Duncan's call).
2. State already in place — do **not** redo: `vsn` is `0.5.5` in
   `src/rebar3_lfe.app.src`; `r3lfe_prv_format` is registered; `r3lfe_prv_clean`
   already has `?DEPS = [{default, app_discovery}]`; the formatter + gallery are
   verification-complete (A7/S6).
3. Files to touch: `docs/commands.md`, `docs/release-notes.md`, `README.md` (verify).
4. Checklist to satisfy: `docs/release-checklist.md` (note it references
   `release-history.md`/`r3lfe.app.src` by old names — the real files are
   `docs/release-notes.md` and `src/rebar3_lfe.app.src`).

## 1. docs/commands.md — add a `### format` entry

Under **`## Core Commands`** (alongside `compile`/`clean`/`repl`/…), add a
`### format` section matching the existing style. Cover:

- **What it does:** reformats LFE source to LFE style conventions
  (`lfe-indent.el`-derived; 80-col; 2-space). **Comment-preserving, idempotent,
  token-preserving** (export/import entry-sort is the one intentional reorder),
  **AST-equivalent**. Zero new runtime dependencies.
- **Behavior summary (knowledge-gated):** known forms are laid out canonically;
  unknown/data forms preserve the author's line breaks (reindented). export/import
  are one-per-line, +1 under the keyword, alphabetically sorted (sort suppressed
  when an entry carries a comment).
- **Usage + flags** (match README's section):
  - default — **in place** (files rewritten): `rebar3 lfe format`
  - `--dry-run` — formatted output to **stdout**, no files changed
  - `--check` — **CI mode**: non-zero exit if any file is not already formatted,
    writes nothing
  - `--path P` — restrict to a dir (recursive) or a single `.lfe` file
- **Examples** mirroring the README block (whole project / `--path` / `--dry-run` /
  `--check`).

Verify the flag set against `src/r3lfe_prv_format.erl` before documenting (don't
invent flags).

## 2. docs/release-notes.md — add a `## [0.5.5]` entry

Above `## [0.5.4]`, add a `## [0.5.5] - <date>` section (Keep-a-Changelog style,
matching the file). Headline: **new `rebar3 lfe format` command — an LFE source
formatter.** Include:

- **### Added** — `rebar3 lfe format` with `--dry-run` / `--check` / `--path`;
  comment-preserving, idempotent, token-/AST-preserving formatting; knowledge-gated
  layout; export/import one-per-line + alphabetical sort; clean-room
  comment-preserving lexer + CST (no dependency on `lfe_scan` for comments); zero
  new runtime deps.
- **### Fixed** (if noting it) — `r3lfe_prv_clean`/provider `?DEPS` so bare
  `rebar3 lfe format`/`clean` run after app discovery (found via real-CLI e2e).
- **Upgrade urgency / breaking changes:** none — purely additive.
- Use a placeholder date if unknown (`- TBD`) and flag it for Duncan to set.

## 3. README.md — verify (light touch)

Confirm the **📐 Consistent Formatting** section is accurate against final v2
behavior: that it documents `--check` (not just `--dry-run`/`--path`), and that any
behavior blurb matches the knowledge-gated model + export/import sort. Fix only
genuine inaccuracies; don't rewrite a correct section.

## 4. Verification — run, confirm, report (no release actions)

Run and record results (state numbers; **stop and report** on any failure):

- `make check` (build + lint + full test suite) — green.
- `make smoke-tests` (or `test/e2e/format_e2e.sh` + the `SMOKE.md` steps) — the real
  `rebar3 lfe format` CLI path (in-place / `--dry-run` / `--check` / `--path`).
- `xref` + `dialyzer` clean; `warnings_as_errors` (no warnings).
- Confirm `vsn = "0.5.5"`.

Then produce a **release-readiness report** mapping to `docs/release-checklist.md`:
each item ✅ / ⚠️ / N/A with a one-line note. Mark the **release-execution** items
(branch, commit, tag, push, `rebar3 hex publish`, multi-OTP CI 24–28, hex.pm
verify) as **Duncan-owned — not done here**. Do not perform them.

## 5. Constraints

Docs + verification only — **no `src/` behavior changes** (the formatter is frozen
post-S6). No publish/tag/branch/commit/push. Don't auto-pass any skip-gate flag to
`make`/`rebar3`/`hex`. If a verification step fails, that's a release blocker →
stop and report (don't paper over).

## 6. Ledger

```
A6 — 0.5.5 release prep (docs + readiness)
[ ] docs/commands.md: ### format added (Core Commands), flags verified vs r3lfe_prv_format.erl
[ ] docs/release-notes.md: ## [0.5.5] entry (Added/Fixed/no-breaking; date or TBD)
[ ] README 📐 section verified (—check documented; blurb matches v2) — fixed or "accurate as-is"
[ ] make check green; smoke/e2e green; xref + dialyzer clean; vsn 0.5.5 confirmed (state numbers)
[ ] release-readiness report vs docs/release-checklist.md (execution items flagged Duncan-owned)
[ ] NO src changes; NO publish/tag/branch/commit/push
[ ] files changed + one-line rationale; blockers surfaced; deviations — or "none"
```

After A6 + a green readiness report, Duncan executes the release
(branch → commit → tag → `rebar3 hex publish` → verify).
