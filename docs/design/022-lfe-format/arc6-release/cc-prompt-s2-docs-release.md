# 022 · Arc A6 · S2 — docs + release prep (CC spec)

> Target: Sonnet 4.6 + `erlang-guidelines`. Final A6 sub-slice — **completes the
> feature and prepares 0.5.5**. Prereq: A6·S1 merged. **Implementation spec** —
> implement exactly. **Stop and report** if blocked.
>
> **Output discipline:** Edit in place; don't reprint files; terse prose; run
> checks; report briefly.
>
> **Release safety (per CLAUDE.md):** this slice **prepares**, it does **not**
> publish. Do NOT run `make publish` / `deno publish` / `cargo publish` /
> `git push` / `git tag`, and do NOT pass `--allow-dirty`/`--no-verify`/`--force`
> to anything. Produce a checklist for Duncan.

## 0. Orientation

1. `CLAUDE.md` (conventions; publishing rules); `erlang-guidelines`.
2. `README.md` "📐 Consistent Formatting" (already written — the source of truth
   for documented behavior).
3. `docs/commands.md` (read it first; the README links here for the full command
   reference) and `docs/design/022-lfe-format/formatting-gallery.md`.
4. `src/rebar3_lfe.app.src` (`vsn` — already `"0.5.5"`); `CHANGELOG*` (find or
   create).

## 1. Scope of THIS slice

### (a) `docs/commands.md` — full `format` reference

Read the existing per-command structure in `docs/commands.md` and add a `format`
entry that matches that style. Include: synopsis; the three modes (in-place
default, `--dry-run`/`-n` to stdout, `--check`/`-c` CI) and `--path`/`-p`; the
full usage matrix (project / dir / file × in-place / dry-run); exit codes
(in-place & dry-run → 0 unless a file fails to parse; `--check` → non-zero if any
file is unformatted or fails); behavior notes — operates on configured `src_dirs`
(default `src/`) without `--path`; **comments preserved**; output is **LF-only**
(CRLF normalized); **idempotent**; 80-col / 2-space / CL-aligned indentation.
Keep examples consistent with the README.

### (b) CHANGELOG

Find the changelog (`CHANGELOG.md` or similar; if none exists, create one
matching common rebar3/Hex conventions). Add a **0.5.5** entry summarizing: new
`rebar3 lfe format` command (in-place / `--dry-run` / `--check` / `--path`),
comment-preserving reflow formatter, LFE house style + CL alignment. Link to the
gallery and the README section.

### (c) Version confirm

Confirm `src/rebar3_lfe.app.src` `vsn` is `"0.5.5"` (it is). If the README's
plugin-version example or any docs reference `0.5.x`, leave generic refs alone but
ensure nothing pins an older exact version.

### (d) Gallery completeness

Ensure `formatting-gallery.md` has **every** fence filled with current
formatter output (run the formatter; don't hand-write). In particular, confirm
the post-S3d entries are correct: **§36** (match-clause guard on one line),
**§38** (export keyword-alone), **§39** (import keyword-alone). Add a one-line
provenance note to the gallery header: the CL-shared subset was cross-checked
against yasi (Lisp dialect) and is whitespace-identical.

### (e) Final verification + release checklist

Run and record: full `rebar3 ct`, `rebar3 eunit` (if used), `rebar3 xref`
(clean), `rebar3 dialyzer` (at standing provider-suite level — note, don't
"fix"). Verify the README/`docs/commands.md` examples match the **actual** CLI
(`rebar3 lfe format --help` and a real run on `_integration/myproj`). Then write a
short **release-readiness checklist** (in the CDC report and/or a
`docs/design/022-lfe-format/RELEASE-0.5.5.md`) listing the manual steps Duncan
will run (commit, the `make publish` / per-registry steps from CLAUDE.md, tag),
explicitly noting that **CC did not perform any of them**.

## 2. Constraints

Docs + tests only (plus any tiny doc-driven fixes). No engine/provider behavior
changes (if you find a behavior bug, that's A6·S1 territory — report it). No
publishing actions. `warnings_as_errors` stays green.

## 3. Ledger (closes Arc A6 and the 022 feature)

```
Arc A6·S2 — docs + release prep
[ ] docs/commands.md: full `format` entry matching existing command style
[ ] CHANGELOG 0.5.5 entry (created if no changelog existed)
[ ] vsn confirmed 0.5.5; no stale exact-version pins
[ ] gallery complete; §36/§38/§39 reflect S3d; CL provenance note added
[ ] final verification: ct/eunit/xref green; dialyzer at standing level;
    README + commands.md match actual `--help`/run output
[ ] RELEASE-0.5.5.md (or CDC section): manual release checklist; CC published nothing
[ ] files changed + one-line rationale; deviations named — or "none"
```

When green, **Arc A6 and the entire 022 LFE-formatter feature are complete** —
0.5.5 is ready for Duncan to cut.
