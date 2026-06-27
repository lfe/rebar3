# 023 · lfmt-integration — slice-doc

> A **collapsed single slice** (per the framework's single-slice rule — this is
> one mergeable diff, so no project-plan/arc-plan wrapper). It lives in
> `rebar3_lfe`'s established `docs/design/NNN-slug/` convention. The five
> per-slice docs (open set: this `slice-doc.md`, `ledger.md`, `cc-prompt.md`;
> close set later: `closing-report.md`, `cdc-verification.md`) live in this dir.
>
> Cross-repo note: this slice **is** the consumer side of `lfe/fmt`'s
> v0.4.0 `arc1-release` slice 4 (rebar3-integration). When that arc closes, its
> A1-4 row points here.

## Purpose

Make `rebar3_lfe` consume the **`lfmt`** formatter library instead of carrying
its own copy of the engine. The `format` provider's behaviour is **preserved**;
the engine simply moves out of this repo and in behind the `lfmt:format/1` API.

## Background / context

`rebar3_lfe`'s formatter engine (`r3lfe_format_lexer` → `r3lfe_format_cst` →
`r3lfe_formatter`) was migrated into `lfe/fmt` (the `lfmt` package) over v0.1.0–
v0.4.0, where it became `lfmt_fezzik*` behind the multi-engine `lfmt:new/1` /
`lfmt:format` API. `rebar3_lfe` now drops its local copy and calls `lfmt`.

The engine is **byte-identical** to what `rebar3_lfe` shipped (the fmt split was
proven byte-identical; the rename was names-only), and `lfmt:format(new(#{engine
=> fezzik}), S) === lfmt_fezzik:format(S)` was proven in fmt slice 2 — so the
provider's formatted output should be **unchanged**. (Caveat: confirm
`rebar3_lfe`'s `r3lfe_formatter` hasn't drifted past the fmt import tip
`A7·S6`; if it has, lfmt's output is the new source of truth and any diff is
disclosed — see LI-5.)

## Build prerequisite (operational)

`_checkouts/lfmt` symlinks to the `lfe/fmt` repo. The `lfmt:format` API lives on
**`feature/v0.4.0-release`** (`a8edcf8`) — **not** `main`. So before building
`rebar3_lfe`, the fmt working tree must be on that branch:
`git -C <…>/fmt checkout feature/v0.4.0-release`. (Operator step; once `lfmt
0.4.0` is on hex, the `{lfmt,"~>0.4"}` dep resolves without `_checkouts`.)

## Scope

In scope:

- **`rebar.config`**: add `{lfmt, "~> 0.4"}` to `deps` (keep `{lfe, "2.2.0"}`).
  `_checkouts/lfmt` overrides it locally during dev.
- **Rewire the provider**: `src/r3lfe_prv_format.erl` `read_and_format/1`
  (line ~213) — `r3lfe_formatter:format(Original)` → `lfmt:format(Original)`.
  Identical contract (`{ok, iolist()} | {error, term()}`); keep the existing
  `unicode:characters_to_binary/1` flatten.
- **Delete the local engine + its tests** (full list below).
- **Resolve `test/r3lfe_properties.erl`** — it calls `r3lfe_formatter:format`
  *and* `r3lfe_format_lexer`/`_cst` internals directly. Those engine-property
  tests now live in lfmt's own suite. If the file is wholly engine-properties,
  delete it; if it mixes in non-formatter properties, excise only the
  formatter parts. (This is the consumer a naive delete would have orphaned.)

Out of scope: any change to the formatter's behaviour/output; exposing an
engine-selection option in the provider (it stays Fezzik-as-default, via
`lfmt:format/1`); touching `rebar3_lfe`'s other providers; the lfmt hex publish
(that's fmt slice 3).

## Delete list

- `src/r3lfe_format_cst.erl`, `src/r3lfe_format_lexer.erl`,
  `src/r3lfe_formatter.erl`
- `test/r3lfe_format_cst_SUITE.erl`, `test/r3lfe_format_lexer_SUITE.erl`,
  `test/r3lfe_formatter_SUITE.erl`
- `test/r3lfe_format_lexer_SUITE_data/`
- `test/r3lfe_properties.erl` (or excise its formatter parts — see Scope)

**Keep:** `src/r3lfe_prv_format.erl` (rewired), `test/r3lfe_prv_format_SUITE.erl`,
`test/e2e/format_e2e.sh` — these now exercise `lfmt` end-to-end (the dependency
edge).

## Success criteria (gate)

- `rebar.config` has `{lfmt, "~> 0.4"}`; `rebar3 compile` resolves it (via
  `_checkouts/lfmt` on the v0.4.0 branch) and is zero-warning.
- The provider calls `lfmt:format/1`; **no** references to `r3lfe_format_lexer`,
  `r3lfe_format_cst`, or `r3lfe_formatter` remain in `src/`+`test/`
  (`grep -rnE 'r3lfe_format_(cst|lexer)|r3lfe_formatter' src test` → empty).
- The delete list is gone; `r3lfe_prv_format`, its SUITE, and `test/e2e/` remain.
- `rebar3 ct` green — the provider suite + `test/e2e/format_e2e.sh` pass against
  lfmt (proves the dependency edge); `xref` clean (no dangling engine refs);
  `dialyzer` clean (the `format_file/1` error-tuple `-spec` still holds, since
  lfmt surfaces the same engine errors — or is relaxed with rationale).
- **Output parity**: formatting a sample corpus through the provider yields the
  same output as before the swap (or any diff is disclosed per the drift caveat).
- `rebar3_lfe`'s `CLAUDE.md` safety gates are **untouched** — this is a dependency
  swap, not a wrapper-flag change.

## Handoff

CC provides: the `rebar.config` dep; the one-line provider rewire; the deletions
(incl. the `r3lfe_properties` resolution); green `compile`/`ct`/`xref`/`dialyzer`;
the empty engine-reference grep; the output-parity evidence; a per-row ledger
walk + `closing-report.md`.
