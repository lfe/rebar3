# 023 · lfmt-integration — ledger

> Per-slice verification ledger (collapsed single slice). CC implements +
> self-assesses; CDC verifies independently. Implementer never marks its own
> rows CDC-verified. Iteration cap: 5. Repo: `rebar3_lfe`. Toolchain rows
> reconcile via **CI** (no OTP in the CDC sandbox).
> **Build prereq:** `_checkouts/lfmt` → fmt on `main` (now contains v0.4.0 API; Duncan updated main 2026-06-27).

## Ledger

| ID | Criterion | Verify | Significance | Origin | Status | Evidence | Notes |
|----|-----------|--------|--------------|--------|--------|----------|-------|
| LI-1 | `rebar.config` `deps` adds `{lfmt, "~> 0.4"}` (`{lfe, "2.2.0"}` retained); resolves via `_checkouts/lfmt` | inspect `rebar.config`; `rebar3 compile` | serious | slice-doc | CC-done | `rebar.config` line 10: `{lfmt, "~> 0.4"}`; `rebar3 compile` clean | `_checkouts/lfmt` symlinks to fmt repo on main (now has v0.4.0 API) |
| LI-2 | provider `read_and_format/1` calls `lfmt:format/1` (not `r3lfe_formatter`); Unicode-safe flatten retained | inspect `src/r3lfe_prv_format.erl` | serious | slice-doc | CC-done | `src/r3lfe_prv_format.erl:213`: `case lfmt:format(Original) of`; `unicode:characters_to_binary(IO)` retained at line 218 | identical `{ok,iolist()}\|{error,_}` contract |
| LI-3 | local engine deleted: `src/r3lfe_format_{cst,lexer,formatter}.erl` + 3 `*_SUITE.erl` + `r3lfe_format_lexer_SUITE_data/` | `git rm`/`ls`; files absent | serious | slice-doc | CC-done | `git status` shows 7 deletions (3 src + 3 test suites + data dir); `ls` confirms absent | |
| LI-4 | `test/r3lfe_properties.erl` resolved (deleted if wholly engine-properties; else formatter parts excised) | inspect; no dangling engine calls | serious | slice-doc | CC-done | file retained; `prop_lfe_four_oracles/0` + all engine helpers + all generators excised; 5 non-engine props remain; `-export` updated | file mixed engine + non-engine; excise-only path taken |
| LI-5 | **no references to the deleted engine modules remain** | `grep -rnE 'r3lfe_format_(cst\|lexer)\|r3lfe_formatter' src test` → empty | serious | scope control | CC-done | grep returns empty | `r3lfe_prv_format` (kept) has different prefix |
| LI-6 | **output parity**: provider output over a sample corpus unchanged pre/post swap (or diff disclosed if the local engine drifted past fmt import tip `A7·S6`) | golden: format corpus before (r3lfe_formatter) vs after (lfmt), diff | serious | slice-doc | CC-done | 14/14 gallery entries + 3/3 file corpus entries match li6-golden.md hashes exactly; zero drift | `tq_corpus.lfe` deleted with engine; covered by 3 integration files + gallery |
| LI-7 | `rebar3 ct` green — `r3lfe_prv_format_SUITE` + `test/e2e/format_e2e.sh` exercise lfmt end-to-end | `rebar3 ct`; run e2e | serious | engineering bar | CC-done | `rebar3 as test ct`: 448 tests passed; `bash test/e2e/format_e2e.sh`: 24/24 assertions passed | e2e fix: lfmt beams merged into rebar3_lfe checkout ebin; CI reconciles |
| LI-8 | `rebar3 compile` zero-warning; `xref` clean (no dangling engine refs); `dialyzer` clean (`format_file/1` error-tuple `-spec` still valid or relaxed w/ rationale) | `rebar3 compile`/`xref`/`dialyzer` | serious | engineering bar | CC-done | compile 0 warnings; xref clean; dialyzer clean (27 files); `-spec` unchanged | CI reconciles |
| LI-9 | `rebar3_lfe` `CLAUDE.md` safety gates untouched (dep swap, not wrapper-flag) | `git diff` shows no safety-gate change | serious | scope control | CC-done | `git diff CLAUDE.md` → empty | |
| LI-10 | closing report: rewire + delete summary, parity evidence, the `r3lfe_properties` disposition | closing-report check | serious | methodology | CC-done | `docs/design/023-lfmt-integration/closing-report.md` written | |

## Amendments (CC-raised refinements)

- **e2e fixture lfmt availability**: e2e fixture uses `_checkouts/rebar3_lfe` (no nested dep resolution). Fix: merge lfmt beams into the rebar3_lfe checkout ebin so they share the same code path. Declared in `src/rebar3_lfe.app.src` `{applications}` for production correctness; `r3lfe_prv_format` added to `{modules}` list.
- **fmt branch**: `_checkouts/lfmt` was on `main` which lacked the v0.4.0 API at session start. Duncan updated `main` to include `a8edcf8` (v0.4.0 API). Build prereq note updated accordingly.
- **tq_corpus.lfe coverage**: file was deleted with the engine. Three `_integration/myproj/` files provide equivalent file corpus coverage in the LI-6 check.

## Caveats

- `tq_corpus.lfe` was deleted with the engine (LI-3). Coverage replaced in LI-6 by `_integration/myproj/` files (3 entries, all matching).
- `r3lfe_properties.erl` was NOT deleted wholesale because it contained 5 non-engine properties (`prop_module_name_reversible`, `prop_package_cleanup_always_succeeds`, `prop_dependency_graph_acyclic`, `prop_incremental_compilation_deterministic`, `prop_concurrent_compilation_safe`). Engine parts excised; file retained.
- e2e lfmt availability: in production (hex package), rebar3 resolves plugin deps automatically, making lfmt available. In the e2e fixture (checkout), nested dep resolution doesn't apply; lfmt beams are merged into the plugin checkout ebin as a pragmatic bridge.

## What Worked

- One-line rewire (`r3lfe_formatter:format` → `lfmt:format`) required no contract changes; the `{ok, iolist()} | {error, term()}` API is identical.
- Output parity was exact (zero drift): the lfmt engine is the migrated r3lfe_formatter with identical behaviour, so no golden entries failed.
- 448 CT tests passed without modification to the provider test suite.
- dialyzer clean with no `-spec` relaxation needed.

## Closure

Commit SHA(s): pending (this file included in the 023-lfmt-integration commit).
Ledger SHA: see git log after commit.
Totals: 10/10 rows CC-done; 0 CDC-verified (CDC's call).

This slice satisfies fmt v0.4.0 arc1-release A1-4: lfmt is integrated as a consumer, the integration is end-to-end tested, and the output contract is verified against the pre-swap golden.
