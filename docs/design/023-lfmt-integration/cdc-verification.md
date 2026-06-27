# 023 · lfmt-integration — CDC verification

Verifier: Claude (Cowork chat seat, acting as CDC — independent of CC).
Date: 2026-06-27.
Repo: `rebar3_lfe` (branch `release/0.5.x`); integration commit **`a9ddb6b`**.
(`_checkouts/lfmt` → fmt repo; Duncan merged the v0.4.0 API to fmt `main`.)

## Verification boundary

Reproduced (git/source) the rewire, deletions, excision, dep, scope, and the
e2e-bridge mechanism; the toolchain + parity run (ct 448, e2e 24/24, golden
match) are **attested by CC** (OTP 28) → reconcile via **CI**.

## Per-row verdict

| ID | CC status | CDC verdict | Basis |
|----|-----------|-------------|-------|
| LI-1 | done | **reproduced** | `rebar.config:11` `{lfmt, "~> 0.4"}`; `{lfe,…}` retained. |
| LI-2 | done | **reproduced** | `r3lfe_prv_format.erl:213` `case lfmt:format(Original) of`; `unicode:characters_to_binary(IO)` retained. One-line swap, identical contract. |
| LI-3 | done | **reproduced** | `a9ddb6b` deletes `r3lfe_format_{cst,lexer,formatter}.erl` (366+457+1869) + 3 `*_SUITE.erl` + `tq_corpus.lfe` — 6778 deletions. |
| LI-4 | done | **reproduced** | `test/r3lfe_properties.erl` retained, −181 lines; engine prop + helpers + generators excised; **5 non-engine props remain** (module-name, package-cleanup, dep-graph, incremental-compile, concurrent-compile). |
| LI-5 | done | **reproduced** | `git grep -E 'r3lfe_format_(cst\|lexer)\|r3lfe_formatter' a9ddb6b -- src test` → **empty**. No dangling engine refs. |
| LI-6 | done | **attested** (golden) | `li6-golden.md` present; CC reports 14/14 gallery + 3/3 corpus match, **zero drift**. Consistent with the engine being the byte-identical migrated `r3lfe_formatter` (= `lfmt_fezzik`). Not re-run (no OTP). |
| LI-7 | done | **attested + see F2** | ct 448, e2e 24/24 — CC-run. The e2e *bridge* is sound (see F2) but doesn't exercise real dep-resolution. |
| LI-8 | done | **attested** (CI) | compile zero-warning, xref clean, dialyzer clean (27 files), `format_file/1` `-spec` unchanged — CC-run. |
| LI-9 | done | **reproduced** | `git show --stat a9ddb6b -- CLAUDE.md` → empty. Safety gates untouched. |
| LI-10 | done | **reproduced** | `closing-report.md` present (rewire + parity table + `r3lfe_properties` disposition + A1-4 bubble-up). |

**Tally:** 10 rows. 6 reproduced, 1 attested-golden (LI-6), 3 attested-CI
(LI-7/-8, with LI-7's F2). 0 rejected. The `app.src` change (lfmt → `{applications}`,
`r3lfe_prv_format` → `{modules}`) is correct and reproduced.

## Findings

- **F1 — RELEASE BLOCKER (not an integration defect): rebar3_lfe 0.5.5 cannot
  publish to hex until `lfmt 0.4.0` is on hex.** `rebar.config` now depends on
  `{lfmt, "~> 0.4"}`, but lfmt 0.4.0 is **not yet published** (fmt slice 3
  hex-release is still open). `_checkouts/lfmt` + the e2e beam-bridge make
  rebar3_lfe **build and test** locally, but `rebar3 hex publish` of rebar3_lfe
  will reject/break on an unpublished dep. **Publish order is fixed: lfmt 0.4.0 →
  hex first, then rebar3_lfe 0.5.5.** The A6 readiness report's "ready to
  publish" is conditional on this.
- **F2 — the e2e validates formatting behaviour, not real dep-resolution
  (disclosed gap, sound bridge).** The fixture copies `_build/.../checkouts/lfmt/
  ebin/*.beam` into the plugin checkout's ebin so `lfmt:format/1` is on the path
  (rebar3 doesn't resolve a checkout's nested deps). This correctly tests that
  the provider *calls lfmt and produces correct output*, but does **not** exercise
  the production path (user adds `rebar3_lfe` plugin → rebar3 fetches `lfmt` from
  hex → on path). That path is inherently untestable pre-hex. **Recommended:
  after lfmt 0.4.0 publishes, run a real-project smoke test** (rebar3_lfe 0.5.5
  as a plugin from hex, no `_checkouts`) to prove dep-resolution. CC disclosed
  this (Amendment 1 / Caveat) — good honesty; flagging it as the one piece the
  e2e can't yet cover.
- **F3 — parity is exact + reproducible-in-principle (endorse).** The golden was
  captured pre-swap and matched post-swap; zero drift is expected because lfmt's
  engine *is* the migrated `r3lfe_formatter`. The `tq_corpus.lfe` fixture went
  with the engine; coverage replaced by 3 `_integration/myproj/` files — fine.

## Closure

**CDC accepts the 023-lfmt-integration core** — rewire, deletions, excision,
no-dangling-refs, app.src, and safety-gate-untouched are independently
reproduced; parity + toolchain are credibly attested (engine is byte-identical).
The integration itself is correct and complete.

**Two conditions ride to the release, not the integration:** F1 (publish lfmt
0.4.0 first — hard ordering) and F2 (post-publish real-project smoke test for
dep-resolution). Neither blocks accepting the integration; both gate the
**rebar3_lfe 0.5.5 hex publish**.

**Satisfies fmt v0.4.0 `arc1-release` A1-4** (the cross-repo consumer edge):
lfmt is integrated, end-to-end tested locally, output-verified against the
pre-swap golden. When fmt's arc closes, A1-4 points here.

Reviewed by: CDC (Cowork chat seat).
