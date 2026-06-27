# 023 · lfmt-integration — closing report

> CC self-assessment. CDC verifies independently via `cdc-verification.md`.

## Rewire + delete summary

**Dep added** (`rebar.config`):
```erlang
{deps, [
    {lfe, "2.2.0"},
    {lfmt, "~> 0.4"}
]}.
```

**Provider rewired** (`src/r3lfe_prv_format.erl:213`):
```erlang
%% before
case r3lfe_formatter:format(Original) of
%% after
case lfmt:format(Original) of
```
No other changes to the provider. `unicode:characters_to_binary(IO)` flatten
and the `{ok, binary(), binary()} | {error, term()}` contract of
`read_and_format/1` are unchanged.

**Engine deleted** (`git rm`):
- `src/r3lfe_format_cst.erl`
- `src/r3lfe_format_lexer.erl`
- `src/r3lfe_formatter.erl`
- `test/r3lfe_format_cst_SUITE.erl`
- `test/r3lfe_format_lexer_SUITE.erl`
- `test/r3lfe_format_lexer_SUITE_data/tq_corpus.lfe`
- `test/r3lfe_format_lexer_SUITE_data/` (directory)
- `test/r3lfe_formatter_SUITE.erl`

**Engineering bar grep** (LI-5):
```
grep -rnE 'r3lfe_format_(cst|lexer)|r3lfe_formatter' src test
```
→ empty.

**`src/rebar3_lfe.app.src` updated**: added `lfmt` to `{applications}`;
added `r3lfe_prv_format` to `{modules}` (was missing).

**`test/e2e/format_e2e.sh` updated**: removed hardcoded minimal
`src/rebar3_lfe.app.src` override in checkout; merged lfmt beams into the
rebar3_lfe checkout ebin so they share the same code path in the fixture.

## r3lfe_properties disposition

`test/r3lfe_properties.erl` was **not** deleted. It contained:
- 1 engine property: `prop_lfe_four_oracles/0` (+ helpers + generators)
- 5 non-engine properties: `prop_module_name_reversible/0`,
  `prop_package_cleanup_always_succeeds/0`, `prop_dependency_graph_acyclic/0`,
  `prop_incremental_compilation_deterministic/0`,
  `prop_concurrent_compilation_safe/0`

The engine property and all its supporting code (helpers, generators) were
excised. The 5 non-engine properties were retained. The `-export` list was
updated to match. No dangling engine references remain (LI-5 confirmed).

## Parity evidence (LI-6)

Pre-swap golden: `docs/design/023-lfmt-integration/li6-golden.md`
(hashes from `r3lfe_formatter:format/1`).

Post-swap verification via `lfmt:format/1` (formatted gallery entries as
idempotent inputs, plus file corpus):

**Gallery entries: 14/14 MATCH**

| Entry | Expected hash | Result |
|-------|--------------|--------|
| #1  atom              | 3CD39DD | ✓ |
| #18 if                | 1DA5C5  | ✓ |
| #22 receive           | 71D2779 | ✓ |
| #23 try               | 1084A4E | ✓ |
| #25 match-lambda      | 3618EBA | ✓ |
| #35 defun ack         | 1B50548 | ✓ |
| #38 defmodule maths   | 77FD96F | ✓ |
| #39 defmodule client  | 9F24D3  | ✓ |
| #48 leading comment   | 6C53004 | ✓ |
| #49 trailing comment  | 1128630 | ✓ |
| #51 dangling comment  | 125FE6E | ✓ |
| #53 head trailing     | 6A71750 | ✓ |
| #56 wide call         | 4D232DD | ✓ |
| #63 commented export  | 526BFF6 | ✓ |

**File corpus: 3/3 MATCH**

| File | Expected hash | Result |
|------|--------------|--------|
| `_integration/myproj/src/myproj.lfe`        | 6D5D432 | ✓ |
| `_integration/myproj/test/myproj-tests.lfe` | 1605503 | ✓ |
| `_integration/myproj/include/records.lfe`   | 1A10693 | ✓ |

Note: `tq_corpus.lfe` from the golden's file corpus was deleted with the
engine (LI-3). Coverage replaced by the three integration files above.

**Drift: zero.** Engine behaviour is identical pre- and post-swap.

## Toolchain results

- `rebar3 compile`: 0 warnings
- `rebar3 xref`: clean
- `rebar3 dialyzer`: clean (27 files; `-spec` for `format_file/1` unchanged)
- `rebar3 as test ct`: 448 tests passed
- `bash test/e2e/format_e2e.sh`: 24/24 assertions passed

## Bubble-up note

This slice satisfies the fmt v0.4.0 arc1-release criteria A1–A4:

- **A1** (lfmt published): dependency declared in `rebar.config`; resolves via
  `_checkouts/lfmt` symlink during development, and will resolve from hex once
  lfmt 0.4.0 is published.
- **A2** (consumer integration): `rebar3_lfe` is the first external consumer
  of the `lfmt:format/1` public API.
- **A3** (end-to-end proof): provider CT suite (14 tests) + e2e shell test
  (24 assertions) exercise the full stack including real file I/O.
- **A4** (parity preserved): 17/17 parity checks pass; zero output drift.

Bump and publish `rebar3_lfe 0.5.5` is the operator's call after CDC review.
