# CC prompt — rebar3_lfe · 023-lfmt-integration

You are CC. Make `rebar3_lfe` consume the **`lfmt`** formatter library instead of
its own engine copy: add the dep, rewire the `format` provider to `lfmt:format/1`,
and delete the local engine + its tests. The provider's behaviour is
**preserved** — output must be unchanged. You run `git` + the toolchain directly.

Target OTP 28 (rebar3_lfe runs OTP 24+). Load **collaboration-framework**
(ledger discipline) and **erlang-guidelines**.

## Read first

- This slice's `slice-doc.md` (full scope + delete list) and `ledger.md`.
- `src/r3lfe_prv_format.erl` (the provider) — the call site is `read_and_format/1`.
- rebar3_lfe's `CLAUDE.md` (safety-gate rules — must stay untouched).

## Build prerequisite — do this first

`_checkouts/lfmt` symlinks to the `lfe/fmt` repo, and `lfmt:format` lives on
**`feature/v0.4.0-release`** (`a8edcf8`), not `main`. Ensure the fmt tree is on
that branch before building:

```sh
git -C <path-to>/lfe/fmt rev-parse --abbrev-ref HEAD   # expect feature/v0.4.0-release
# if not: git -C <path-to>/lfe/fmt checkout feature/v0.4.0-release
```

Confirm `lfmt:format/1` is visible: `_checkouts/lfmt/src/lfmt.erl` exports
`new/1, format/1, format/2`. (Confirm with Duncan if the branch checkout is his
to make.)

## Step 0 — capture the parity baseline FIRST

Before changing anything: format a sample corpus through the **current** provider
(or `r3lfe_formatter:format/1` directly) and save the output as a golden. This is
your LI-6 parity proof — capture it before the swap.

## Step 1 — dep

`rebar.config`: add `{lfmt, "~> 0.4"}` to `deps` (keep `{lfe, "2.2.0"}`).

## Step 2 — rewire the provider (one line)

`src/r3lfe_prv_format.erl` `read_and_format/1`: change
`r3lfe_formatter:format(Original)` → `lfmt:format(Original)`. The contract is
identical (`{ok, iolist()} | {error, term()}`); keep the existing
`unicode:characters_to_binary(IO)` flatten. Verify the `format_file/1`
error-tuple `-spec` still holds (lfmt surfaces the same engine errors); relax it
with rationale only if needed.

## Step 3 — delete the local engine + its tests

`git rm`: `src/r3lfe_format_{cst,lexer,formatter}.erl`,
`test/r3lfe_format_{cst,lexer,formatter}_SUITE.erl`,
`test/r3lfe_format_lexer_SUITE_data/`.

**`test/r3lfe_properties.erl`** also calls `r3lfe_formatter:format` +
`r3lfe_format_lexer`/`_cst` internals. Inspect it: if it is wholly
engine-property tests, `git rm` it (those properties now live in lfmt's suite);
if it mixes in non-formatter properties, excise only the formatter parts. Do
**not** leave dangling references.

**Keep:** `src/r3lfe_prv_format.erl`, `test/r3lfe_prv_format_SUITE.erl`,
`test/e2e/format_e2e.sh`.

## Engineering bar

- `grep -rnE 'r3lfe_format_(cst|lexer)|r3lfe_formatter' src test` → **empty**
  (note `r3lfe_prv_format` is a different prefix — it stays).
- `rebar3 compile` zero-warning; `rebar3 xref` clean; `rebar3 dialyzer` clean;
  `rebar3 ct` green (provider suite + `test/e2e/format_e2e.sh`). (CI reconciles.)
- **Output parity**: provider output over the corpus matches the Step-0 golden
  (or disclose any diff if the local engine had drifted past fmt's `A7·S6`
  import tip).
- `rebar3_lfe`'s `CLAUDE.md` safety gates untouched.

## Working ledger + close

Update `ledger.md` per-row (toolchain rows note "CI reconciles"). At close write
`closing-report.md`: the rewire + delete summary, the parity evidence, the
`r3lfe_properties` disposition, and the bubble-up (this satisfies fmt v0.4.0
A1-4). Don't mark your own rows CDC-verified. Bump + release `rebar3_lfe` is the
operator's call after CDC.
