# 022 · Arc A6 · S1 — hardening: property, fuzz, edge cases (CC spec)

> Target: Sonnet 4.6 + `erlang-guidelines`. First A6 sub-slice. **Implementation
> spec** — implement exactly. **Stop and report** if blocked.
>
> **Output discipline:** write to files with Write/Edit; don't reprint files;
> terse prose; run tests; report briefly.

## 0. Orientation

1. `CLAUDE.md` (`warnings_as_errors` ON); `erlang-guidelines` (`11-anti-patterns`,
   `15-testing`).
2. `arc6-release/cc-prompt.md` — shared ref (standing facts, safety).
3. `src/r3lfe_formatter.erl` (`format/1`), `src/r3lfe_format_lexer.erl`
   (`tokens/1`), `src/r3lfe_format_cst.erl` (`significant_tokens/1`, `comments/1`).
4. `test/r3lfe_properties.erl` (existing PropEr module — add to it) and the
   `r3lfe_formatter_SUITE` oracle helpers (reuse them).

## 1. The oracles (reuse the established four)

For input `X` with `{ok, IO} = format(X)`, `Y = iolist_to_binary(IO)`:
- **idempotency:** `format(Y) == {ok, Y}` (fixed point);
- **token-preservation:** significant tokens of `Y` == those of `X`, in order;
- **comment-preservation:** ordered comment texts of `Y` == those of `X`;
- **AST-equivalence:** `lfe`-read of `Y` ≡ `lfe`-read of `X` (exclude inputs
  containing `#.(` read-eval).

## 2. Scope of THIS slice

### (a) Property-based testing (PropEr, in `r3lfe_properties.erl`)

1. **Generator property:** write a recursive PropEr generator that emits **valid
   LFE source text** from a restricted-but-representative grammar — symbols,
   numbers, strings, nested lists/tuples/maps, and a handful of special forms
   (`defun`, `let`, `case`, `cond`, `if`, `lambda`, quotes). Build the text so it
   always parses (you control the grammar). Property: for every generated `X`, all
   four oracles hold. This exercises **novel structures**, not just the corpus.
2. **Corpus sweep (committed, not one-off):** a property or parameterized test
   that runs the four oracles over every `.lfe` file discoverable under the repo
   (`_integration/**`, `_build/**` vendored LFE, test data), skipping files that
   don't read as valid UTF-8/LFE (record the skip count). State how many files
   were exercised.

### (b) Edge-case battery (CT, in `r3lfe_formatter_SUITE`, group `edge_hardening`)

Each must format without crashing, be idempotent, and (where applicable)
token/comment-preserve:
- empty input → `""`; whitespace-only input; comment-only input (line **and**
  block);
- file with **no trailing newline**; input with **CRLF** → LF output;
- **unicode** in symbols and strings (e.g. `é`, `日本語`, an emoji in a string);
- a **single token longer than 80 cols** (a very long atom and a very long
  string) — must not loop; emitted on its line as-is;
- **deep nesting** (e.g. 500+ levels) → no stack overflow, completes;
- a `#.(…)` read-eval form → does not crash (excluded from AST-equiv only);
- a **large** real file → formats in reasonable time (sanity, not a benchmark).

### (c) Robustness / fuzz (CT, group `fuzz`)

`format/1` must **never crash** — it returns `{ok, _}` or `{error, _}` for *any*
binary:
- **truncated** valid files (cut a known-good file at several offsets) → `{ok,_}`
  or `{error,_}`, never an exception;
- **random byte** sequences → `{error, _}` (or ok), never a crash;
- **unbalanced** parens / unterminated string / unterminated block comment →
  `{error, _}` with a sensible reason.
  Implement as a loop over many random/derived inputs asserting the call returns a
  tagged tuple (wrap in try/catch in the test to assert no exception escapes).

### (d) Fix what you find

If any property/edge/fuzz case reveals a real bug, **fix it minimally** in the
engine, keep all prior oracles green, and add the failing case as a permanent
regression (corpus fixture or explicit test). If a fix is non-trivial or changes
formatting behavior, **stop and report** for adjudication rather than deciding
unilaterally.

## 3. Constraints

`-spec` any new exported funcs; `xref` clean; `dialyzer` at standing level (no new
*real* categories); `warnings_as_errors`. Engine stays pure. No new runtime deps
(PropEr is test-only, already present).

## 4. Ledger

```
Arc A6·S1 — hardening
[ ] PropEr generator property (restricted LFE grammar): 4 oracles hold, green
[ ] committed corpus-sweep test: 4 oracles over all discoverable .lfe (state count
    + skip count)
[ ] edge_hardening group: empty/ws-only/comment-only/no-trailing-nl/CRLF/unicode/
    over-long-token/deep-nesting/read-eval/large-file — all green
[ ] fuzz group: truncated + random-bytes + unbalanced never crash; return tagged
    tuples
[ ] any bug found → fixed minimally + permanent regression added (or reported if
    behavior-changing)
[ ] full suite green (state count); xref clean; dialyzer at standing level
[ ] files changed + one-line rationale; deviations named — or "none"
```

Stop here. Do not start S2 (docs + release).
