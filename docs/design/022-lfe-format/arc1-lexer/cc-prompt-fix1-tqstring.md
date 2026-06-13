# 022 · Arc A1 · Fix iteration — triple-quoted string delimiting

> For Sonnet 4.6 + `erlang-guidelines`. This is a **scoped follow-up** to Arc A1.
> A1 was accepted pending this one fix. Everything else in
> `src/r3lfe_format_lexer.erl` stands — do **not** refactor unrelated code.
>
> Context for you (no blame): your A1 deviation note was correct. The triple-quote
> rule you implemented ("close on first `"""`") matched a **bad test in the spec**.
> The spec author has since corrected the spec. Your job now is to bring the
> tokenizer in line with LFE's real reader and fix the test that sanctioned the
> old behavior.

## 0. Re-read first (in order)

1. `docs/design/022-arc1-lexer.md` — **§5.4 (Strings)** and **§7 group 2**, both
   now corrected. These are the authority for this fix.
2. `lfe/src/lfe_scan.erl` — the real reader's triple-quote scanner:
   `scan_tq_string_1`, `scan_tq_string_lines`, `scan_tq_string_tq`,
   `scan_tq_string_end`, and `blank_line/1` (≈ lines 717–823). Read these — they
   are the behavior you must match. (Reference only; do not fork.)
3. Your own `src/r3lfe_format_lexer.erl` — the functions `scan_tq_string/5` and
   its two call sites (the `tqstring` clause ≈ L163 and the `tqbstring` clause
   ≈ L113), plus `test/r3lfe_format_lexer_SUITE.erl`.

## 1. The defect

`scan_tq_string` currently closes on the **first** `"""` it sees. LFE does not
work that way:

- The **opening** `"""` (or `#"""`) must be alone on its line — only spaces may
  follow it before the newline. `"""abc…` is **invalid LFE** (`bad_tq_string`).
- A `"""` appearing on a line that has non-space content before it is **string
  content, not a closer** — the LFE guide's own example embeds `"""` in a content
  line.
- The **closing** `"""` is recognized only when the current line up to it is all
  spaces (`lfe_scan`'s `blank_line/1`).

Because the `_integration` corpus contains no triple-quoted strings, the A1
round-trip never exercised this — losslessness still held (concatenation is
verbatim regardless), but the **token boundary** is wrong, which would corrupt
the CST in Arc A2.

## 2. Required behavior

The token text stays **fully verbatim** (opening delimiter through closing
delimiter, including the opening line's newline and every content line). Only the
*boundary detection* changes. Implement, for both `tqstring` (`"""`) and
`tqbstring` (`#"""`):

1. **Opening line.** After the opening delimiter, consume only spaces up to the
   `\n`. If any non-space, non-newline character appears before the `\n`, return
   `{error, {bad_tq_string, StartLine}}`. Consume the `\n` into the token text.
2. **Content lines.** Accumulate verbatim. Track the characters seen since the
   last `\n` (the "current line so far").
3. **Closer.** When you encounter `"""`:
   - if the current line so far is all spaces → this is the closer: consume the
     `"""` and finish the token (text spans through this `"""`);
   - otherwise → treat the `"""` as content and keep scanning.
4. **EOF** before a valid closer ⇒ `{error, {unterminated_string, StartLine}}`.
5. 4+ consecutive quotes (`""""…`): match whatever `lfe_scan` does; if that's
   ambiguous to you, pick the `lfe_scan`-consistent reading, and **document +
   test** the choice rather than guessing silently.

Keep the existing column/line tracking conventions you used elsewhere (positions
are advisory; correctness of the *text* and *boundary* is what matters).

## 3. Tests (`test/r3lfe_format_lexer_SUITE.erl`)

- **Remove** the invalid `tqstring_inner_quotes` case
  (`"""a "b" #| c"""` — not valid LFE).
- **Add** a real multi-line triple-quoted string: opening `"""` alone on its
  line, a content line that itself contains `"""` **and** `#|`, then a closing
  `"""` alone on a whitespace-only line. Assert: exactly **one** `tqstring`
  token, its `text` equals the verbatim source span, and it round-trips (§6).
- **Add** the same shape for `#"""` ⇒ one `tqbstring`.
- **Add** error cases: content after the opening delimiter ⇒
  `{error, {bad_tq_string, _}}`; EOF before close ⇒
  `{error, {unterminated_string, _}}`.
- **Add a corpus fixture** (a small `.lfe` test data file, or an inline binary in
  the suite) that contains **both** a block comment (`#| … |#`) and a multi-line
  triple-quoted string, and round-trip it. The `_integration` files exercise
  neither path end-to-end today.

## 4. Constraints (unchanged from A1)

- Pure module; no new deps; no runtime use of `lfe_scan`.
- `-spec` intact; compile clean under `warnings_as_errors`; `xref` + `dialyzer`
  clean.
- Do not touch unrelated scanners (strings, comments, symbols, numbers, chars).
- snake_case, `%%` function comments, tagged `{ok,_}`/`{error,_}` returns.

## 5. Report against this ledger

```
Arc A1 fix — triple-quoted strings
[ ] scan_tq_string now matches lfe_scan: opening-line check, content """ kept,
    blank-line closer, EOF + bad-open errors
[ ] tqstring + tqbstring multi-line tests added and passing (paste assertions)
[ ] bad_tq_string and unterminated error tests added and passing
[ ] new fixture with block comment + triple-quote round-trips green
[ ] invalid tqstring_inner_quotes test removed
[ ] full suite green (paste `rebar3 ct` summary); count unchanged-or-higher
[ ] xref clean; dialyzer clean
[ ] no unrelated code changed (confirm; list the diff)
[ ] 4+-quote behavior: state what you did and why
```

Stop after this fix. Do not start Arc A2.
