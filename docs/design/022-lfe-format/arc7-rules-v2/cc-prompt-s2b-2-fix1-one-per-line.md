# 022 · Arc A7 · S2b-2 · fix1 — flat-overflow calls wrap one-per-line (not fill)

> Target: Sonnet 4.6 + `erlang-guidelines`. **Scoped fix** to the S2b-2
> break-preserving renderer. Duncan's call: a call written fully flat that
> overflows should wrap **one argument per line under the first arg**, not greedy
> fill. **Stop and report** if blocked.
>
> **Output discipline:** Edit in place; don't reprint files; terse prose; run
> tests; report briefly.

## 0. The decision

For a **break-preserving** container that must break:
- **If the author grouped it across lines** (`multiline(Node)` true — some child has
  `nl_before`) → **preserve** the author's grouping: current behavior (break before
  a child iff `nl_before(child)` or it would overflow). This keeps `make-op`'s k/v
  pairs. Unchanged.
- **If the author wrote it fully flat** (`multiline(Node)` false) but it must break
  (overflows 80, or has a `must_break` descendant) → **one argument per line**,
  each aligned under the first argument. **Not** greedy fill.

(So `multiline/1` is the discriminator: grouped → preserve; flat → one-per-line.)

## 1. The fix

In the break-preserving broken renderer:
- when `multiline(Node)` is **true** → keep the `nl_before`-driven logic as-is.
- when `multiline(Node)` is **false** → emit head + first child on the opener line,
  then **every** subsequent child on its own line at the align column
  (align-under-first-arg; if the first child itself broke, fall to open-col + 2 as
  today). Do not pack multiple children per line.

Result (flat input `(some-function aaa bbb ccc)` that overflows):
```
(some-function aaaaaaaaaaaaaaaaaaaaaa
               bbbbbbbbbbbbbbbbbbbbbb
               cccccccccccccccccccccc)
```
`make-op` (author-grouped, multiline) is unaffected — pairs stay grouped.

**Idempotency check (do verify):** the one-per-line output is itself `multiline`
with `nl_before` on each wrapped child, so reformatting routes through the preserve
branch and reproduces the same layout. Confirm `format(format(x)) == format(x)`.

## 2. Tests

- Revert/update the goldens CC changed to greedy-fill back to **one-per-line**:
  at least `indent_funcall_align`, `indent_list_head`, `data_tuple_wide` (and any
  other that asserts multiple flat args packed per line). Leave the
  first-arg-overflow → `C+2` hanging goldens and the `case`-head-on-opener
  regime golden as they are (those are correct).
- Add/keep: a flat call that overflows → one-per-line; a `make-op`-style
  author-grouped call → grouping preserved (regression); a flat call that fits →
  stays flat.
- Full oracles over the corpus: idempotency, token-, comment-preservation,
  AST-equivalence — green.

## 2a. Drive-by cleanup (tiny)

Remove the now-stale comment at `regime/2` (≈line 55):
`"A7·S1 (cons-dot) not yet merged; dot tokens currently render as symbols."` —
cons-dot **is** merged and `regime/2` handles `dot_token`; the comment contradicts
the code. Delete or correct it.

## 3. Constraints

Pure engine; `xref`/`dialyzer` standing; `warnings_as_errors`. Don't touch
lexer/CST. Grouping-preservation (multiline case) must remain intact.

## 4. Ledger (closes S2b-2)

```
Arc A7·S2b-2·fix1 — one-per-line flat overflow
[ ] BP renderer: multiline → preserve (unchanged); flat+must-break → one-per-line
    under first arg (no greedy fill)
[ ] make-op / author-grouped calls still preserved (regression test)
[ ] greedy-fill goldens reverted to one-per-line; first-arg-hanging goldens untouched
[ ] idempotency verified on flat-overflow (one-per-line is a fixed point)
[ ] full oracles green over corpus; xref + dialyzer standing; warnings_as_errors
[ ] files changed + one-line rationale; deviations named — or "none"
```
