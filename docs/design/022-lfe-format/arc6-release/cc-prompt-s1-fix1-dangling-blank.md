# 022 · Arc A6 · S1 · fix1 — close the blank-as-break-forcing class (dangling)

> Target: Sonnet 4.6 + `erlang-guidelines`. **Tiny, test-first** follow-up to
> A6·S1. **Stop and report** if blocked.
>
> **Output discipline:** Edit in place; terse prose; run tests; report briefly.

## 0. Context

A6·S1 found that **blank-only leading trivia** was wrongly forcing broken layout
(an idempotency bug) and fixed it via `has_comment_leading/1` (blanks ignored,
only comments force a break). The **same logic still applies to dangling trivia**:
`has_internal_trivia/1` uses `dangling(Node) =/= []` (own) and
`has_descendant_trivia/1` uses `dangling(Node) =/= []` (descendant) — both treat a
**blank-only dangling** (a blank line just before a closing paren, no comment) as
break-forcing. This is the same class. It may or may not actually be
non-idempotent — verify, then fix only if needed.

## 1. Test first

Add an idempotency + format test for a **blank-only dangling**, e.g. source like:

```
(foo
  a
  b

  )
```

and the nested/guard variants (a blank line before the close inside a `let` body,
inside a clause). Assert `format(format(X)) == format(X)` and token/comment
preservation. Run it.

- **If it passes** as-is → no code change needed; keep the test as a permanent
  regression and report "dangling-blank already idempotent, test added."
- **If it fails** → apply §2.

## 2. Fix (only if the test fails)

Mirror the leading fix for dangling: add `has_comment_dangling/1` (same shape as
`has_comment_leading/1` — skip `blank`, true on `{comment,_}`), and use it in
place of the bare `dangling(Node) =/= []` checks in **both**
`has_internal_trivia/1` (own dangling) and `has_descendant_trivia/1` (descendant
dangling). Keep `trailing =/= []` as-is (trailing is always a comment). Re-run the
full oracle suite — idempotency, token-, comment-preservation, AST-equivalence —
all green; comments must still force breaks.

## 3. Constraints

Pure module; `xref`/`dialyzer` at standing level; `warnings_as_errors`. Comments
(leading/trailing/dangling) must still force breaks — only blank-only items are
ignored.

## 4. Ledger

```
Arc A6·S1·fix1 — dangling blank
[ ] dangling-blank idempotency test added (own + nested/guard variants)
[ ] verified: passes as-is (no change), OR fix applied (has_comment_dangling)
[ ] if fixed: has_comment_dangling used in has_internal_trivia + has_descendant_trivia
[ ] full oracle suite green; comments still force breaks
[ ] xref + dialyzer at standing level; warnings_as_errors; no regressions
[ ] files changed + one-line rationale; deviations named — or "none"
```
