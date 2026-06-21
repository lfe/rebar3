# 022 · Arc A7 · S3b-1 — clause body rule: the renderer + case/cond (CC spec)

> Target: Sonnet 4.6 + `erlang-guidelines`. First half of S3b. **Implementation
> spec.** **Stop and report** if blocked.
>
> **Output discipline:** Edit in place; don't reprint files; terse prose; build
> incrementally; run tests; report briefly.

## 0. Orientation

1. `CLAUDE.md`; `erlang-guidelines` (`11-anti-patterns`).
2. `cc-prompt-s3.md` (shared ref); `formatting-rules.md` §3.3.
3. `src/r3lfe_formatter.erl` — `case` and `cond` rendering, `print_broken`/list_head
   (which already does pattern + `(when …)` guard on one line, A4·S3d).

## 1. The rule (formatting-rules §3.3)

A **clause** `(pattern body…)` (the children of `case`/`cond` here):
- **Flat** iff its body is a **single trivial datum** and it fits — where "trivial
  datum" = a leaf (symbol / number / string / char) or a prefixed leaf (`'x`,
  `,x`), and the clause has exactly two children (pattern + that datum) with no
  comments. e.g. `(1 'one)`, `((> x 0) 'pos)`.
- **Otherwise break:** pattern (with its `(when …)` guard, if any, on the same
  line per A4·S3d) on the first line; body form(s) below, list_head-aligned under
  the pattern. e.g. `((tuple 'ok v) (store v))` →
  ```
  ((tuple 'ok v)
   (store v))
  ```

The enclosing `case`/`cond` already always-breaks (clauses one per line); this
slice governs each **clause's own** flat-vs-break.

## 2. Implementation

Add `render_clause(Clause, Col, InData)`:
- if `trivial_clause(Clause)` → `flat_render` (flat).
- else → `print_broken(Clause, Col, InData)` — forces the broken list_head layout
  (pattern/guard line + body below). This **overrides** the regime's normal
  flat-if-author-flat behavior for clauses (a flat-written non-trivial clause must
  still break).

`trivial_clause(Clause)`: `Clause` is a `list`; its children are exactly
`[_Pattern, Datum]`; `Datum`'s type is a leaf (`symbol`/`number`/`string`/`char`)
or a `prefixed` node whose inner is such a leaf; and the clause carries no comment
trivia (leading/trailing/dangling anywhere in it). Otherwise not trivial.

Wire it in:
- **`case`**: the clauses are the body children (everything after the distinguished
  `expr`). Render each via `render_clause` instead of the generic child path.
- **`cond`**: the clauses are all children after the `cond` head. Render each via
  `render_clause`. (cond stays funcall-aligned — first clause on the head line,
  rest aligned under it — but each clause's *body* now follows the clause rule.)

Do **not** touch match-lambda / defun-match / receive / try yet (S3b-2).

## 3. Tests — `r3lfe_formatter_SUITE`, group `clauses`

- `case` trivial clauses stay flat: `(case x (1 'one) (2 'two))` →
  ```
  (case x
    (1 'one)
    (2 'two))
  ```
- `case` non-trivial clause breaks: `(case r ((tuple 'ok v) (store v)))` →
  ```
  (case r
    ((tuple 'ok v)
     (store v)))
  ```
- `cond`: trivial bodies flat (`((> x 0) 'pos)`); a clause with a call body breaks.
- a clause with a `(when …)` guard → pattern+guard on one line, body below
  (regression that A4·S3d still holds through the clause path).
- a clause with multiple body forms → all break below the pattern.
- idempotency on each; full oracles over corpus green; update any affected goldens.

## 4. Constraints

Pure engine; `xref`/`dialyzer` standing; `warnings_as_errors`. Idempotent;
token-/comment-preserving; AST-equivalent.

## 5. Ledger

```
Arc A7·S3b-1 — clauses (case/cond)
[ ] render_clause + trivial_clause implemented per §3.3
[ ] case clauses + cond clauses routed through render_clause
[ ] trivial clause → flat; non-trivial (call/compound/multi-form) → break below pattern
[ ] guard clause: pattern+guard one line, body below (A4·S3d still holds)
[ ] group `clauses` tests; idempotency + full oracles green; goldens updated
[ ] xref + dialyzer standing; warnings_as_errors; full suite count stated
[ ] files changed + one-line rationale; deviations named — or "none"
```

Stop here. match-lambda / defun-match / receive / try clauses are S3b-2.
