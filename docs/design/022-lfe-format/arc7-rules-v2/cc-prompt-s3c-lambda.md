# 022 · Arc A7 · S3c — lambda structure rule (CC spec)

> Target: Sonnet 4.6 + `erlang-guidelines`. Final S3 sub-slice — closes S3.
> **Implementation spec.** **Stop and report** if blocked.
>
> **Output discipline:** Edit in place; don't reprint files; terse prose; run
> tests; report briefly.

## 0. Orientation

1. `CLAUDE.md`; `erlang-guidelines` (`11-anti-patterns`).
2. `cc-prompt-s3.md` (shared ref); `formatting-rules.md` §3.2.
3. `src/r3lfe_formatter.erl` — `lambda` is specform N=1 in `specform_table`, **not**
   in `is_always_break_head` (it's flat-if-fits); `must_break/1` drives forced
   breaks via `flat_width → infinity`.

## 1. The rule (formatting-rules §3.2)

`lambda` is **flat** iff its body is a **single non-structural form** that fits;
otherwise it breaks:
- 1 body form, flat-eligible, fits in 80 → flat. e.g. `(lambda (x y) (+ x y))`,
  `(lambda () (do-thing))`.
- **2+ body forms** (implicit `progn`) → **break** (one body form per line at +2),
  even if it would fit. e.g. `(lambda (x) (a) (b))`.
- single body form that is itself structural (a `case`/`let`/map/… that breaks) →
  already breaks via `must_break` propagation — no new logic needed.
- overflow → breaks as usual.

So the only **new** behavior to add: a `lambda` with **more than one body form**
always breaks.

## 2. Implementation

Add a `lambda`-specific must-break: a `lambda`-headed list whose **body has more
than one form** forces a break. Body-form count = `length(children) - 2` (children
= `[lambda-head, arglist | body…]`); > 1 → must break. Wire this into `must_break/1`
(alongside the existing always-break-head / map / def checks) so `flat_width`
returns `infinity` for such a lambda.

Do **not** put `lambda` in `is_always_break_head` (a single-body lambda must stay
flat-if-fits). Keep the specform N=1 layout for the broken case (arglist on the
head line, body at +2).

(Edge: `(lambda (x))` with no body, or `(lambda () x)` single body → ≤1 body form →
flat-if-fits, unchanged.)

## 3. Tests — `r3lfe_formatter_SUITE`, group `lambda`

- `(lambda (x y) (+ x y))` → flat (1 body form).
- `(lambda () (do-thing))` → flat.
- `(lambda (x) (a) (b))` → breaks: arglist on head line, `(a)` and `(b)` at +2.
- `(lambda (x) #m(k v))` / `(lambda (x) (case x …))` → breaks (structural single
  body; confirm it already did via must_break).
- a single-body lambda that overflows → breaks.
- idempotency on each; full oracles over corpus green; update affected goldens.

## 4. Constraints

Pure engine; `xref`/`dialyzer` standing; `warnings_as_errors`. Idempotent;
token-/comment-preserving; AST-equivalent.

## 5. Ledger (closes S3)

```
Arc A7·S3c — lambda
[ ] must_break: lambda with >1 body form forces break (count = children-2 > 1)
[ ] lambda NOT added to is_always_break_head (single-body lambda stays flat-if-fits)
[ ] structural single-body lambda still breaks via must_break (confirmed)
[ ] group `lambda` tests; idempotency + full oracles green; goldens updated
[ ] xref + dialyzer standing; warnings_as_errors; full suite count stated
[ ] files changed + one-line rationale; deviations named — or "none"
```

When green, **S3 is complete**; A7 continues with S4 (layout refinements).
