# 022 · Arc A4 · S3c — always-break let / case / cond / maps (CC implementation spec)

> Target: Sonnet 4.6 + `erlang-guidelines`. Implements adjudicated conformance
> decisions. **Implementation spec** — decisions made; implement exactly. **Stop
> and report** if blocked.
>
> **Output discipline:** Edit in place; don't reprint files; terse prose; run
> tests; report briefly.

## 0. Orientation

1. `CLAUDE.md` (`warnings_as_errors` ON); `erlang-guidelines` (`11-anti-patterns`).
2. Your `src/r3lfe_formatter.erl` — `flat_width/1`, `is_force_break_defform/1`,
   the specform `print_classified` clause, `print_node/2`. A4·S1+S2+S3a closed.

## 1. Decisions to implement (Duncan's rulings)

These constructs must **always break onto multiple lines, even when they would fit
in 80** (today they're flat-if-fits):

- **`let` / `let*`** — the form breaks (body on its own line) **and** the binding
  list renders one binding per line.
- **`case` / `cond`** — clauses one per line.
- **maps** (`#m(…)`) — key-value pairs one per line.

(Trailing-comment alignment is **not** changing — accepted divergence. Do not
touch comment spacing.)

## 2. Implementation

### (a) A `must_break/1` predicate driving `flat_width`

Generalize the existing defform force-break. Define `must_break(Node)` true when:
- `is_force_break_defform(Node)` (unchanged def-form behavior), **or**
- `type(Node) =:= map`, **or**
- `type(Node) =:= list` and the head is a symbol whose text is one of
  `"let"`, `"let*"`, `"case"`, `"cond"`.

In `flat_width/1`, the existing defform→`infinity` branch becomes
`must_break(Node) -> infinity`. `infinity` forces the form to break and
propagates to enclosing forms (as today). Everything else computes width normally.

(Scope note: only `let`/`let*` from the let-family are forced here, per the
ruling. `flet`/`fletrec`/`letrec-function`/etc. keep flat-if-fits for now —
mention this in your report; we can extend later.)

### (b) let/let* binding list → one binding per line

When rendering `let`/`let*` (specform N=1), the single distinguished arg is the
binding list. Render it **force-broken** so multiple bindings land one per line:
in the specform path, when the head text is `"let"` or `"let*"`, print that first
distinguished arg via the broken path (`print_broken/2`) instead of `print_node/2`,
so it ignores flat-if-fits. A single-binding list stays compact naturally
(`((x 1))` — one element, close hugs). Result:

```
(let ((x 1))            (let* ((low 1)
  (+ x 1))                     (high 2)
                               (sum (+ low high)))
                          ...)
```

Keep the binding list a list_head (bindings aligned under the first binding) —
that already happens once it breaks. Do not otherwise change the specform path;
the comment-on-head-line matrix must keep holding (let with comments still routes
through the existing guards).

### (c) case / cond / maps

These need no new rendering — `must_break` (a) forces them through the existing
broken paths: `case` → specform N=1 (expr on head line, clauses at +2); `cond` →
funcall-align (first clause on head line, rest aligned); `map` → `print_map_pairs`
(pairs per line). Just confirm with tests.

## 3. Tests — extend `r3lfe_formatter_SUITE`, group `always_break`

Golden + idempotency, all on forms that **fit in 80** (to prove they break
anyway):
- `(let ((x 1)) (+ x 1))` → breaks; `let*` with 3 bindings → one per line.
- `(case x (1 'a) (2 'b))` → clauses one per line.
- `(cond (a 1) (b 2))` → clauses one per line, aligned.
- `#m(a 1 b 2)` → pairs one per line.
- single-binding `let` stays compact (`((x 1))` not exploded).
- a `flet` (NOT forced) → still flat-if-fits (documents the scope note).
- nested: a `let` inside a `defun`, a `case` inside a `let` → propagation +
  idempotency.
- comment-matrix spot-check on a `let` and a `case` (head/last-child comments) —
  still safe.

Update any prior goldens whose shape changes (small let/case/map that used to be
flat). Re-run the full `oracles` group over the corpus — all green; add
representatives to the fixture.

## 4. Constraints

Pure module; keep `format/1`'s abstract spec + both `no_underspecs` suppressions;
`xref`+`dialyzer` clean; `warnings_as_errors`; don't modify lexer/CST. Idempotency
and the comment matrix must keep holding.

## 5. Ledger

```
Arc A4·S3c — always-break
[ ] must_break/1 (defform OR map OR let/let*/case/cond list) drives flat_width→infinity
[ ] let/let* binding list rendered force-broken (one binding/line; single stays compact)
[ ] case/cond/map always break (via must_break); confirmed by tests
[ ] flet-family scope note stated (not forced; flat-if-fits retained)
[ ] group `always_break`: goldens + idempotency (incl. single-binding, nested)
[ ] comment-matrix spot-checks on let/case still green
[ ] prior goldens updated; representatives added to corpus
[ ] full `oracles` green over corpus (state count)
[ ] xref + dialyzer clean; warnings_as_errors clean; no regressions
[ ] files changed + one-line rationale; deviations named — or "none"
```

Stop here. Do not start S3d (export/import + clause guards).
