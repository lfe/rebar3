# 022 · Arc A7 · S3a — always-break additions: if/progn/receive/try/maybe

> Target: Sonnet 4.6 + `erlang-guidelines`. First S3 sub-slice. **Implementation
> spec.** **Stop and report** if blocked.
>
> **Output discipline:** Edit in place; don't reprint files; terse prose; run
> tests; report briefly.

## 0. Orientation

1. `CLAUDE.md`; `erlang-guidelines` (`11-anti-patterns`).
2. `cc-prompt-s3.md` (shared ref); `formatting-rules.md` §2.
3. `src/r3lfe_formatter.erl` — the always-break set (`is_always_break_head/1` /
   `must_break/1`), currently `let`/`let*`/`case`/`cond` (+ maps, def-with-body).

## 1. Scope

Add **`if`, `progn`, `receive`, `try`, `maybe`** to the always-break set, so a
list with one of these heads always breaks even when it would fit on one line
(joining `let`/`let*`/`case`/`cond`). Mechanically: extend the
`is_always_break_head/1` symbol set (the predicate `must_break/1` already consults)
with these five.

Each keeps its existing specform layout when broken (e.g. `if` → test on the head
line, then/else at +2; `progn` N=0 → body at +2; `receive`/`try`/`maybe` →
clauses/sections at +2). This slice only forces the break; the layout is already
implemented.

Do **not** add anything else to the always-break set (lambda stays flat-if-fits —
that's S3c; `when`/comprehensions/`:` stay flat-if-fits).

## 2. Tests

- `(if (> x 0) x (- x))` → breaks:
  ```
  (if (> x 0)
    x
    (- x))
  ```
- a tiny `(progn (a) (b))` → breaks (one body form per line).
- a small `receive`/`try`/`maybe` that would fit → breaks.
- confirm `lambda`, `when`, `lc`, `(: …)` still flat-if-fits (NOT in the set).
- idempotency on each; full oracles over corpus green.
- **Golden updates:** update any existing goldens that showed a flat `if`/`progn`/
  `receive`/`try`/`maybe` (e.g. the `if` example) to the broken form; list them.

## 3. Ledger

```
Arc A7·S3a — always-break additions
[ ] is_always_break_head extended with if/progn/receive/try/maybe; nothing else
[ ] each always breaks (even when it would fit); existing layout unchanged when broken
[ ] lambda/when/lc/(: …) confirmed still flat-if-fits
[ ] golden updates listed; idempotency + full oracles green over corpus
[ ] xref + dialyzer standing; warnings_as_errors; full suite count stated
[ ] files changed + one-line rationale; deviations named — or "none"
```

Stop here. Clause rule is S3b; lambda rule is S3c.
