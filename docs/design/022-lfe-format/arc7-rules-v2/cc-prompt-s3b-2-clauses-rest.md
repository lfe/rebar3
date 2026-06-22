# 022 · Arc A7 · S3b-2 — clause rule for match-lambda / defun-match / receive

> Target: Sonnet 4.6 + `erlang-guidelines`. Second half of S3b — apply the
> already-built clause rule to the remaining clause-bearing forms. **Implementation
> spec.** **Stop and report** if blocked.
>
> **Output discipline:** Edit in place; don't reprint files; terse prose; run
> tests; report briefly.

## 0. Orientation

1. `CLAUDE.md`; `erlang-guidelines` (`11-anti-patterns`).
2. `cc-prompt-s3.md` (shared ref); `formatting-rules.md` §3.3.
3. `src/r3lfe_formatter.erl` — `render_clause`/`trivial_clause` + the clause loop
   variants from S3b-1; the `IsCaseHead`/`IsCondHead` dispatch pattern to mirror.

## 1. Scope — route clause children through `render_clause`

Reuse the S3b-1 `render_clause` (flat iff single trivial datum; else broken
list_head with the A4·S3d guard handling). Wire it into the remaining forms whose
body children are clauses:

- **`match-lambda`** (specform N=0): all children are clauses
  `((pat…) body…)` → each via `render_clause`.
- **`defun`/`defmacro` match-clause form** (the dynamic-N=1 case where arg2 is a
  match clause): the children after the name are clauses → each via `render_clause`.
  (The signature/simple-arglist form is unaffected — it has no clauses.)
- **`receive`**: the pattern clauses `(pat body…)` → each via `render_clause`. The
  `(after timeout body…)` section is **not** a clause — leave its current handling
  unchanged.

**Out of scope (deferred to S4):** `try`'s `case`/`catch` section clauses — S4
already covers `try` symmetry (#10), so the clause routing for `try` lands there to
keep the two consistent. Note this in your report.

Add the necessary head-dispatch (mirroring `IsCaseHead`/`IsCondHead`): detect
match-lambda / defun-match / receive when rendering, and route their clause
children through the clause loop.

## 2. Tests — `r3lfe_formatter_SUITE`, group `clauses` (extend)

- `match-lambda`: trivial clause stays flat (`((x) x)`); non-trivial clause breaks
  (`((x) (process x))` → pattern line + body below); multi-clause match-lambda each
  clause one per line.
- `defun` match-form: `(defun f ((0) 1) ((n) (* n (f (- n 1)))))` → trivial clause
  `((0) 1)` flat, non-trivial `((n) (...))` breaks below the pattern; the name stays
  on the head line.
- `defun` match-form **with guard** (the `factorial` example) → pattern+guard on one
  line, body below (regression vs gallery #36).
- `receive`: a non-trivial clause breaks; a trivial one stays flat; `(after …)`
  unchanged.
- idempotency on each; full oracles over corpus green; update affected goldens
  (the gallery-style `factorial`/`fact`/`match-lambda` shapes).

## 3. Constraints

Pure engine; reuse `render_clause` (don't duplicate the rule); `xref`/`dialyzer`
standing; `warnings_as_errors`. Idempotent; token-/comment-preserving;
AST-equivalent.

## 4. Ledger (closes S3b)

```
Arc A7·S3b-2 — clauses (match-lambda / defun-match / receive)
[ ] match-lambda, defun/defmacro match-form, receive clauses routed via render_clause
[ ] trivial → flat; non-trivial / multi-form → break below pattern; guard on pattern line
[ ] receive (after …) unchanged; try clauses explicitly deferred to S4 (noted)
[ ] clauses group extended; idempotency + full oracles green; goldens updated
[ ] xref + dialyzer standing; warnings_as_errors; full suite count stated
[ ] files changed + one-line rationale; deviations named — or "none"
```

Stop here. Lambda structure rule is S3c.
