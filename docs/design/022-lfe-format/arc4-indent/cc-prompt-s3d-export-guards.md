# 022 · Arc A4 · S3d — export/import keyword-alone + clause guards (CC spec)

> Target: Sonnet 4.6 + `erlang-guidelines`. **Final A4 sub-slice — closes Arc A4.**
> Implements the last two adjudicated conformance decisions. **Implementation
> spec** — implement exactly; **stop and report** if blocked.
>
> **Output discipline:** Edit in place; don't reprint files; terse prose; run
> tests; report briefly.

## 0. Orientation

1. `CLAUDE.md` (`warnings_as_errors` ON); `erlang-guidelines` (`11-anti-patterns`).
2. Your `src/r3lfe_formatter.erl` — `specform_table/0`, the `list_head`
   `print_classified` clause, the comment-matrix helpers. A4·S1+S2+S3a+S3c closed.

## 1. Decision A — `export`/`import` keyword-alone

Make `export` and `import` render with the keyword alone on its line and items
indented when they break (and flat when they fit):

```
(export (run 0))                 ; fits → flat

(export                          ; wide → keyword alone, items at +2
  (ackermann 2)
  (factorial 1)
  (large-prime-number? 1))
```

**Implementation:** add `"export" => 0` and `"import" => 0` to `specform_table/0`.
That's it — specform N=0 already means "head alone on the opener line, all items
at `C+2`, flat-if-fits", which is exactly this. Add a comment noting these two are
an **intentional extension beyond `lfe-indent.el`** (which has no entry for them),
per Duncan's ruling. (Scope: just `export` and `import` for now; note that other
module clauses could be added later.)

## 2. Decision B — match-clause: pattern + guard on one line

In a match clause whose pattern is a list, when a `(when …)` guard immediately
follows the pattern, keep **pattern and guard on the same line**, with the body
below:

```
;; before (current list_head)        ;; after (this slice)
((n acc)                             ((n acc) (when (> n 0))
 (when (> n 0))                       (factorial (- n 1) (* n acc)))
 (factorial (- n 1) (* n acc)))
```

**Implementation — in the `list_head` rendering only:** if the children are
`[Pat, Guard | Body]` where `Guard` is a list whose head is the symbol `when`
(add an `is_when_form/1` helper), then:
- print `Pat` on the head line at `AlignCol = C + OpenLen`;
- print `Guard` on the **same** line, after `Pat` + one space (threaded column =
  `PatEndCol + 1`);
- print `Body` (the rest) one per line aligned at `AlignCol` via `print_rest_loop`;
- `close_section` as usual (receives the body's `HasTrail`, or — if `Body` is
  empty — whether `Guard` had a trailing comment).

**Comment safety (reuse the established pattern):** if `Pat` or `Guard` has a
leading or trailing comment, do **not** combine them — fall back to the plain
`list_head` rendering (every element on its own line). A comment must never be
followed by content on the same line.

(Only list-headed clauses get this — `(pat (when …) body)` where `pat` is a list,
i.e. defun match-clause / match-lambda style. A clause whose pattern is an atom is
symbol-headed and goes through funcall; leave it as-is and note this.)

## 3. Tests — extend `r3lfe_formatter_SUITE`, group `export_guards`

Golden + idempotency:
- `defmodule` with a **wide** `export` → keyword alone, items at +2.
- `defmodule` with a **short** `export (run 0)` → stays flat.
- `import` with `(from …)` and `(rename …)` → keyword alone when wide.
- a `defun` match-clause **with a guard** (the `factorial`/`ackermann` style) →
  pattern + guard on one line, body below.
- a `match-lambda` clause with a guard.
- guard clause where `Pat`/`Guard` carries a **comment** → falls back to
  element-per-line (no swallow); token-preserved, idempotent.
- confirm a non-guard clause (`((n) body)`) is unchanged.

Update the `defmodule`/match-clause goldens from earlier slices whose shape now
changes; list which and why. Re-run the full `oracles` group over the corpus —
all green; add representatives to the fixture.

## 4. Constraints

Pure module; keep `format/1`'s abstract spec + both `no_underspecs` suppressions;
`xref`+`dialyzer` clean; `warnings_as_errors`; don't modify lexer/CST. Idempotency
and the comment matrix must keep holding.

## 5. Ledger (closes Arc A4)

```
Arc A4·S3d — export/import + clause guards
[ ] export/import added to specform_table as N=0 (keyword-alone, flat-if-fits);
    noted as an intentional extension beyond lfe-indent.el
[ ] list_head: Pat + (when …) guard kept on one line; body below at AlignCol
[ ] is_when_form/1 helper; column threading consistent (idempotent)
[ ] comment on Pat/Guard → fallback to element-per-line (no swallow)
[ ] atom-pattern clause (symbol-headed) left as-is; noted
[ ] group `export_guards`: goldens + idempotency (export wide/short, import,
    defun guard, match-lambda guard, comment fallback, non-guard unchanged)
[ ] prior defmodule/match-clause goldens updated; representatives added to corpus
[ ] full `oracles` green over corpus (state count)
[ ] xref + dialyzer clean; warnings_as_errors clean; no regressions
[ ] files changed + one-line rationale; deviations named — or "none"
```

When this slice's ledger is green, **Arc A4 is complete** — the formatter matches
the adjudicated LFE house style. Remaining: A5 (provider/CLI) and A6 (release).
After A4, refresh gallery entries §36 (guard) and §38/§39 (export/import).
