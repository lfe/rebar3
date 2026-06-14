# 022 · Arc A4 · S1 — indent table + 3 primitives (CC implementation spec)

> Target: Sonnet 4.6 + `erlang-guidelines`. First A4 sub-slice. **Implementation
> spec** — decisions are made; implement exactly. **Stop and report** if blocked.
>
> **Output discipline:** Edit in place; don't reprint files; terse prose; run
> tests; report briefly. Stay within this slice.

## 0. Orientation

1. `CLAUDE.md` (`warnings_as_errors` ON); `erlang-guidelines` (`11-anti-patterns`).
2. `arc4-indent/cc-prompt.md` — the **shared reference**: head classification, the
   specform table (verbatim), and the reflow semantics for each class. This slice
   implements the table + three primitives + provisional defform.
3. `lfe/emacs/lfe-indent.el` — the source of truth for the table (reference only).
4. Your `src/r3lfe_formatter.erl` (A3, closed) — you extend `print_broken_container`
   and add a classification function. Do not touch lexer/CST.

## 1. Scope of THIS slice

Replace A3's uniform broken-container rule with **head-classified** rendering:

- Add `classify_head/1` → `{specform, N} | defform | funcall | list_head`, per the
  shared ref's algorithm (list-head if head not a symbol; else table lookup; else
  `def`-prefix → defform; else funcall). Encode the specform table as a function
  clause set or a literal map — exactly the entries in the shared ref.
- Implement the broken-form rendering for each class (shared ref "Reflow
  semantics"):
  - **specform N**: distinguished args `1..N` on the head line (space-joined after
    head; each may itself span lines), body `N+1..end` at `C+2` one per line;
    `Close` hugs last. N=0 ⇒ head alone, all args at `C+2`.
  - **funcall**: `a1` on the head line; `a2..end` aligned under `a1`'s column
    (`C + len(Open) + len(flat(head)) + 1`); `Close` hugs last.
  - **list_head**: all elements aligned under the first (`C + len(Open)`), one per
    line; `Close` hugs last.
  - **defform (provisional)**: treat as specform 1 (name on head line, rest at
    `C+2`). Mark clearly in a comment that S2 refines this.
- Preserve **all** A3 trivia handling (leading/trailing/dangling, the
  head-leading-comment branch, blank policy). Only the indentation columns and
  which args share the head line change. The flat path is unchanged.

Column threading is the crux (as in A3·S2): the column you pass to each child must
equal the spaces you actually emit before it. funcall's align column and
list_head's align column are new; compute them from `flat(head)` length and
`len(Open)` and thread them consistently.

## 2. Tests — extend `r3lfe_formatter_SUITE`, group `indent`

Golden + idempotency for each class (use forms that must break, i.e. exceed 80 or
contain a comment):
- **funcall** align-under-first-arg: `(some-fn a b c …)` wide ⇒ args under `a`.
- **specform N≥1**: `case` (expr on head line, clauses at +2); `if`; a wide
  `let` whose binding-list is list-head-aligned and body at +2.
- **specform N=0**: `progn`/`receive` (head alone, body at +2).
- **list_head**: a bare `((a b) (c d) …)` wide ⇒ elements aligned under the first.
- **provisional defform**: a `defun` (assert the provisional shape so S2 can update
  it deliberately).

Then re-run the full `oracles` group (idempotency, token-, comment-preservation,
AST-equivalence) over the full corpus — all green. (The existing A3 golden tests
whose output shape changes under the new rules: update their expected output and
note which you changed and why.)

## 3. Constraints

Pure module; keep `format/1`'s abstract `{ok, iolist()} | {error,_}` spec and the
`-dialyzer({no_underspecs, format/1})`; `xref`+`dialyzer` clean;
`warnings_as_errors`; don't modify lexer/CST (report bugs, don't patch across
modules).

## 4. Ledger

```
Arc A4·S1 — indent table + primitives
[ ] classify_head/1 implements the shared-ref algorithm; full specform table
[ ] specform N (incl. N=0) rendering; body at C+2; distinguished args on head line
[ ] funcall align-under-first-arg (correct align column, threaded)
[ ] list_head vertical alignment under first element
[ ] provisional defform (=specform 1), marked for S2
[ ] group `indent`: goldens + idempotency per class, green
[ ] full `oracles` group green over corpus (state count)
[ ] A3 goldens whose shape changed: updated + listed with rationale
[ ] xref + dialyzer clean; warnings_as_errors clean; no regressions
[ ] files changed + one-line rationale; deviations named — or "none"
```

Stop here. Do not start S2 (def-forms + docstrings).
