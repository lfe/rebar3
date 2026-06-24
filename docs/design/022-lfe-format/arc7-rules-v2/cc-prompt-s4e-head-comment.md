# 022 · Arc A7 · S4e — data-list head comment inline (CC spec)

> Target: Sonnet 4.6 + `erlang-guidelines`. Final S4 sub-slice.
> **Implementation spec.** **Stop and report** if blocked.
>
> **Output discipline:** Edit in place; don't reprint files; terse prose; run
> tests; report briefly. **Gallery stays paused** (S6 regenerates).

## 0. Orientation

1. `CLAUDE.md`; `erlang-guidelines` (`11-anti-patterns`).
2. `cc-prompt-s4.md` (shared ref); `formatting-rules.md` §3.9.
3. `src/r3lfe_formatter.erl` — `print_bp_container/13` (~252), specifically its
   `head_has_leading_comment(Head)` **true** branch (~259–269, currently
   opener-alone); `head_has_leading_comment/1` (~1494); `print_node/3`;
   `bp_rest_loop/5`; `close_section/8`; `emit_trailing/2`; `apply_dot_suffix/3`.

## 1. The rule (rules §3.9, corr. #3)

A leading comment as the **first element of a data list** (quoted/quasiquoted
content — `InData =:= true`) should stay on the **opener line**, with the elements
aligned under the first element — **not** the current opener-alone layout.

```
;; input                         ;; WRONG (current opener-alone)   ;; RIGHT (#3)
'(;; the items                   '(                                '(;; the items
  alpha                            ;; the items                      alpha
  beta)                            alpha                             beta)
                                   beta)
```

**Scope:** data only. Code lists (`InData =:= false`, plain calls / unknown forms
in the break-preserving regime) keep the existing opener-alone behavior — comment
safety is unchanged for code. Canonical (code/known-form) lists never reach this
branch.

## 2. The fix

In `print_bp_container/13`, the `head_has_leading_comment(Head) =:= true` branch:
split on `InData`.

- **`InData =:= false`** → unchanged (existing opener-alone via `print_rest_loop`
  at `Indent`).
- **`InData =:= true`** → emit the head's leading comment(s) on the opener line and
  align elements under the first element:
  1. `AlignCol = C + length(Open)`, `AlignStr = lists:duplicate(AlignCol, $\s)`.
  2. Take the head's leading trivia, drop blanks, keep comments in order. Emit the
     **first** comment **immediately after `Open`** on the opener line (matching
     §3.9's `'(;; comment` — glued, so the comment aligns with the elements below).
     Each **subsequent** head comment goes on its own line: `"\n" ++ AlignStr ++`
     comment text.
  3. Then the head element itself: `"\n" ++ AlignStr ++` `print_node(Head,
     AlignCol, InData)` + its trailing (`emit_trailing`). `print_node` does **not**
     re-emit the head's leading trivia (the container does), so consuming it
     manually in step 2 avoids any double-emit.
  4. Remaining children via `bp_rest_loop(RestBody, AlignCol, AlignStr, …)` — author
     break positions preserved, aligned under the first element at `AlignCol`.
  5. `apply_dot_suffix` (cons-dot tails) then `close_section` exactly as the other
     `print_bp_container` arms do (hug unless dangling / last-child trailing).

Keep the change inside this branch; do not alter the `false`-`head_has_leading_comment`
arms or the canonical path.

## 3. Tests — `r3lfe_formatter_SUITE` (new `data_head_comment` group; add to `all/0`)

- **single head comment in a quoted list** → comment on opener line, elements at
  `C+OpenLen` (assert exact output).
- **multiple head comments** → first on opener line, rest on their own lines at the
  align column, then elements.
- **quasiquote** (`` `( ``) data list → same behavior.
- **code list unchanged:** a break-preserving *code* list (`InData=false`) with a
  head leading comment still renders opener-alone — regression guard, exact output.
- **idempotency** on each (assert exact, never `contains()`): the re-lexed output
  must re-emit identically (the comment is leading trivia on the head element on
  re-parse). Full oracles (idempotency, token/comment-preservation raw-lexer,
  AST-equivalence) over corpus.
- update any affected goldens with one-line rationale.

## 4. Constraints

Pure engine; `xref`/`dialyzer` standing; `warnings_as_errors`. Idempotent;
token-/comment-preserving; AST-equivalent. **Code-list head-comment behavior
byte-identical to before.**

## 5. Ledger

```
Arc A7·S4e — data-list head comment inline (rules §3.9)
[ ] InData=true branch: first head comment on opener line; rest + elements at C+OpenLen
[ ] InData=false (code) branch unchanged (opener-alone); canonical path untouched
[ ] head element emitted once (no double-emit of its leading trivia)
[ ] reuse bp_rest_loop + apply_dot_suffix + close_section (no duplicated layout)
[ ] tests: single, multiple, quasiquote, code-unchanged regression, idempotency
[ ] full oracles green over corpus; goldens updated + rationale
[ ] xref + dialyzer standing; warnings_as_errors; full project suite count stated
[ ] files changed + one-line rationale; deviations named — or "none"
    (note: comment placement follows §3.9 glued form `(;; c`; flag if a space is wanted)
```

Stop here. Exports sort + oracle carve-out is S5.
