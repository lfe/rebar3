# 022 · Arc A4 — special-form indentation + alignment (shared reference)

> ⚠️ **DO NOT hand CC this whole file.** A4 is sub-sliced to stay under the output
> cap. Hand one at a time:
> - `cc-prompt-s1-table.md` — the indent table + 3 primitives (specform / funcall
>   align-under-first-arg / list-head); provisional defform.
> - `cc-prompt-s2-defforms.md` — `defun`/`defmacro`/`defmodule` signature lines,
>   docstrings, clause layout. *(JIT after S1.)*
> - `cc-prompt-s3-conformance.md` — style-guide golden conformance + idempotency
>   sweep. *(JIT after S2.)*
>
> This file is the **shared reference**: the indent table, the reflow semantics,
> and the fixed decision below. Read it, then work the sub-slice you were handed.

## Goal

Replace A3's uniform "+2 hang, head on opener line" break rule with LFE's real
indentation, per `lfe/emacs/lfe-indent.el` and the style guide. Only the
**broken-form rendering** changes; flat rendering, comment/trivia handling, the
oracles, and the lexer/CST are untouched. All A3 invariants (idempotency,
token-/comment-preservation, AST-equivalence) must continue to hold.

## Fixed decision (Duncan, this session)

**Plain function calls (funcall, no indent rule) align continuation arguments
under the first argument**, faithful to `lfe-indent.el`'s default. Not +2 hang.

```
(some-function arg-one
               arg-two
               arg-three)
```

## Head classification (compute once per breaking list)

Given a breaking list node `(head a1 a2 … aN)`:

1. **head is not a symbol** (a list/tuple/etc.) → **list-head alignment**.
2. **head text is in the specform table** (below) → **specform N**.
3. **else head text starts with `def` and length > 3** → **defform**.
4. **else** → **funcall**.

(Order matters: the table wins over the `def`-prefix, so `define-function` is
specform 1, while `defun` — not in the table — is defform. This mirrors
`lfe-indent-function`'s cond.)

### The specform table (symbol → N), verbatim from `lfe-indent.el`

```
:  2      after 1    bc 1       binary-comp 1   call 2     case 1
catch 0   define-function 1     define-macro 1  define-module 1
extend-module 0       do 2      else 0          eval-when-compile 0
flet 1    flet* 1     fletrec 1 if 1            lambda 1
let 1     let* 1      let-function 1            letrec-function 1
let-macro 1           lc 1      list-comp 1     macrolet 1
match-lambda 0        match-spec 0              maybe 0
prog1 1   prog2 2     progn 0   receive 0       try 1     when 0
syntaxlet 1           defflavor 3
begin 0   let-syntax 1          syntax-rules 0  macro 0
```

(`defflavor` is in the table as specform 3 even though it matches `def` — the
table is checked first, so it stays specform 3.)

## Reflow semantics for each class (broken form)

Let `C` = the list's opening column (0-based), `Open`/`Close` = delimiter texts.

- **specform N** — args `1..N` (the *distinguished* args) start on the head line,
  space-separated after the head: `(head d1 d2 … dN`. A distinguished arg may
  itself span multiple lines (it is printed starting at its column on the head
  line). The *body* args `N+1..end` go at `C + 2`, one per line. `Close` hugs the
  last emitted child. If there are fewer than `N` args, put them all on the head
  line with no body.
  - N = 0 (e.g. `progn`, `receive`, `match-lambda`): head alone on the opener
    line; **all** args at `C + 2`, one per line.
  - This makes `case`/`if`/`let`/… come out right: `(case <expr>` then clauses at
    `C+2`; `(let <binding-list>` then body at `C+2`, where the binding-list is a
    list-head-aligned list (next rule).

- **funcall** — `a1` starts on the head line: `(head a1`. Args `a2..end` each on
  their own line, aligned under `a1`'s column =
  `C + len(Open) + len(flat(head)) + 1`. `Close` hugs the last arg. (If `a1`
  itself must break, it breaks at that column.)

- **list-head** (head not a symbol) — every element aligned vertically under the
  **first element**, at column `C + len(Open)`, one per line. `Close` hugs the
  last element. This is what aligns `let` bindings and clause lists:
  ```
  ((x 1)
   (y 2))
  ```

- **defform** — `defun`/`defmacro`/`defmodule`/etc. Full treatment is **S2**
  (signature line + docstrings + clause layout). **S1 uses a provisional rule:**
  treat defform as specform 1 (head + name on the opener line, everything else at
  `C + 2`). Correct and idempotent, just not yet pretty — S2 refines it.

Comments/trivia: keep A3's emission model unchanged (leading/trailing/dangling,
the head-leading-comment branch, blank policy). Indentation columns now come from
the rules above instead of the uniform +2.

## Invariants (every A4 sub-slice)

Idempotency, token-preservation, comment-preservation, AST-equivalence — all must
stay green over the full corpus. Plus the new golden/conformance tests per slice.
Keep `format/1`'s abstract `iolist()` spec + the `no_underspecs` suppression.
Pure module; `xref`+`dialyzer` clean; `warnings_as_errors`; don't touch lexer/CST.
