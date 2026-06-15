# 022 · Arc A4 · S2 — def-forms, signature lines, docstrings (CC implementation spec)

> Target: Sonnet 4.6 + `erlang-guidelines`. Second A4 sub-slice. **Implementation
> spec** — decisions made; implement exactly. **Stop and report** if blocked.
>
> **Output discipline:** Edit in place; don't reprint files; terse prose; run
> tests; report briefly.

## 0. Orientation

1. `CLAUDE.md` (`warnings_as_errors` ON); `erlang-guidelines` (`11-anti-patterns`).
2. `arc4-indent/cc-prompt.md` shared ref — head classification, specform semantics.
3. Your `src/r3lfe_formatter.erl` — `classify_head/1`, `flat_width/1`, the
   `print_classified` defform + specform clauses. A4·S1 (incl. fix1+fix2) is
   closed; the comment-on-head-line matrix is fully handled and must stay so.

## 1. Scope of THIS slice — make defform real

Today defform is provisional (= specform 1). Two changes:

### (a) Def-form body breaking — nuanced (Duncan's decision)

Rule:
- A `defun`/`defmacro` with an **empty arglist** `()` — the LFE *constant* idiom,
  e.g. `(defun +my-pi+ () 3.14)` — renders **flat if it fits** in 80 (and is
  comment-free), like any other node. One line.
- A `defun`/`defmacro` **with arguments** (non-empty arglist) **or** in
  **match-clause** form **always breaks** its body onto the next line(s), even
  when the whole form would fit in 80.
- **All other defform heads** (`defmodule`, `defrecord`, `defstruct`,
  `defsyntax`, …) **always break**.
- Anything exceeding 80 or carrying comments breaks as usual.

Implement via **`flat_width/1`**: for a `list` node whose head classifies as
`defform`, return `infinity` (force break) — **except** when it is a
`defun`/`defmacro` with an *empty* arglist, in which case compute the width
normally (allow flat-if-fits). `infinity` both forces the break and propagates
(a parent containing a force-broken def-form also breaks — you can't inline a
definition). A force-broken def-form with no body still renders compactly because
the close hugs when there is nothing to break.

"Empty arglist": for `defun`/`defmacro`, `RestChildren = [_Name, Arg2 | _]` where
`is_arglist(Arg2)` and `children(Arg2) =:= []`. (Same `is_arglist` as part (b).)

(The nuance applies to `defun` **and** `defmacro` — parallel structure. Only the
**defform** class is affected; `define-function`/`define-module`/`extend-module`/
`defflavor` are specforms and keep normal flat-if-fits.)

### (b) `defun`/`defmacro` signature line via dynamic N

In the **defform** `print_classified` clause, choose the specform N from the
shape, then delegate to the specform path (so the whole comment matrix and
idempotency handling are inherited — do **not** reimplement them):

- head text is `"defun"` or `"defmacro"`:
  - if `RestChildren` is `[_Name, Arg2 | _]` and `is_arglist(Arg2)` → **N = 2**
    (signature form: `(defun name (args)` on the head line, body at `C+2`);
  - else → **N = 1** (match-clause form: `(defun name` on the head line, clauses
    at `C+2`).
- any other defform head (`defmodule`, `defrecord`, `defstruct`, `defsyntax`, …)
  → **N = 1** (name on head line, rest at `C+2`).

`is_arglist(Node)`: `type(Node) =:= list` **and** (`children(Node) =:= []` **or**
the first child's `type` is **not** `list`). So `()` and `(x y)` are arglists;
`((pat) body)` (first child is a list) is a match clause, not an arglist.

**Docstrings need no special handling:** a docstring is just the first body form
(a string), so it lands at `C+2` naturally — e.g.
`(defun f (x) "doc" (+ x 1))` →
```
(defun f (x)
  "doc"
  (+ x 1))
```

## 2. Tests — extend `r3lfe_formatter_SUITE`, group `defforms`

Golden + idempotency:
- simple `defun` with args + single-expr body (e.g. `factorial`) → signature line
  + body at +2 (**breaks even though it fits** — has args).
- tiny `defun` with args: `(defun id (x) x)` → still breaks (has args).
- `defun` **constant** (empty arglist): `(defun +my-pi+ () 3.14)` → stays on **one
  line** (fits, no args). And a no-args defun that *exceeds* 80 → breaks.
- `defun` with a docstring → docstring on its own line at +2.
- `defun` with a multi-form body.
- **match-clause** `defun` (`(defun f ((0) 1) ((n) (* n 2)))`) → name on head line,
  each clause at +2.
- `defmacro` (both signature and match-clause).
- `defmodule` and `defrecord` → name on head line, members at +2 (specform-1).
- a def-form **nested** inside another form → parent breaks too (propagation).
- comment-matrix spot-checks on a def-form (head-leading, head-trailing, last
  clause trailing) — confirm the S1 invariants still hold for def-forms.

Update the A4·S1 `defform_provisional` golden (its shape changes now). Re-run the
full `oracles` group (idempotency, token-, comment-preservation, AST-equivalence)
over the corpus — all green; add representative def-forms to the corpus fixture.

## 3. Constraints

Pure module; keep `format/1`'s abstract spec + both `no_underspecs` suppressions;
`xref`+`dialyzer` clean; `warnings_as_errors`; don't modify lexer/CST. Reuse the
specform path — the comment-on-head-line matrix (S1 fix1/fix2) must keep holding
for def-forms.

## 4. Ledger

```
Arc A4·S2 — def-forms
[ ] flat_width: infinity for defform-headed lists EXCEPT defun/defmacro with empty
    arglist (constant idiom = flat-if-fits); propagates to parents
[ ] no-args defun fits → one line; with-args/match-clause defun → body breaks
[ ] defun/defmacro dynamic N (2 = signature form via is_arglist; 1 = match-clause)
[ ] other defform → specform 1; delegates to specform path (matrix inherited)
[ ] docstrings land at C+2 naturally (no special case); verified
[ ] group `defforms`: goldens + idempotency (incl. constant, match-clause, nested)
[ ] comment-matrix spot-checks on def-forms still green
[ ] defform_provisional golden updated; representatives added to corpus
[ ] full `oracles` green over corpus (state count)
[ ] xref + dialyzer clean; warnings_as_errors clean; no regressions
[ ] files changed + one-line rationale; deviations named — or "none"
```

Stop here. Do not start S3 (style-guide conformance).
