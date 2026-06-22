# 022 · Arc A7 · S4b — closing delimiters never de-indent (CC spec)

> Target: Sonnet 4.6 + `erlang-guidelines`. S4 sub-slice. **Implementation spec.**
> **Stop and report** if blocked.
>
> **Output discipline:** Edit in place; don't reprint files; terse prose; run
> tests; report briefly.

## 0. Orientation

1. `CLAUDE.md`; `erlang-guidelines` (`11-anti-patterns`).
2. `cc-prompt-s4.md` (shared ref); `formatting-rules.md` §3.4a.
3. `src/r3lfe_formatter.erl` — `close_section/8`.

## 1. The bug (rules §3.4a)

When a close is forced onto its own line (the preceding line is a comment — a `)`
can't follow `; …`), it currently de-indents to the form's **open column** `C`
instead of staying at the **content's** indent. Duncan's example (a `defmodule`
`export` whose last entries are comments):

```
;; WRONG — close de-indented to the form column          ;; RIGHT
(defmodule m                                             (defmodule m
  (export                                                  (export
    (new 0)                                                  (new 0)
    ;; XXX broken; see #397                                  ;; XXX broken; see #397
  ))                                                         ))
```

i.e. the lone close should sit **at the same indent as the preceding
comment/content line**, never shallower.

## 2. The fix

In `close_section/8`, the branch that emits the close on its own line (when
`Dangling =/= []` or the last child had a trailing comment) currently places the
close at `CIndStr` (the form's open column `C`). Change it to place the close at
**`IndStr`** — the same indent `emit_dangling/2` already uses for the dangling
comments (the content/body indent). So the comments and the closing paren line up.

- The **hug** branch (no dangling, no trailing comment) is unchanged — closes
  still hug the last token wherever possible (the Lisp norm).
- This applies to **both** triggers: a dangling comment before the close, and a
  trailing comment on the last child that forced the close to its own line.
- The rule is indent-agnostic: whatever `IndStr` the caller passed (specform body
  `C+2`, funcall align column, export items, etc.), the close aligns to it. (So it
  composes with the still-open export-indent decision in S5 — close follows the
  items wherever they end up.)

## 3. Tests — `r3lfe_formatter_SUITE`

- `defmodule` export ending in a dangling comment → the `))` aligns with the
  export items/comment, not de-indented to the defmodule column.
- a `progn`/`let` body whose last child has a trailing comment → the close is at
  the body indent, not the form column.
- a funcall/list with a dangling comment before close → close at the content
  (align) indent.
- confirm normal closes (no comment) still hug.
- idempotency on each (a close-on-its-own-line at content indent must re-parse and
  re-emit identically); full oracles over corpus; update affected goldens.

## 4. Constraints

Pure engine; `xref`/`dialyzer` standing; `warnings_as_errors`. Idempotent;
token-/comment-preserving; AST-equivalent.

## 5. Ledger

```
Arc A7·S4b — close never de-indents
[ ] close_section lone-close branch uses IndStr (content indent), not CIndStr (C)
[ ] applies to both dangling-comment and last-child-trailing-comment triggers
[ ] hug branch unchanged (no-comment closes still hug)
[ ] tests: export-with-dangling-comment, body-trailing-comment, funcall-dangling
[ ] idempotency + full oracles green; goldens updated
[ ] xref + dialyzer standing; warnings_as_errors; full project suite count stated
[ ] files changed + one-line rationale; deviations named — or "none"
```

Stop here. flet/fletrec-as-defun is S4c.
