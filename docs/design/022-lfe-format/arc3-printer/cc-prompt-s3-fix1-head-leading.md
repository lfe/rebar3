# 022 · Arc A3 · S3 · fix1 — head-leading-comment idempotency

> Target: Sonnet 4.6 + `erlang-guidelines`. **Scoped fix** to A3·S3. The rest of
> A3 is correct and closed-pending-this. Touch only what's described. **Stop and
> report** if blocked.
>
> **Output discipline:** Edit in place; don't reprint files; terse prose; run
> tests; report briefly.

## 0. Orientation

1. `CLAUDE.md`; `erlang-guidelines` (`11-anti-patterns.md` first).
2. `arc3-printer/cc-prompt.md` shared ref — §3.2 (break rule), §5, §6 (oracles).
3. Your `src/r3lfe_formatter.erl` — functions `print_broken_container/2` and
   `emit_head_leading/2`.

## 1. The defect (idempotency violation)

A comment that sits **between `(` and the first element** (the head child's
`leading`) is currently hoisted *above* the opening paren by `emit_head_leading`.
On reparse that comment re-attaches to the **container** (not the head), which
flips `has_internal_trivia` from true→false and the form from broken→flat. Not a
fixed point:

```
input :  (;; c            pass 1 :  ;; c           pass 2 :  ;; c
          alpha beta)                (alpha           →       (alpha beta)
                                       beta)
```

This violates the idempotency oracle. The corpus didn't exercise it (a comment
right after `(` is rare), so it shipped silently.

## 2. The fix

Make the head's leading **comment** stay *inside* the parens so it re-attaches to
the head on reparse. Rule:

- **If the head child has a leading comment** (a `{comment, _}` item in its
  `leading`; blanks alone do not count): the opening delimiter stands **alone** on
  the opener line, and **all** children — head included — are emitted on their own
  lines at `Indent` via the normal child path (so each child's leading is emitted
  inside, before it). The closer hugs the last child (or its own line at `C` when
  there is dangling, exactly as today). Result for the example:

  ```
  (
    ;; c
    alpha
    beta)
  ```

- **If the head child has no leading comment**: behavior is unchanged — head on
  the opener line (head-leading blanks dropped, as today).

Concretely: in `print_broken_container/2`, branch on
`head_has_leading_comment(Head)`. When true, emit `Open` then run the existing
`print_rest_loop/4` over `[Head | RestChildren]` at `Indent` with `IsFirst=true`
(this emits each child's leading inside, drops a leading blank, and prefixes each
with `"\n" ++ IndentStr`), then `close_section/7`. When false, keep the current
head-on-opener-line path. `emit_head_leading/2` is now only reached on the
false branch (where it still drops blanks); you may simplify/remove it if the
false branch no longer needs comment handling (head with no leading comment ⇒
nothing to emit). Verify this shape is a fixed point before reporting.

(A4 will rework head/indent handling and may refine this; the requirement here is
only that idempotency holds now.)

## 3. Tests

- **Add the exact repro** as both an idempotency assertion and a golden:
  `(;; c\nalpha beta)` ⇒
  ```
  (
    ;; c
    alpha
    beta)
  ```
  and `format(format(X)) == format(X)`.
- **Add the head-leading case to the corpus fixture** used by the full-corpus
  oracles so it's permanently covered.
- Also cover: single child with a head-leading comment (`(;; c\nalpha)`); a
  head-leading comment plus a normal nested child; head-leading **blank only**
  (must stay on opener line — blank dropped, unchanged).
- Re-run the full `oracles` group (idempotency, token-, comment-preservation,
  AST-equivalence) over the full corpus — all green.

## 4. Constraints

Pure module; keep `format/1`'s abstract `{ok, iolist()} | {error,_}` spec and the
`-dialyzer({no_underspecs, format/1})`; `xref`+`dialyzer` clean;
`warnings_as_errors`; don't modify lexer/CST.

## 5. Ledger (closes A3 once green)

```
Arc A3·S3·fix1 — head-leading idempotency
[ ] print_broken_container branches on head_has_leading_comment; opener stands
    alone + all children at Indent when a head-leading comment is present
[ ] head with no leading comment unchanged (head on opener line)
[ ] repro is a fixed point; added as idempotency + golden test
[ ] head-leading case added to the full-corpus fixture
[ ] single-child + blank-only-head cases tested
[ ] full `oracles` group green over the corpus (state count)
[ ] xref + dialyzer clean; warnings_as_errors clean; no regressions
[ ] files changed + one-line rationale; deviations named — or "none"
```
