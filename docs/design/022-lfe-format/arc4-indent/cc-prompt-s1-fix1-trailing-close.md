# 022 · Arc A4 · S1 · fix1 — comment must end the line (close + distinguished args)

> Target: Sonnet 4.6 + `erlang-guidelines`. **Scoped correctness fix.** This
> repairs a serious bug surfaced during the A4·S1 audit that actually predates A4
> (it has existed since A3·S3). **Stop and report** if blocked.
>
> **Output discipline:** Edit in place; don't reprint files; terse prose; run
> tests; report briefly.

## 0. Orientation

1. `CLAUDE.md`; `erlang-guidelines` (`11-anti-patterns.md` first).
2. Your `src/r3lfe_formatter.erl` — functions `close_section/7`,
   `print_rest_loop/4`, `print_distinguished/2`, and the `print_classified/12`
   clauses.

## 1. The bug (formatter can emit invalid code)

A line comment runs to end-of-line, so **anything emitted on the same line after
a comment is swallowed by it.** Two places violate this:

**(a) Close hugs a last child that has a trailing comment** — common and serious:

```
input                current (WRONG)        the ) is now commented out →
(progn               (progn                 unbalanced, invalid LFE, lost token
  a                    a
  b ; note             b ; note)
  )
```

`close_section/7` only breaks the close onto its own line when there is
*dangling* trivia. It must also break when the **last emitted child carries a
trailing comment**. This affects every class (funcall / specform / list_head /
defform) because they all end by hugging the close to the last child.

**(b) `print_distinguished/2` puts content after a distinguished arg's comment on
the same line** — narrow (N≥2 specforms `:` `call` `do` `prog2` `defflavor`): a
leading or trailing comment on a non-last distinguished arg swallows the next
arg, or emits a mid-line newline at column 0 (non-idempotent).

## 2. The fix

**Invariant to enforce:** a comment is always the last thing on its line; the
next emitted token (including a closing delimiter) starts on a fresh line.

**(a) Close after a trailing comment.** Make the close go on its own line at
column `C` whenever the content before it ends in a trailing comment — exactly as
it already does for dangling. Suggested approach: have `print_rest_loop/4` (and
the single-child / distinguished paths) report whether the **last child emitted a
trailing comment**, and pass that to `close_section/7`; when true (or when
dangling is non-empty), emit `"\n" ++ CIndStr ++ Close` instead of hugging. Pick
a clean signature; keep it total. Result:

```
(progn
  a
  b ; note
  )
```

(Close on its own line at the form's column. Verify this is a fixed point: on
reparse, `; note` is still `b`'s trailing and the close still breaks.)

**(b) Distinguished args with comments.** Extend the existing head-leading-comment
guard: if **any** distinguished arg (args `1..N` that would share the head line)
has a leading **or** trailing comment, fall back to body layout — emit **all**
`RestChildren` at `C+2` via `print_rest_loop` (the safe path), with no args on the
head line. This subsumes the current `D1HasComment` check.

## 3. Tests — extend `r3lfe_formatter_SUITE`

Add regression + idempotency cases (and add representative ones to the
full-corpus fixture so they're permanently covered):
- trailing comment on the **last child** of: a `progn` (N=0 body), a `case`
  (clause body), a `let` body, a **funcall** (last aligned arg), a **list_head**
  list. Assert the close lands on its **own line** and the output **re-lexes with
  the closing paren intact** (token-preservation) and is idempotent.
- trailing comment on a non-last distinguished arg of `:` and `call` (N=2):
  assert fallback to body layout, no token swallowed, idempotent.
- leading comment on the 2nd distinguished arg of an N≥2 form: same.
- Re-run the full `oracles` group (idempotency, token-, comment-preservation,
  AST-equivalence) over the corpus — all green.

## 4. Constraints

Pure module; keep `format/1`'s abstract spec + both `no_underspecs` suppressions;
`xref`+`dialyzer` clean; `warnings_as_errors`; don't modify lexer/CST.

## 5. Ledger (closes A4·S1)

```
Arc A4·S1·fix1 — comment-ends-line
[ ] close_section breaks the close onto its own line when the last child has a
    trailing comment (as well as for dangling)
[ ] print_distinguished falls back to body layout if ANY distinguished arg has a
    leading OR trailing comment
[ ] regression tests: last-child trailing comment for progn/case/let/funcall/
    list_head — close on own line, paren intact, idempotent
[ ] regression tests: N≥2 distinguished-arg comment (: and call), no swallow
[ ] representative cases added to the full-corpus fixture
[ ] full `oracles` group green over corpus (state count)
[ ] xref + dialyzer clean; warnings_as_errors clean; no regressions
[ ] files changed + one-line rationale; deviations named — or "none"
```
