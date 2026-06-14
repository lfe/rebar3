# 022 · Arc A4 · S1 · fix2 — head trailing comment + complete the head-line matrix

> Target: Sonnet 4.6 + `erlang-guidelines`. **Scoped correctness fix**, completing
> the comment-on-the-head-line handling that fix1 started. **Stop and report** if
> blocked.
>
> **Output discipline:** Edit in place; don't reprint files; terse prose; run
> tests; report briefly.

## 0. Orientation

1. `CLAUDE.md`; `erlang-guidelines` (`11-anti-patterns.md` first).
2. Your `src/r3lfe_formatter.erl` — `print_classified/12` funcall + specform
   clauses, and `close_section/8`.

## 1. The remaining bug

A line comment runs to end-of-line, so nothing may be emitted on the head line
*after* a comment. fix1 handled trailing comments on the last child (before close)
and on distinguished args. **The head's own trailing comment is still mishandled:**

```
input            current (WRONG)         a1 swallowed by the comment →
(foo ; c         (foo ; c a1              token loss, invalid output
  a1 a2)          ...)
```

funcall emits `[…, HeadIO, HeadTrailIO, " ", A1IO, …]`; specform emits
`[…, HeadTrailIO, DistIO, …]` — both put content after the head's trailing comment
on the same line. Also the specform `Body=[]` branch passes a hardcoded `false` to
`close_section`, so `(progn ; c)` (head trailing, no args) hugs the close →
`(progn ; c)` with the `)` swallowed.

## 2. The fix — head trailing comment forces all args to the body

In **funcall** and **specform** (and therefore defform, which delegates to
specform): compute `HeadHasTrail = r3lfe_format_cst:trailing(Head) =/= []`. When
`HeadHasTrail` is true, **no argument may share the head line** — emit
`(head ; comment` then every argument on its own line at the body indent, via
`print_rest_loop`, exactly like the existing a1-leading-comment / `N=0` fallback.
Then `close_section` receives `HasTrail` from `print_rest_loop` (or, when there are
no args at all, receives `HeadHasTrail`, so the close still breaks onto its own
line). Result:

```
(foo ; c            (progn ; c
  a1                  )
  a2)
```

Concretely:
- **funcall**: change the body-fallback condition from `head_has_leading_comment(A1)`
  to `HeadHasTrail orelse head_has_leading_comment(A1)`. In the no-args (`[]`)
  branch, `close_section` already receives `HeadHasTrail` — keep that.
- **specform**: add `HeadHasTrail` to the fallback that sends everything to the
  body (alongside `N =:= 0` and `any_dist_has_comment`). Fix the `Body=[]` branch
  so it passes `HeadHasTrail` to `close_section` instead of hardcoded `false`
  (covers `(progn ; c)` and any head-trailing-no-args form).
- **list_head** is already safe (rest children always start on a new line; the
  head-only branch already passes `HeadHasTrail`) — leave it, but cover it in tests.

`emit_trailing` for the head's comment still emits `" ; c"` (no newline); the body
arguments then begin with `print_rest_loop`'s leading `"\n"`, so the comment ends
its line correctly.

## 3. Tests — a systematic head-line comment matrix

Add a parametrized/explicit matrix asserting **valid output, token-preservation,
and idempotency** for every vector below, across `funcall`, `specform` N≥1
(`case`), `specform` N=0 (`progn`), `specform` N≥2 (`call`/`:`), `list_head`, and
`defform` (`defun`), each in a form wide enough (or commented enough) to break:

1. head **leading** comment
2. head **trailing** comment — with args, **and** with no args (`(foo ; c)`,
   `(progn ; c)`)
3. first arg **leading** comment
4. arg **trailing** comment — middle child, last child, single arg
5. distinguished arg (N≥2) **leading** and **trailing** comment
6. **dangling** comment before close
7. a combination (head trailing + a body child trailing)

For each: the formatted output must re-lex with all parens/tokens intact (no
swallow), and `format(format(X)) == format(X)`. Add representatives to the
full-corpus fixture. Re-run the full `oracles` group — all green.

## 4. Constraints

Pure module; keep `format/1`'s abstract spec + both `no_underspecs` suppressions;
`xref`+`dialyzer` clean; `warnings_as_errors`; don't modify lexer/CST.

## 5. Ledger (closes A4·S1)

```
Arc A4·S1·fix2 — head-trailing + matrix
[ ] funcall: HeadHasTrail forces all args to body; no content after head comment
[ ] specform: HeadHasTrail forces body layout; Body=[] passes HeadHasTrail to close
[ ] (progn ; c) and (foo ; c) (head trailing, no args) → close on own line, intact
[ ] head-line comment matrix tests (vectors 1–7) across all classes, green
[ ] token-preservation + idempotency asserted for every matrix case
[ ] representatives added to full-corpus fixture; full `oracles` green (state count)
[ ] xref + dialyzer clean; warnings_as_errors clean; no regressions
[ ] files changed + one-line rationale; deviations named — or "none"
```
