# 022 · Arc A7 · S3 — known-form break rules (shared reference)

> ⚠️ **DO NOT hand CC this whole file.** S3 is three distinct rule sets; to stay
> under the output cap it's pre-split. Hand one at a time:
> - `cc-prompt-s3a-always-break.md` — add `if`/`progn`/`receive`/`try`/`maybe` to
>   the always-break set.
> - `cc-prompt-s3b-1-clauses-case-cond.md` — define the clause renderer (break
>   unless trivial) + wire `case` and `cond`.
> - `cc-prompt-s3b-2-clauses-rest.md` — wire the clause renderer into
>   `match-lambda`, `defun`/`defmacro` match-form, `receive`, `try`. *(JIT.)*
> - `cc-prompt-s3c-lambda.md` — lambda structure rule (flat only if single
>   non-structural body). *(JIT after S3b.)*
>
> Authoritative spec: `formatting-rules.md` §2 (always-break list), §3.2 (lambda),
> §3.3 (clauses). This file is the map.

## Context

S2 established the regime split (canonical vs break-preserving) and the
break-preserving renderer. S3 refines the **canonical** (known-form) layout per
the v2 rules:
- §2 always-break list gains `if`, `progn`, `receive`, `try`, `maybe` (joining the
  existing `let`/`let*`/`case`/`cond`/maps/def-with-body).
- §3.3 clauses: a case/match clause breaks its body below the pattern unless the
  body is a single trivial datum (atom/number/quoted-atom).
- §3.2 lambda: flat only if its body is a single non-structural form that fits;
  otherwise break.

## Invariants (every S3 sub-slice)

Idempotent; token-/comment-preserving (token = raw-lexer); AST-equivalent. Pure
engine; `xref`/`dialyzer` standing; `warnings_as_errors`. Update affected goldens;
add permanent regressions. **Gallery stays paused** (S6 regenerates).

## Per-slice ledger skeleton

```
Arc A7·S3x — <name>
[ ] implements formatting-rules.md §<n> (cite)
[ ] golden updates listed + rationale; new regressions added
[ ] full oracles green over corpus (idempotency, token, comment, AST)
[ ] xref + dialyzer standing; warnings_as_errors; full suite count stated
[ ] files changed + one-line rationale; deviations named — or "none"
```
