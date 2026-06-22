# 022 · Arc A7 · S4 — layout refinements (shared reference)

> ⚠️ **DO NOT hand CC this whole file.** S4 is five independent refinements,
> pre-split to stay under the output cap. Hand one at a time:
> - `cc-prompt-s4a-signature.md` — def-forms never alone on a line + narrow
>   A4·S1·fix2 (rules §3.1).
> - `cc-prompt-s4b-close-deindent.md` — closing delimiters never de-indent (§3.4a).
> - `cc-prompt-s4c-flet.md` — flet/fletrec locals format like defuns (#9).
> - `cc-prompt-s4d-try.md` — try section symmetry (#10) + route try case/catch
>   clauses through `render_clause` (deferred from S3b-2).
> - `cc-prompt-s4e-head-comment.md` — data-list head comment inline with `(` (#3).
>
> Authoritative spec: `formatting-rules.md` §3.1, §3.4a, §3.6, §3.7, §3.9. This
> file is the map. Order is flexible; all are independent. Do **S4b early** if
> convenient — it changes lone-close indentation, so doing it first reduces golden
> churn in the others.

## Invariants (every S4 sub-slice)

Idempotent; token-/comment-preserving (raw-lexer); AST-equivalent. Pure engine;
`xref`/`dialyzer` standing; `warnings_as_errors`. Update affected goldens; add
permanent regressions. **Gallery stays paused** (S6 regenerates).

## Per-slice ledger skeleton

```
Arc A7·S4x — <name>
[ ] implements formatting-rules.md §<n> (cite)
[ ] golden updates listed + rationale; new regressions added
[ ] idempotency + full oracles green over corpus
[ ] xref + dialyzer standing; warnings_as_errors; full suite count stated
[ ] files changed + one-line rationale; deviations named — or "none"
```
