# 022 · Arc A7 — formatting model v2 (knowledge-gated) — shared reference

> ⚠️ **DO NOT hand CC this whole file.** A7 implements the revised ruleset and is
> sub-sliced; hand one at a time. The authoritative spec is
> `docs/design/022-lfe-format/formatting-rules.md` — read it first; this file is
> the arc map.

## What A7 is

Real-world testing (lfe/examples) showed the full-reflow model collapses things
the author deliberately broke and mis-handles forms it can't understand. A7
implements the **knowledge-gated** model from `formatting-rules.md`:

- **Known forms** (special-form table, def-forms, maps, clauses) → canonical;
  the formatter owns the layout and adds/fixes breaks.
- **Unknown forms** (plain calls, record-gen macros, user macros, and all
  quoted/data content) → **break-preserving**; keep the author's break positions,
  only reindent (align-under-first-arg) and wrap > 80.

This **supersedes** the over-collapsing behavior in A3/A4. The **gallery is paused**
and gets regenerated at the end (S6).

## Slice map (work in order; hand one at a time)

- **S1 — cons-dot + dropped-code** *(in flight)*: a standalone `.` is the cons
  operator, not a symbol; fixing it likely also fixes the CRITICAL dropped-code
  bug. Prompt: `arc4-indent/cc-prompt-CRITICAL-dropped-code.md`. Must close first.
- **S2a — CST records author breaks**: instrument the CST to record, per child,
  whether a newline preceded it in source (the author's break positions). No
  printer/behavior change yet. → `cc-prompt-s2a-cst-breaks.md`
- **S2b — printer regimes**: known-vs-unknown regime split (incl. quote/data
  context = always break-preserving); break-preserving renderer for unknown/data
  forms (preserve author break positions, align-under-first-arg, wrap > 80). Known
  forms keep current behavior (S3 refines). → `cc-prompt-s2b-regimes.md`
- **S3 — known-form break rules**: always-break additions (`if`, `progn`,
  `receive`, `try`, `maybe`); clause body rule (break unless single trivial atom);
  lambda structure rule (flat only if single non-structural body).
- **S4 — layout refinements**: signature-on-head-line incl. single-symbol args
  (#2); **def-forms never alone on a line** + narrow A4·S1·fix2 so a trailing
  comment on the *last* signature item stays on the head line (rules §3.1);
  **closing delimiters never de-indent** — a lone close (forced by a preceding
  comment) aligns with that comment, not the form column (rules §3.4a);
  flet/fletrec locals format like defuns (#9); try section symmetry (#10);
  data-list head comment inline with `(` (#3).
- **S5 — exports**: always one-per-line + alphabetical sort (name then arity) +
  the token-preservation oracle carve-out for export/import entry order.
- **S6 — regenerate gallery + full sweep**: re-fill the gallery from the new
  output; run the full property + corpus + e2e sweep; confirm all invariants.

## Invariants (hold across every slice)

Idempotent; comment-preserving; AST-equivalent; token-preserving **except** the
deliberate `export`/`import` sort (S5). Pure engine modules; `xref`/`dialyzer` at
standing level; `warnings_as_errors`. Each slice adds permanent regression tests.

## Per-slice ledger skeleton

```
Arc A7·Sx — <name>
[ ] implements the relevant section(s) of formatting-rules.md (cite which)
[ ] new behavior covered by tests (golden + idempotency + token/comment/AST oracles)
[ ] no regression in prior slices/suites (full ct + property)
[ ] xref + dialyzer at standing level; warnings_as_errors clean
[ ] files changed + one-line rationale; deviations named — or "none"
```
