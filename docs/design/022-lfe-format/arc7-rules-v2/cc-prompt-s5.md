# 022 · Arc A7 · S5 — export / import (shared reference)

> ⚠️ **DO NOT hand CC this whole file.** S5 is three sub-slices, pre-split to stay
> under the output cap and to keep the oracle carve-out isolated. Hand one at a
> time:
> - `cc-prompt-s5a-export-layout.md` — export always one-per-line + **+1 indent**
>   (under the keyword). Pure layout; **oracles stay green** (no reorder yet).
> - `cc-prompt-s5b-export-sort-oracle.md` — alphabetical sort of export entries +
>   the **oracle carve-out** (token→multiset, AST→export/import-normalized) + tests.
>   Coupled: the sort needs the carve-out to keep oracles green.
> - `cc-prompt-s5c-import.md` *(written after S5a/S5b land)* — import full: nested
>   `from`/`rename` keyword+module-on-line, entries one-per-line at +1, sort entries
>   within each clause, extend the carve-out to import.
>
> Authoritative spec: `formatting-rules.md` §3.5 (decisions locked 2026-06-23:
> **+1 under keyword**; **full export+import sort**; clause order preserved, only
> within-clause entries sorted; oracle carve-out = token-multiset + AST-normalized).
> Do **S5a then S5b then S5c** in order (S5b's tests assume S5a's +1 layout; S5c
> mirrors the S5a renderer and extends S5b's carve-out).

## Invariants (every S5 sub-slice)

Pure engine; `xref`/`dialyzer` standing; `warnings_as_errors`. Idempotent;
comment-preserving; **AST-equivalent except the deliberate export/import entry
reorder (S5b/S5c, via the carve-out)**. Update affected goldens; add permanent
regressions. **Gallery stays paused** (S6 regenerates).

## Why the carve-out (read before S5b)

Sorting entries reorders the `(name arity)` lists. That changes **both**:
- the **token sequence** → the strict token-preservation oracle (`fmt_sig_pairs`
  sequence equality) fails; and
- the **read s-expr** → the AST oracle (`lfe_io:read_string` equality) fails,
  because Lisp lists are ordered.

So the carve-out has two halves (S5b implements for export, S5c extends to import):
1. **Token oracle → multiset.** Compare `lists:sort(fmt_sig_pairs(Src)) =:=
   lists:sort(fmt_sig_pairs(Out))`. Still catches every add / drop / mutate of a
   significant token; becomes order-insensitive.
2. **AST oracle → normalized.** Before comparing, sort export/import entries in
   **both** Orig and Fmted s-exprs (a `normalize_module_decls/1` pass). The AST
   oracle thereby remains the **ordering authority everywhere except** the
   carved-out entries — so a stray reorder anywhere else still fails.

Apply the carve-out at **every** oracle site: `test/r3lfe_properties.erl` and the
`r3lfe_formatter_SUITE` helpers (and any corpus-sweep oracle). The comments oracle
and idempotency oracle are unaffected.

## Per-slice ledger skeleton

```
Arc A7·S5x — <name>
[ ] implements formatting-rules.md §3.5 (cite the clause)
[ ] golden updates listed + rationale; new regressions added
[ ] idempotency + full oracles green over corpus (post-carve-out where applicable)
[ ] xref + dialyzer standing; warnings_as_errors; full suite count stated
[ ] files changed + one-line rationale; deviations named — or "none"
```
