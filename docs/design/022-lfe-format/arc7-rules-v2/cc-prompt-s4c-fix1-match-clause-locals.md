# 022 · Arc A7 · S4c·fix1 — match-clause local fns route through render_clause (CC spec)

> Target: Sonnet 4.6 + `erlang-guidelines`. Small follow-up to S4c.
> **Implementation spec.** **Stop and report** if blocked.
>
> **Output discipline:** Edit in place; don't reprint files; terse prose; run
> tests; report briefly. **Gallery stays paused** (S6 regenerates).

## 0. Orientation

1. `CLAUDE.md`; `erlang-guidelines` (`11-anti-patterns`).
2. `formatting-rules.md` §3.6 (local fns format like defuns).
3. `src/r3lfe_formatter.erl` — `print_local_fn_binding/3` (~1200);
   `local_fn_n/1` (~621); `print_clause_loop/5` (~1282); `render_clause/3` (~724);
   `all_clauses/1` (~690); `close_section/8` (~1132); the specform body-routing
   decision (~1044–1052) — **reference only, do not change it**.

## 1. The gap (from S4c)

S4c renders flet/fletrec locals defun-like, but a **match-clause** local fn
(`local_fn_n` returns 0) routes its body through `print_rest_loop`, treating each
clause as a generic list element. A clause that **fits flat** renders fine; a
**wide** clause breaks as a plain list (align-under-first) instead of true clause
layout (body below the pattern, `render_clause`). A real `defun` match-clause
routes through `print_clause_loop`/`render_clause` — local fns should match.

```
;; want (wide match-clause local fn), clauses via render_clause:
(flet ((classify
         ((x) (when (> x 0))                 ;; pattern + guard on the clause line
          (some-very-long-positive-branch x))  ;; body below, render_clause layout
         ((_) 'other)))
  …body…)
```

## 2. The fix (localized — do NOT touch the shared specform renderer)

Deciding clause-routing structurally inside the shared specform renderer would
**misfire**: e.g. a `progn` body `(foo) (bar)` satisfies `all_clauses/1`, so a
blanket "N=0 + all_clauses → clause loop" rule would wrongly clause-route progn and
other N=0 specforms. Keep the change **inside `print_local_fn_binding/3`** so it
applies only to local fns.

In `print_local_fn_binding/3`, branch on N:

- **N >= 1** (signature form) → unchanged: delegate to
  `print_classified({specform, N}, Name, RestChildren, …)` exactly as now.
- **N == 0** (match-clause form):
  - if `all_clauses(RestChildren)` is **true** → render the match-clause form
    directly: `Open` + name on the head line, then `print_clause_loop(RestChildren,
    Indent, IndentStr, true, InData)` at `Indent = C+2`, then `close_section/8`
    with the loop's `HasTrail` and content indent (same call shape S4c already
    uses — `Indent`/`IndentStr` for content, `C`/`CIndStr` for the form column).
    This mirrors how `match-lambda` (N=0) routes clauses via
    `is_clause_specform_head` → `print_clause_loop`.
  - else (not all clauses — defensive) → delegate to `{specform, 0}` as now.

Reuse `render_clause`/`print_clause_loop` and `close_section` — **no new clause
layout code, no duplicated close/dangling logic**. The head-alone + clause-loop
shape parallels the existing specform N=0 body path; only the loop function differs
(`print_clause_loop` instead of `print_rest_loop`).

Watch the head-leading-comment and head-trailing-comment handling: keep parity with
the specform path (emit head leading via `CIndStr`, head trailing ends the head
line). If matching that is fiddly, the cleanest route may be a dedicated
`{local_fn, N}` head-class clause in `print_classified` that is identical to
`{specform, N}` except the body-routing predicate also fires for
`N =:= 0 andalso all_clauses(Body)` — your call, but **the shared `{specform, N}`
clause must remain unchanged** so progn et al. are unaffected.

## 3. Tests — `r3lfe_formatter_SUITE` (extend `flet_locals`)

- **wide match-clause local fn** → clauses via `render_clause`: pattern (and any
  `(when …)` guard) on the clause line, body below at the clause's align column.
  Assert exact output.
- **narrow match-clause local fn** (existing `flet_match_clause_local`) still
  renders clauses flat — unchanged.
- **guarded clause** in a local fn: `((n) (when (> n 0)) body)` → guard stays on
  the pattern line (A4·S3d), body below.
- **regression guard:** a `progn` (and a `let` body) whose forms are all
  parenthesized calls is **not** clause-routed — confirm unchanged output.
- idempotency on each (assert exact, never `contains()`); full oracles over corpus.
- update affected goldens with one-line rationale.

## 4. Constraints

Pure engine; `xref`/`dialyzer` standing; `warnings_as_errors`. Idempotent;
token-/comment-preserving; AST-equivalent. The shared `{specform, N}` renderer's
behavior for non-local-fn forms (progn, case, defun, …) must be **byte-identical**
to before.

## 5. Ledger

```
Arc A7·S4c·fix1 — match-clause local fns via render_clause
[ ] change localized to print_local_fn_binding (or a new {local_fn,N} class); shared {specform,N} unchanged
[ ] N=0 + all_clauses → print_clause_loop/render_clause; else specform-0 fallback
[ ] reuse close_section + clause helpers (no duplicated close/dangling/layout)
[ ] tests: wide match-clause local, narrow unchanged, guarded clause, progn/let NOT clause-routed
[ ] idempotency + full oracles green over corpus; goldens updated + rationale
[ ] xref + dialyzer standing; warnings_as_errors; full project suite count stated
[ ] files changed + one-line rationale; deviations named — or "none"
```

Stop here. try symmetry + try clauses is S4d.
