# 022 · Arc A7 · S4d — try full symmetry + clause routing (CC spec)

> Target: Sonnet 4.6 + `erlang-guidelines`. S4 sub-slice. **Implementation spec.**
> **Stop and report** if blocked.
>
> **Output discipline:** Edit in place; don't reprint files; terse prose; run
> tests; report briefly. **Gallery stays paused** (S6 regenerates).

## 0. Orientation

1. `CLAUDE.md`; `erlang-guidelines` (`11-anti-patterns`).
2. `cc-prompt-s4.md` (shared ref); `formatting-rules.md` §3.7.
3. `src/r3lfe_formatter.erl` —
   `specform_table/0` (~860; `"try" => 1`, `"catch" => 0`, `"after" => 1`);
   the specform body-routing decision (~1044–1052, the `IsReceiveHead`/`IsCaseHead`
   case);
   `print_receive_body_loop/5` (~1317, the closest precedent: per-child render with
   render_clause + after-section exception);
   `print_local_fn_binding/3` (~1200, precedent for forced keyword-alone container
   rendering);
   `print_clause_loop/5` (~1296), `render_clause/3` (~724), `print_rest_loop/5`,
   `close_section/8`, `is_after_section/1` (~679).

## 1. Target shape (rules §3.7 — Duncan's decision: FULL SYMMETRY)

LFE try grammar: `(try BODY-EXPR (case CLAUSE…)? (catch CLAUSE…)? (after BODY…)?)`.
The first child after `try` is the protected body expr; the trailing
`case`/`catch`/`after` lists are **sections**. (`case`/`catch` sections hold
*clauses* directly — no test expr; `after` holds body forms.)

`try` **always breaks** (already in `is_always_break_head`). When it breaks:
`try` keyword **alone**; the body expr and every section at **+2**; each section
**keyword alone** on its line with its contents below at **+4**; case/catch clauses
rendered via `render_clause`.

```
;; input
(try (foo x) (case ((tuple 'ok v) v)) (catch ((tuple 'error reason) (error reason))) (after (cleanup)))
;; output
(try
  (foo x)
  (case
    ((tuple 'ok v) v))
  (catch
    ((tuple 'error reason)
     (error reason)))
  (after
    (cleanup)))
```

Note: even small sections break (keyword alone) — symmetry is the rule, not
flat-if-fits. The body expr always goes below `try` at +2 (no longer on the head
line). `(after …)` contents are body forms (`print_rest_loop`), not clauses.

## 2. Implementation

1. **`specform_table`:** change `"try"` from `1` to `0` so the body expr is no
   longer pinned to the head line (it becomes the first body child at +2). Leave
   `catch`/`after` table entries unchanged — they are not rendered via the table
   when inside a try (see step 4); standalone `catch`/`case`/`after` elsewhere are
   unaffected by this slice.
2. **`is_try_head/1`** — head symbol text `"try"` (mirror `is_receive_head/1`).
3. **Body-routing** (the `~1049` `case {IsReceiveHead, …}`): add an `IsTryHead`
   arm **before** the receive/clause arms → `print_try_body_loop(Body, Indent,
   IndentStr, true, InData)`. (`Body` = all children after the `try` head =
   `[BodyExpr | Sections]`.)
4. **`print_try_body_loop/5`** — model on `print_receive_body_loop/5` (same
   `\n`+`emit_child_leading`+`IndentStr`+child+`emit_trailing` scaffolding, same
   `HasTrail` propagation). The per-child render differs by position:
   - **`IsFirst =:= true`** (the body expr) → `print_node(Child, Indent, InData)`
     (render normally — may be flat or break).
   - **`IsFirst =:= false`** (a section) → `print_try_section(Child, Indent, InData)`.
5. **`print_try_section/3`** — render a `(case/catch/after …)` container
   keyword-alone-contents-below (model on `print_local_fn_binding/3`'s geometry +
   `close_section` use):
   - geometry: `Open`/`Close`/`Dangling` from the section node, `Indent = C+2`,
     `IndentStr`, `CIndStr`.
   - `[SectionHead | Contents] = children`. Emit `SectionHead` alone on the section
     line (head at `C + OpenLen`, honoring head leading/trailing comments as the
     specform path does).
   - Contents at `Indent` (= C+2):
     - `is_after_section(Section)` → `print_rest_loop(Contents, Indent, IndentStr,
       true, InData)` (body forms).
     - else (case/catch) → `print_clause_loop(Contents, Indent, IndentStr, true,
       InData)` (clauses via `render_clause`).
   - `close_section(Dangling, HasTrail [orelse HeadHasTrail], LastCol, Indent,
     IndentStr, C, CIndStr, Close)`.
   - **Defensive:** if `Section` is not a list / has no symbol head, fall back to
     `print_node(Section, C, InData)`.

`print_try_section` must be reachable **only** from `print_try_body_loop` — do not
change how standalone `case`/`catch`/`after` render elsewhere.

## 3. Tests — `r3lfe_formatter_SUITE`

- **Update `ab_try_small`** (and any other try golden): the old
  `(try (foo)\n  (catch (_ 'err)))` becomes the full-symmetry shape
  (`try` alone, `(foo)` at +2, `(catch` alone, `(_ 'err)` at +4). State the change.
- new `try_full_symmetry`: the §1 example → exact output above.
- **wide catch clause** → `render_clause` (pattern line, body below).
- **case + catch + after** all present → all three sections symmetric.
- **body is a `(progn …)` / `(let …)`** → body breaks normally at +2 under `try`.
- **after section** contents render as body (`print_rest_loop`), not clauses.
- idempotency on each (assert exact, never `contains()`); full oracles
  (idempotency, token/comment-preservation raw-lexer, AST-equivalence) over corpus.

## 4. Constraints

Pure engine; `xref`/`dialyzer` standing; `warnings_as_errors`. Idempotent;
token-/comment-preserving; AST-equivalent. Standalone `case`/`catch`/`after`
(outside a try) render **byte-identically** to before. `print_try_section` is
private to the try path.

## 5. Ledger

```
Arc A7·S4d — try full symmetry + clause routing (rules §3.7)
[ ] specform_table "try" 1→0; is_try_head/1 added
[ ] body-routing: IsTryHead → print_try_body_loop (before receive/clause arms)
[ ] print_try_body_loop: first child = body (print_node); rest = sections (print_try_section)
[ ] print_try_section: keyword alone; case/catch → print_clause_loop; after → print_rest_loop; +4 contents
[ ] reuse close_section + clause/rest loops (no duplicated close/dangling/layout)
[ ] standalone case/catch/after unchanged (byte-identical); print_try_section private to try
[ ] tests: ab_try_small updated, full_symmetry, wide-catch-clause, case+catch+after, progn-body, after-body
[ ] idempotency + full oracles green over corpus; goldens updated + rationale
[ ] xref + dialyzer standing; warnings_as_errors; full project suite count stated
[ ] files changed + one-line rationale; deviations named — or "none"
```

Stop here. data-list head comment is S4e.
