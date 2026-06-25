# 022 · Arc A7 · S5a — export one-per-line + +1 indent (CC spec)

> Target: Sonnet 4.6 + `erlang-guidelines`. First S5 sub-slice. **Implementation
> spec.** **Stop and report** if blocked.
>
> **Output discipline:** Edit in place; don't reprint files; terse prose; run
> tests; report briefly. **Gallery stays paused** (S6 regenerates).

## 0. Orientation

1. `CLAUDE.md`; `erlang-guidelines` (`11-anti-patterns`).
2. `cc-prompt-s5.md` (shared ref); `formatting-rules.md` §3.5.
3. `src/r3lfe_formatter.erl` — `specform_table/0` (`"export"`/`"import"` => 0);
   `must_break/1` (~566); the `{specform, 0}` path in `print_classified` (~968);
   `print_try_section/3` (~1374, the structural model: keyword-alone + contents at
   a chosen indent + `close_section`); `print_rest_loop/5`; `close_section/8`;
   `is_force_break_defform/1` (~764, the must_break helpers).

## 1. The rule (rules §3.5 — this slice = LAYOUT only, no sorting)

`export`/`import` always render **one entry per line**, regardless of count
(force-break; not flat-if-fits), with items at **+1, aligned under the keyword**.

Because `Open` is `"("` (length 1), the keyword sits at `C + OpenLen` and the items
align at the **same column** `C + OpenLen` (= C+1) — *not* the body `C+2`.

```
;; input
(defmodule m (export (factorial 1) (ackermann 2) (factorial 2)))
;; output (this slice — order UNCHANGED; sorting is S5b)
(defmodule m
  (export
   (factorial 1)
   (ackermann 2)
   (factorial 2)))
```

(Note `(export` is at column 2 inside the defmodule, so its items are at column 3 —
one past the `(`, under the `e`.)

**This slice does NOT reorder anything** — entries keep source order. Oracles stay
green (pure whitespace change). Sorting + the oracle carve-out is S5b.

## 2. Implementation

1. **`is_export_import_head/1`** — head symbol text `"export"` or `"import"`
   (mirror `is_try_head/1`).
2. **Force-break:** in `must_break/1`, return `true` for an `export`/`import`-headed
   list **that has at least one entry** (an empty `(export)` may stay flat). Add via
   `is_export_import_head` + non-empty children. This makes short export/import
   break too (corr. #4/#12).
3. **Dedicated +1 renderer.** In the `{specform, 0}` path (or via a guard that
   routes export/import before the generic N=0 body loop), when
   `is_export_import_head(Head)`: render keyword alone on the head line at
   `C + OpenLen`, then the entries one per line at `Indent = C + OpenLen` (NOT
   `C+2`) via `print_rest_loop(Items, C+OpenLen, IndStr, true, InData)`, then
   `close_section(Dangling, HasTrail, LastCol, C+OpenLen, IndStr, C, CIndStr,
   Close)`. Model the structure on `print_try_section/3`; the only differences are
   the indent (`C+OpenLen`, not `C+2`) and that there is no keyword distinguished
   arg. Honor head leading/trailing comments as the specform path does.
   - **`import` this slice:** top-level one-per-line at +1 is fine, but its
     `(from M …)`/`(rename M …)` clauses render via the **existing** path (each
     clause is itself a list — leave its internals to S5c). Do **not** yet special-
     case `from`/`rename`. Just ensure the import clauses go one-per-line at +1.
4. Closing delimiters already follow §3.4a (`close_section`) — verify the close
   aligns with the items at `C+OpenLen`, not de-indented.

## 3. Tests — `r3lfe_formatter_SUITE` (new `export_layout` group; add to `all/0`)

- **short export** that previously stayed flat → now one-per-line at +1
  (assert exact output; this is the behavior change — state it).
- **wide export** → one-per-line at +1 (update the existing
  `conf_defmodule_exports_our_canonical` golden from +2 to +1; state the change).
- **single-entry export** → still breaks (one entry on its own line).
- **import** top-level clauses one-per-line at +1 (internals unchanged this slice).
- **close alignment:** an export ending in a dangling comment → `))` aligns with
  items at +1 (§3.4a), not de-indented.
- idempotency on each (assert exact, never `contains()`); **full oracles green**
  over corpus (no reorder, so token/AST oracles unchanged and must still pass).
- list every golden updated +2→+1 with one-line rationale.

## 4. Constraints

Pure engine; `xref`/`dialyzer` standing; `warnings_as_errors`. Idempotent;
token-/comment-preserving; AST-equivalent (no reorder this slice). Non-export/import
specform N=0 forms (progn, case-with-no-args, etc.) render **byte-identically** to
before — the +1 indent applies **only** to export/import.

## 5. Ledger

```
Arc A7·S5a — export/import one-per-line + +1 indent
[ ] is_export_import_head/1 added
[ ] must_break: export/import with >=1 entry force-breaks (short ones too)
[ ] dedicated renderer: keyword alone; items at C+OpenLen (+1); close §3.4a at +1
[ ] +1 applies ONLY to export/import; other specform-0 forms byte-identical
[ ] import top-level clauses one-per-line at +1 (from/rename internals deferred to S5c)
[ ] tests: short, wide(+golden +2→+1), single-entry, import, close-alignment, idempotency
[ ] full oracles green over corpus (no reorder); goldens updated + rationale
[ ] xref + dialyzer standing; warnings_as_errors; full project suite count stated
[ ] files changed + one-line rationale; deviations named — or "none"
```

Stop here. Sorting + oracle carve-out is S5b.
