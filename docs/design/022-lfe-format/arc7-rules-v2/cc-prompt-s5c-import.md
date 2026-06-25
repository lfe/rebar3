# 022 · Arc A7 · S5c — import full (nested layout + entry sort) (CC spec)

> Target: Sonnet 4.6 + `erlang-guidelines`. Final S5 sub-slice. **Implementation
> spec.** **Stop and report** if blocked.
>
> **Output discipline:** Edit in place; don't reprint files; terse prose; run
> tests; report briefly. **Gallery stays paused** (S6 regenerates).

## 0. Orientation

1. `CLAUDE.md`; `erlang-guidelines` (`11-anti-patterns`).
2. `cc-prompt-s5.md` (shared ref); `formatting-rules.md` §3.5.
3. `src/r3lfe_formatter.erl` — the export/import dispatch arm (`IsExportImportHead`
   / `EffIndent` / `SortedBody`, ~1109–1171); `print_try_section/3` (~1374, the
   keyword-alone-contents-below model); `print_export_*` / S5a renderer; the S5b
   sort helpers (`is_export_entry/1`, `sort_export_entries/1`, the commented-entry
   suppression); `close_section/8`; `print_rest_loop/5`.
4. Oracle sites: `normalize_module_decls/1` in **both** `test/r3lfe_properties.erl`
   and `test/r3lfe_formatter_SUITE.erl` (the `import` case is currently a no-op hook).

## 1. The rules (rules §3.5)

LFE import: `(import (from M E…) (rename M P…) (deprecated …) …)`. Each `(from …)`
/`(rename …)`/`(deprecated …)` is a **clause**.

**Layout (one-per-line, +1 under keyword at every level):**
- `import` keyword alone; clauses one-per-line at +1 (already from S5a).
- **`(from M E…)`** and **`(rename M P…)`**: the keyword **and module `M` stay on
  the clause head line**; the entries go one-per-line below at the clause's
  `C+OpenLen` (+1, under the keyword — same rule as export). `deprecated` (and any
  unrecognized clause): one-per-line at +1, no module-on-line special form (render
  generically; just ensure +1).

```
(import
 (from lists
  (all 2)
  (any 2)
  (member 2))
 (rename lists
  ((all 2) every)
  ((any 2) some)
  ((filter 2) find-all)))
```

**Sort (within each clause; clause order preserved):**
- `(from M E…)`: sort the `(name arity)` entries by `{name, arity}` (reuse the S5b
  `is_export_entry`/`sort_export_entries` logic).
- `(rename M P…)`: sort the `((name arity) new-name)` pairs by the **old** name then
  arity (the `(name arity)` inside each pair).
- **Commented-entry suppression** (same as export, §3.5): if any entry/pair in a
  clause has a leading comment, **preserve that clause's order** (no sort).
- The **order of clauses themselves** (`from`/`rename`/`deprecated`) is **never**
  reordered. `deprecated` entries are not sorted.

## 2. Implementation — formatter

1. **Route import clauses through a dedicated renderer.** In the export/import arm,
   when the head is `import`, render the body via a `print_import_body_loop` that
   emits each clause one-per-line at `EffIndent` (+1) via `print_import_clause`
   (instead of the generic `print_rest_loop`). Model the loop on
   `print_try_body_loop`/`print_rest_loop` (same `\n`+lead+indent+trail scaffolding).
2. **`print_import_clause(Clause, C, InData)`** — model on `print_try_section/3`:
   - `(from M E…)` / `(rename M P…)`: keyword **and** `M` on the head line (head at
     `C+OpenLen`, then a space + `M` via `print_node`); entries one-per-line at
     `C+OpenLen` (+1) via `print_rest_loop`; `close_section` at `C+OpenLen`. Before
     emitting entries, **sort** them per §1 (reuse S5b helpers; suppress on any
     leading comment). For `rename`, sort by the inner `(name arity)`.
   - other clause heads (`deprecated`, unknown): render via `print_node(Clause, C,
     InData)` (generic) — just appears one-per-line at +1 from the loop.
   - **Defensive:** non-list / non-symbol-head clause → `print_node`.
3. Keep export behavior from S5a/S5b unchanged. `import` no longer routes its body
   through the plain `print_rest_loop`.

## 3. Implementation — oracle carve-out (extend, don't re-architect)

Populate the **`import` case** of `normalize_module_decls/1` (in **both** the
properties module and the SUITE) so a canonical import compares equal regardless of
within-clause entry order:
- `[import | Clauses]` → for each clause, if `[from, M | Es]` sort the `Es` that are
  `[name, arity]` pairs; if `[rename, M | Ps]` sort the `Ps` that are
  `[[name, arity], new]` by the inner `[name, arity]`; leave `deprecated`/other
  clauses' order; recurse otherwise. Use the existing `norm_list` for improper-list
  safety. The token oracle (multiset) already covers import — no change there.

## 4. Tests — `r3lfe_formatter_SUITE` (new `import_full` group; add to `all/0`)

- **from layout + sort:** unsorted `(from M …)` entries → keyword+module on the
  head line, entries sorted one-per-line at +1 (assert exact).
- **rename layout + sort:** `(rename M …)` pairs sorted by old name/arity, +1.
- **multiple clauses:** `from` then `rename` — clause order preserved, each sorted
  internally.
- **commented entry suppresses sort** within that clause (order preserved).
- **deprecated clause:** one-per-line at +1, not sorted.
- **close alignment** (§3.4a) at +1 for a clause ending in a dangling comment.
- **oracle:** a corpus/synthetic import whose entries the formatter reorders still
  passes the normalized AST oracle; and the AST self-test still catches a non-import
  reorder (extend/confirm `es_ast_oracle_catches_reorder` coverage).
- idempotency on each (assert exact, never `contains()`); full oracles green over
  corpus.

## 5. Constraints

Pure engine; `xref`/`dialyzer` standing; `warnings_as_errors`. Idempotent;
comment-preserving; AST-equivalent modulo the documented export/import entry sort.
Export (S5a/S5b) byte-identical. `print_import_clause` private to the import path
(standalone `from`/`rename`/`deprecated` outside import — rare — must not change;
they are only special-cased as direct children of `import`).

## 6. Ledger

```
Arc A7·S5c — import full (layout + entry sort)
[ ] import body routed through print_import_body_loop → print_import_clause (not generic print_rest_loop)
[ ] from/rename: keyword+module on head line; entries one-per-line at +1; close §3.4a at +1
[ ] from entries sorted by {name,arity}; rename pairs sorted by old {name,arity}; reuse S5b helpers
[ ] commented-entry suppression per clause; clause order preserved; deprecated not sorted
[ ] normalize_module_decls import case populated in BOTH properties + SUITE
[ ] export (S5a/S5b) byte-identical; print_import_clause private to import path
[ ] tests: from, rename, multi-clause, commented-suppress, deprecated, close-align, oracle
[ ] idempotency + full oracles green over corpus; goldens updated + rationale
[ ] xref + dialyzer standing; warnings_as_errors; full project suite count stated
[ ] files changed + one-line rationale; deviations named — or "none"
```

Stop here. Gallery regen + full sweep is S6.
