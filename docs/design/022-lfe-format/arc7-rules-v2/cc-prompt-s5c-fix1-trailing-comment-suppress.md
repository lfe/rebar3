# 022 · Arc A7 · S5c·fix1 — suppress export/import sort on any entry comment (CC spec)

> Target: Sonnet 4.6 + `erlang-guidelines`. Small follow-up to S5b/S5c.
> **Implementation spec.** **Stop and report** if blocked.
>
> **Output discipline:** Edit in place; don't reprint files; terse prose; run
> tests; report briefly. **Gallery stays paused** (S6 regenerates).

## 0. Orientation

1. `CLAUDE.md`; `erlang-guidelines`.
2. `formatting-rules.md` §3.5 (commented-entry suppression).
3. `src/r3lfe_formatter.erl` — the export `SortedBody` condition (~1208–1216);
   `sort_import_entries/2` (~805, the `"from"` and `"rename"` clauses);
   `has_comment_leading/1`.

## 1. The gap

The sort-suppression that protects developer-annotated entries currently checks
**leading comments only** (`has_comment_leading`). An entry with a **trailing**
comment and no leading comment is therefore still sorted, and the sort moves the
entry **with its trailing comment**, reordering the comment stream — violating
comment-preservation (and tripping the comment oracle):

```
(export (start_link 0)  ; API
        (init 1))       ; gen_server callback
```

Leading → suppress; trailing → sort-and-reorder is **inconsistent**. The intended
rule (§3.5) is: any developer comment on an entry marks intentional structure →
**don't sort**.

## 2. The fix

Define one predicate, e.g.:

```erlang
entry_has_comment(E) ->
    has_comment_leading(r3lfe_format_cst:leading(E))
    orelse r3lfe_format_cst:trailing(E) =/= [].
```

Use it in **all three** suppression checks (replacing the current leading-only
`lists:any`):
1. the export `SortedBody` condition (sort export only when **no** entry
   `entry_has_comment`);
2. `sort_import_entries("from", …)`;
3. `sort_import_entries("rename", …)`.

(Dangling comments live on the container, not on an entry, and don't move under an
entry sort — no change needed for those.) No oracle changes: the AST oracle already
normalizes both sides; this fix keeps the **comment** oracle strict by not moving
comments.

## 3. Tests — extend `export_sort` and `import_full`

- **export, trailing comment on an entry** (unsorted) → order **preserved**
  (assert exact; the comment stays put). e.g.
  `(export (z 0) ; zc\n (a 0))` → unchanged order.
- **import `from`/`rename`, trailing comment on an entry** → clause order preserved.
- confirm an **uncommented** export/import still sorts (existing tests) — the fix
  must only add suppression, not disable sorting generally.
- idempotency on each (assert exact); full oracles green over corpus (the comment
  oracle in particular must stay green).

## 4. Constraints

Pure engine; `xref`/`dialyzer` standing; `warnings_as_errors`. Idempotent;
comment-/token-preserving; AST-equivalent modulo the (now further-narrowed) entry
sort. Uncommented export/import sorting unchanged.

## 5. Ledger

```
Arc A7·S5c·fix1 — suppress sort on any entry comment
[ ] entry_has_comment predicate (leading OR trailing) used in all 3 suppression sites
[ ] export + import-from + import-rename suppression consistent
[ ] tests: export trailing-comment preserved; import trailing-comment preserved; uncommented still sorts
[ ] idempotency + full oracles green over corpus (comment oracle stays strict)
[ ] xref + dialyzer standing; warnings_as_errors; full project suite count stated
[ ] files changed + one-line rationale; deviations named — or "none"
```

Stop here. Gallery regen + full sweep is S6.
