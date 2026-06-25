# 022 · Arc A7 · S5b — export sort + oracle carve-out (CC spec)

> Target: Sonnet 4.6 + `erlang-guidelines`. Second S5 sub-slice. **Implementation
> spec.** **Stop and report** if blocked.
>
> **Output discipline:** Edit in place; don't reprint files; terse prose; run
> tests; report briefly. **Gallery stays paused** (S6 regenerates).

## 0. Orientation

1. `CLAUDE.md`; `erlang-guidelines` (`11-anti-patterns`).
2. `cc-prompt-s5.md` (shared ref — **read the "Why the carve-out" section**);
   `formatting-rules.md` §3.5.
3. `src/r3lfe_formatter.erl` — the S5a export renderer (where items are emitted).
4. Oracle sites: `test/r3lfe_properties.erl` (`fmt_oracle_tokens`, `fmt_sig_pairs`,
   `fmt_oracle_ast`); `test/r3lfe_formatter_SUITE.erl` (its raw-lexer
   token-preservation helpers — `sig_pairs`/`sweep_sig_pairs` — and any AST/corpus
   oracle). **Find and update every oracle site.**

## 1. The rule (rules §3.5)

`export` entries are **sorted alphabetically by name, then arity** — the one
deliberate, semantics-preserving token reorder. (Import sorting is S5c.)

```
;; input
(defmodule m (export (factorial 1) (ackermann 2) (factorial 2)))
;; output (S5b: sorted)
(defmodule m
  (export
   (ackermann 2)
   (factorial 1)
   (factorial 2)))
```

- Sort key: name text (atom), then arity (integer). `(factorial 1)` < `(factorial 2)`.
- **Only sort when every entry is a `(name arity)` pair** (a 2-element list:
  symbol + number). If any entry is not — e.g. `(export all)`, a macro, a comment-
  bearing oddity — **leave the entry order unchanged** (no partial sort).
- Sorting is a stable reorder of the item CST nodes before rendering; comments
  attached to an entry travel with it.

## 2. Implementation — formatter

In the S5a export renderer, before emitting the items: if
`all_export_entries(Items)` (each is a 2-child list of symbol + number), reorder
`Items` by `{name_text, arity_int}` using a **stable** sort; else keep as-is. Render
the (possibly reordered) list exactly as S5a does. Idempotent: sorting an
already-sorted list is a no-op.

## 3. Implementation — oracle carve-out (both halves; see S5 map)

**(a) Token oracle → multiset** (every token-preservation site). Change the
comparison from sequence equality to multiset equality:
`lists:sort(SigPairs(Src)) =:= lists:sort(SigPairs(Out))`. This still detects any
significant-token add / drop / mutation; it becomes order-insensitive. Apply in
`r3lfe_properties.erl` **and** the `SUITE` helpers (and corpus sweep).

**(b) AST oracle → normalized.** Add `normalize_module_decls/1` over
`lfe_io:read_string` terms: recursively walk; for any `(export E…)` form, sort its
`(name arity)` entries by `{name, arity}` (leave non-pair entries' order alone);
recurse into all sub-forms. (Write it to also handle `import` now — a no-op until
S5c populates the import cases — or leave a clearly-marked TODO for S5c.) Change
`fmt_oracle_ast` to compare `normalize_module_decls(Orig) =:=
normalize_module_decls(Fmted)`. The AST oracle thus stays the **ordering
authority everywhere except** export entries.

## 4. Tests — `r3lfe_formatter_SUITE` (new `export_sort` group; add to `all/0`)

- **unsorted export → sorted output** (the §1 example; assert exact).
- **already-sorted export** → unchanged (idempotency).
- **same name, different arity** → ordered by arity (`(f 1)` before `(f 2)`).
- **`(export all)`** and a **mixed** export (one non-pair entry) → **order
  preserved**, not sorted.
- **comment travels with its entry:** an entry with a leading comment sorts with
  the entry (comment stays attached).
- **carve-out soundness (oracle self-tests):**
  - the multiset token oracle still **fails** on a synthetic dropped/mutated token
    (prove it didn't become a no-op);
  - the normalized AST oracle still **fails** on a synthetic non-export reorder
    (e.g. two swapped defuns) — proving ordering is still enforced outside the
    carve-out.
- idempotency on each (assert exact, never `contains()`); full oracles green over
  corpus **after** the carve-out.

## 4a. Drive-by fix — mis-targeted S4e regression test

`dhc_code_list_unchanged` (the `data_head_comment` group, A7·S4e) is **hollow**: its
input `(some-fn\n;; comment\narg1\narg2)` puts the comment before `arg1`, **not**
before the head `some-fn`, so `head_has_leading_comment(some-fn)` is `false` and the
test never enters the `InData=false` head-comment branch it claims to guard. Fix the
input so the comment precedes the head, exercising the code-list opener-alone path:

```erlang
Input  = <<"(;; comment\nsome-fn arg1 arg2)">>,
assert_format(Input,
              <<"(\n  ;; comment\n  some-fn\n  arg1\n  arg2)\n">>),
assert_idempotent(Input).
```

Engine is unchanged — this is a test-only correction (verify the expected output
against the actual opener-alone layout when you run it; the shape above is the
intended `InData=false` rendering). Keep it a genuine assertion of the head-comment
code path.

## 5. Constraints

Pure engine; `xref`/`dialyzer` standing; `warnings_as_errors`. Idempotent;
comment-preserving. The carve-out must **narrow** the invariant exactly to
export/import entry order — it must not silently weaken detection of any other
add/drop/mutate/reorder (the oracle self-tests in §4 guard this). Per `CLAUDE.md`
ledger discipline: this is a deliberate, documented relaxation of a safety oracle —
name it clearly in the report.

## 6. Ledger

```
Arc A7·S5b — export sort + oracle carve-out
[ ] export entries sorted by {name, arity}; only when all entries are (name arity) pairs
[ ] (export all)/mixed → order preserved; comments travel with entries; stable + idempotent
[ ] token oracle → multiset at EVERY site (properties + SUITE + corpus sweep)
[ ] AST oracle → normalize_module_decls (export now; import hook for S5c)
[ ] oracle self-tests: multiset still catches drop/mutate; normalized-AST still catches non-export reorder
[ ] tests: unsorted→sorted, already-sorted, arity-order, all/mixed-preserved, comment-travels
[ ] drive-by: fix dhc_code_list_unchanged (comment before head; genuinely exercises InData=false branch)
[ ] full oracles green over corpus post-carve-out; goldens updated + rationale
[ ] xref + dialyzer standing; warnings_as_errors; full project suite count stated
[ ] files changed + one-line rationale; carve-out named explicitly — deviations or "none"
```

Stop here. Import full (layout + sort + carve-out extension) is S5c.
