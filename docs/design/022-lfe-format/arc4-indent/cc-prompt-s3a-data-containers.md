# 022 · Arc A4 · S3a — data-container alignment (CC implementation spec)

> Target: Sonnet 4.6 + `erlang-guidelines`. First half of A4·S3. **Implementation
> spec** — decisions made; implement exactly. **Stop and report** if blocked.
>
> **Output discipline:** Edit in place; don't reprint files; terse prose; run
> tests; report briefly. Stay within this slice.

## 0. Orientation

1. `CLAUDE.md` (`warnings_as_errors` ON); `erlang-guidelines` (`11-anti-patterns`).
2. `arc4-indent/cc-prompt.md` shared ref — head classification + reflow semantics.
3. Your `src/r3lfe_formatter.erl` — `print_broken_container/2`,
   `print_classified/12` (esp. the `list_head` clause), the comment-matrix
   helpers. A4·S1+S2 closed.

## 1. The bug this fixes

`print_broken_container/2` calls `classify_head(Head)` for **every** container
type. That's only correct for **code** (`list`). For **data** containers
(`tuple`/`map`/`binary`), the first child is an *element*, not a function head, so
`#(case x y)` is wrongly treated as a `case` specform, and a breaking map gets
funcall alignment instead of the style guide's key-value pairs.

## 2. The fix — dispatch by node type

In `print_broken_container/2`, branch on the node's `type/1` (only in the
`[Head | RestChildren]` path; the `head_has_leading_comment` opener-alone branch
and the empty-container branch stay as-is and apply to all types):

- **`list` and `eval`** (`#.(…)` is read-eval = code) → unchanged: `classify_head`
  + `print_classified`.
- **`tuple` and `binary`** → **element alignment**: every element aligned under the
  first element at `C + OpenLen`, one per line. This is exactly the existing
  `list_head` rendering — reuse `print_classified(list_head, …)` for these types
  (do not run `classify_head`).
- **`map`** → **key-value pairs**, one pair per line (style guide §6):
  ```
  #m(k1 v1
     key2 value2
     key-the-third value-the-third)
  ```
  Align column = `C + OpenLen` (e.g. `C+3` for `#m(`). Pair the children
  `[k1,v1,k2,v2,…]`; the first pair shares the opener line (`#m(k1 v1`), each
  later pair on its own line at the align column; each pair printed as
  `print_node(key) ++ " " ++ print_node(value)`. Close hugs the last value
  (own line per the usual `close_section` rules). Odd final child (malformed map)
  → emit it alone as a trailing element.

  **Comment safety:** if **any** map child has a leading or trailing comment, do
  **not** pair — fall back to element-per-line (the `tuple`/`list_head` rendering).
  Pairing + comments would risk a comment swallowing the paired value; the
  element-per-line fallback routes every child through the safe `print_rest_loop`
  path. (Reuse the head-line-comment guards already in place.)

## 3. Tests — extend `r3lfe_formatter_SUITE`, group `data_containers`

Golden + idempotency, on forms wide enough to break:
- **map** pairs: a wide `#m(k1 v1 k2 v2 …)` → first pair on opener line, rest
  aligned, pairs intact.
- **map with a comment** → falls back to element-per-line; token-preserved,
  paren intact, idempotent.
- **tuple** wide → elements aligned under the first.
- **binary** wide → segments aligned under the first.
- **`#(case x …)`** (tuple whose first element is a table keyword) → element
  alignment, **not** case-specform indentation (this is the regression).
- a **map/tuple nested in a list** and vice versa → correct propagation.
- Re-run the full `oracles` group (idempotency, token-, comment-preservation,
  AST-equivalence) over the corpus; add representative data containers to the
  fixture.

## 4. Constraints

Pure module; keep `format/1`'s abstract spec + both `no_underspecs` suppressions;
`xref`+`dialyzer` clean; `warnings_as_errors`; don't modify lexer/CST. The
comment-on-head-line matrix and idempotency must keep holding for all container
types.

## 5. Ledger

```
Arc A4·S3a — data-container alignment
[ ] print_broken_container dispatches by type: list/eval → classify_head;
    tuple/binary → element align (reuse list_head); map → key-value pairs
[ ] map pairing: first pair on opener line, rest aligned at C+OpenLen, close ok
[ ] map with any comment → element-per-line fallback (no swallow)
[ ] #(case …) regression: tuple not treated as a specform
[ ] group `data_containers`: goldens + idempotency (map/tuple/binary/nested)
[ ] full `oracles` green over corpus (state count); representatives added
[ ] xref + dialyzer clean; warnings_as_errors clean; no regressions
[ ] files changed + one-line rationale; deviations named — or "none"
```

Stop here. Do not start S3b (style-guide conformance).
