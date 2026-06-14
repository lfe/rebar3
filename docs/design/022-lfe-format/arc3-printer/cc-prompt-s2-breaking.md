# 022 · Arc A3 · S2 — the break algebra (CC implementation spec)

> Target: Sonnet 4.6 + `erlang-guidelines`. Second of three A3 sub-slices.
> Prereq: S1 merged (flat rendering works). Implement exactly this; **stop and
> report** if blocked.
>
> **Output discipline:** edit the existing module in place with Edit; don't
> reprint files; terse prose; run tests; report briefly. Stay within this slice.

## 0. Orientation

1. `CLAUDE.md`; `erlang-guidelines` (`11-anti-patterns.md` first).
2. `arc3-printer/cc-prompt.md` shared ref — §2 fixed decisions, §3.1/§3.2 break
   algebra, §6 oracles.
3. Your S1 `src/r3lfe_formatter.erl` (extend it) and the closed lexer/CST.

## 1. Scope of THIS slice

Add **width-driven breaking** to `r3lfe_formatter`. Still **comment-free** inputs
(comments are S3).

- `?WIDTH` = 80 constant; indent unit 2 spaces.
- Add `flat_width/1` (or reuse S1's `flat_render` and measure its length).
- **Flat eligibility** (shared ref §3.1, minus the comment clause, which is S3): a
  node prints flat iff it contains no multi-line token (`tqstring`/`tqbstring`)
  **and** `current_column + flat_width(node) =< ?WIDTH`.
- **Broken form** (shared ref §3.2, the generic +2 rule): for a container at
  column `C` that can't be flat —
  ```
  <OPEN><child0>
  <C+2><child1>
  <C+2><child2> … <childN><CLOSE>
  ```
  head (child0) on the opener line; children 1..N each on their own line at
  `C+2`; `<CLOSE>` hugs the last child. Children are printed recursively
  (each makes its own flat/broken choice) starting at `C+2`.
- `prefixed` that must break: `prefix.text` then the broken `inner`, still glued.
- Multi-line leaf tokens (`tqstring`/`tqbstring`): emit verbatim; set current
  column to the length after the last `\n`.

Column tracking is the crux: thread a current-column through the printer so flat
eligibility is decided correctly at each node.

## 2. Tests — extend the suite, group `breaking`

Comment-free inputs spanning widths: a form that fits (stays flat), a form that
exceeds 80 (breaks to +2 hanging), a nested form where an inner list breaks but
an outer part stays flat, a deeply nested form. Assert (shared ref §6):
idempotency, token-preservation, AST-equivalence. Add 2–3 golden outputs showing
the exact +2 broken shape. Keep inputs comment-free.

## 3. Constraints

As S1: pure module; `-spec`; `xref`+`dialyzer` clean; `warnings_as_errors`; don't
modify lexer/CST. **Keep** `format/1`'s abstract `{ok, iolist()} | {error,_}`
spec and the existing `-dialyzer({no_underspecs, format/1})` — do not narrow the
spec to match Dialyzer's inferred nested-list type.

## 4. Ledger

```
Arc A3·S2 — breaking
[ ] flat_width / flat-eligibility (width 80) implemented; column threaded
[ ] generic +2 broken form for containers and prefixed; CLOSE hugs last child
[ ] multi-line tokens emitted verbatim with correct column update
[ ] group `breaking`: idempotency + token + AST-equiv across widths, green
[ ] golden +2-shape tests present
[ ] xref + dialyzer clean; warnings_as_errors clean; no regressions
[ ] files changed + one-line rationale; deviations named — or "none"
```

Stop here. Do not start S3.
