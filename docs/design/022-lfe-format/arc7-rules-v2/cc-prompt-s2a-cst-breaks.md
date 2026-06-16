# 022 · Arc A7 · S2a — CST records author break positions (CC spec)

> Target: Sonnet 4.6 + `erlang-guidelines`. First new A7 slice. **Behavior-neutral
> instrumentation** — the CST gains information; the printer does NOT use it yet
> (S2b does). Implement exactly this. **Stop and report** if blocked.
>
> **Output discipline:** Edit in place; don't reprint files; terse prose; run
> tests; report briefly.

## 0. Orientation

1. `CLAUDE.md` (`warnings_as_errors` ON); `erlang-guidelines` (`11-anti-patterns`).
2. `docs/design/022-lfe-format/formatting-rules.md` §1 and §4 — why we need this
   (break-preserving for unknown forms requires knowing where the author broke).
3. `src/r3lfe_format_cst.erl` — the module you instrument. (Prereq: S1 cons-dot
   fix should be merged first; if it isn't yet, proceed but note it.)

## 1. Goal

The break-preserving regime (S2b) needs to know, for each form, **where the author
placed line breaks**. Record that in the CST now. **No printer change, no output
change** — this slice only adds data to the tree and tests that the data is
correct.

## 2. What to record

For every node, record whether **a newline preceded it among its siblings at its
parent's level** in the source — i.e. "the author started a new line here."

- Add a boolean field to the node record, e.g. `nl_before :: boolean()`, default
  `false`. Set it `true` when one or more `newline` tokens occur between the
  previous sibling (or the opener) and this node, at this list's level. (You
  already consume `newline` tokens for blank-line detection in `parse_seq_loop` /
  `consume_newlines_inner` — thread a "saw a newline since the last node" bit and
  stamp it onto the next node, then reset.)
- This must be set for children at **every** level: top-level forms, list/tuple/
  map/binary/eval children, and prefixed inner nodes.
- Expose an accessor `nl_before/1 :: cst_node() -> boolean()`.
- Optional convenience: `multiline/1 :: container-or-document -> boolean()` =
  "any direct child has `nl_before =:= true`" (derive; don't store separately
  unless cleaner).

Notes:
- A single `\n` sets `nl_before` (not just blank-line runs). Blank lines (≥2
  newlines) still produce the existing `blank` leading trivia — keep that
  unchanged; `nl_before` is additional and independent.
- A comment between siblings still attaches as today; `nl_before` reflects whether
  the *node* itself starts a new line.

## 3. Constraints

- **No behavior change.** `r3lfe_formatter:format/1` output must be **byte-for-byte
  identical** to before this slice (the printer ignores `nl_before`). Prove it.
- Keep all existing oracles green (idempotency, token-, comment-preservation,
  AST-equivalence). `-spec` the new accessor; `xref`/`dialyzer` at standing level;
  `warnings_as_errors`.
- Don't touch the lexer or the formatter/printer.

## 4. Tests — extend `r3lfe_format_cst_SUITE`, group `nl_before`

Assert `nl_before` for representative inputs:
- `(a b c)` (flat) → all children `nl_before=false`.
- `(a b\n  c)` → `c` is `true`, `a`/`b` `false`.
- `(make-op action 'x\n  preconds '(...)\n  add-list '(...))` → the children that
  start each line are `true`, the grouped ones `false` (this is the make-op
  grouping S2b will preserve).
- top-level: two forms separated by a newline → second form `nl_before=true`.
- nested: a child list that itself is multi-line → its inner children stamped
  correctly.
- a `'(a\n  b)` quoted list → inner `b` is `true`.
Plus a **no-change** assertion: format a corpus sample before/after is identical
(or simply rely on the full formatter suite staying green).

## 5. Ledger

```
Arc A7·S2a — CST nl_before
[ ] node gains nl_before (per-child "author started a new line"); accessor exported
[ ] set correctly at all levels (top-level, containers, prefixed inner, quoted)
[ ] single \n sets it; existing blank-line trivia unchanged
[ ] NO output change: formatter output byte-identical; full formatter suite green
[ ] group `nl_before` tests assert positions on the listed inputs
[ ] xref + dialyzer standing; warnings_as_errors; no regressions
[ ] files changed + one-line rationale; deviations named — or "none"
```

Stop here. The printer starts using `nl_before` in S2b.
