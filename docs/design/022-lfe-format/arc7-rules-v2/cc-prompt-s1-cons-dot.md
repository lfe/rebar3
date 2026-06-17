# 022 · Arc A7 · S1 — cons-dot / improper lists (CC spec)

> Target: Sonnet 4.6 + `erlang-guidelines`. **Implementation spec.** (The
> "dropped-code" half of the original CRITICAL prompt was a false alarm —
> compaction, not loss — so this slice is now cons-dot only.) **Stop and report**
> if blocked.
>
> **Output discipline:** Edit in place; don't reprint files; terse prose; run
> tests; report briefly.

## 0. Orientation

1. `CLAUDE.md`; `erlang-guidelines` (`11-anti-patterns`).
2. `docs/design/022-lfe-format/formatting-rules.md` §3.8.
3. `lfe/src/lfe_scan.erl` — reference for how the real reader treats `.`.
4. `src/r3lfe_format_lexer.erl`, `src/r3lfe_format_cst.erl`,
   `src/r3lfe_formatter.erl`.

## 1. The bug

A standalone `.` (the cons / improper-list operator, e.g. `(a . b)`,
`(cond . ,cond)`, `(a b . rest)`) is currently tokenized as an ordinary **symbol**
and treated as a regular list element, so the formatter breaks around it:

```
;; wrong
(_ (cond .
         ,cond))
;; right
(_ (cond . ,cond))
```

`lfe_scan` treats a `.` at a token boundary as its own token but a `.` *inside* a
symbol run (`a.b.c` pseudo-package names) as part of the symbol. We must match
that distinction.

## 2. Fix

### Lexer
A run that is **exactly** `"."` is the cons-dot → emit a distinct token kind
**`dot`** (not `symbol`). A `.` inside a larger run (`a.b.c`, `...`, `.5`) stays
part of that run (symbol/number as today). Minimal change: in the run classifier,
`classify_run(".")` → `dot`. Add `dot` to the `kind()` type. `dot` is a
**significant** (non-trivia) token; it round-trips via `to_iolist` like any other.

### CST
A `dot` inside a list marks an **improper tail**: the node *after* the `dot` is
the cons-tail of that list. Represent this so the printer can keep it glued (e.g.
a flag/field on the list node marking "dotted tail = last child", with the `dot`
itself not treated as an ordinary element). Keep it simple; the goal is that the
printer can render `(a b . tail)`.

### Printer
Render an improper list as `(elem… . tail)` — the ` . tail` is **glued**: never
break before the `.`, never break between the `.` and the tail. The elements
before the dot follow the normal regime (break-preserving / canonical as usual);
only the ` . tail` suffix is special (stays on the line of the last element /
hugs).

## 3. Tests — `r3lfe_format_lexer_SUITE` + `r3lfe_formatter_SUITE`

- lexer: `.` standalone → `dot`; `a.b.c` → one `symbol`; `...` → `symbol`;
  `(a . b)` → tokens incl. one `dot`. Round-trip (lossless) holds.
- formatter: `(a . b)` → `(a . b)`; `(cond . ,cond)` → `(cond . ,cond)`;
  `(a b . rest)` → kept on one line if it fits / dot glued when broken;
  the §3.8 example `(_ (cond . ,cond))` formats correctly.
- pseudo-package symbol `(project.subdir:foo …)` / a symbol containing `.` is
  **not** split.
- **AST-equivalence**: `lfe_io:read_string` of input and output agree (a real
  improper list reads as a cons — must match). Idempotency + token-preservation
  (now lexer-anchored) green.

## 4. Constraints

`xref`/`dialyzer` standing; `warnings_as_errors`. Don't break existing literal
tokenization. Add permanent regressions.

## 5. Ledger

```
Arc A7·S1 — cons-dot
[ ] lexer: run "." → dot token (distinct kind); a.b.c / ... unaffected; lossless
[ ] CST: dot marks improper tail; list carries a dotted-tail representation
[ ] printer: ` . tail` glued — never breaks around the dot; elements otherwise normal
[ ] lexer + formatter tests incl. (cond . ,cond), (a b . rest), pseudo-package symbol
[ ] AST-equivalence + idempotency + token-preservation green
[ ] xref + dialyzer standing; warnings_as_errors; no regressions
[ ] files changed + one-line rationale; deviations named — or "none"
```
