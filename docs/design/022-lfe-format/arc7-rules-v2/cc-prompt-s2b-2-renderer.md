# 022 · Arc A7 · S2b-2 — break-preserving renderer (CC spec)

> Target: Sonnet 4.6 + `erlang-guidelines`. Second half of S2b — the renderer that
> makes break-preserving real. Prereq: S2b-1 merged (`regime/2` + `InData`).
> **Implementation spec.** **Stop and report** if blocked.
>
> **Output discipline (this is the one that blew the cap):** edit the module in
> place with small Edits; **do not** paste files back; keep prose to a few lines;
> add the renderer first and run a couple of goldens, then add the rest of the
> tests. Do NOT emit everything in one giant response.

## 0. Orientation

1. `CLAUDE.md`; `erlang-guidelines` (`11-anti-patterns`).
2. `cc-prompt-s2b-regimes.md` (shared ref) §3 (the renderer) and §4 (keep
   canonical unchanged).
3. Your S2b-1 `regime/2` + `InData`; CST `nl_before/1`, `multiline/1`.

## 1. Scope — branch break-preserving nodes to the new renderer

Where the printer currently renders a container, branch on `regime(Node, InData)`:
- `canonical` → existing A4 path, **unchanged**.
- `break_preserving` → the new renderer below.

### Break-preserving renderer (shared ref §3)
- **Flat** iff: `not multiline(Node)` (author didn't break it) AND it fits in 80
  AND no `must_break` descendant → current flat path.
- **Else broken, preserving author break positions:**
  - head/opener + first child on the opener line — unless the first child has
    `nl_before=true`, then it starts a new line too.
  - each subsequent child: new line iff `nl_before(child)` **or** it would
    overflow 80 on the current line; else a single space on the current line
    (preserves grouping, e.g. `make-op` k/v pairs).
  - continuation column = the column where the **first argument** is rendered
    (align-under-first-arg); if the first arg itself broke to its own line, use
    the form's open column + 2 and align subsequent args there.
  - close hugs the last child (comment-before-close is S4 — leave current).
- `dot`/improper-tail (from S1, if merged): the ` . tail` stays glued — never
  break around the dot. (If S1 isn't merged yet, note it; the dot is currently a
  symbol child and will be handled once S1 lands.)

## 2. Tests — `r3lfe_formatter_SUITE`, group `regimes`

Keep this group focused (add incrementally):
- make-op-style call with author-grouped k/v across lines → grouping preserved.
- plain call author broke (fits flat) → stays broken.
- plain call author wrote flat (fits) → stays flat.
- plain call too wide → wraps, aligned under first arg.
- quoted data list `'(a\n b\n c)` → breaks preserved; `'(a b c)` → stays flat.
- quasiquote with unquoted `if` (`` `(x ,(if a b c)) ``) → `if` canonical, outer data.
- tuple/binary author broke → preserved.
- **canonical-unchanged regression:** a `case` and a `defun` format identically to
  before this slice.
- Full oracles over the corpus: idempotency, token-, comment-preservation,
  AST-equivalence (token-preservation now lexer-anchored). Add representatives to
  the fixture.

## 3. Constraints

Pure engine; `xref`/`dialyzer` standing; `warnings_as_errors`. Idempotent;
token-/comment-preserving; AST-equivalent. Don't touch lexer/CST.

## 4. Ledger (closes S2b)

```
Arc A7·S2b-2 — break-preserving renderer
[ ] container rendering branches on regime/2; canonical path byte-unchanged
[ ] break-preserving renderer: flat iff author-flat+fits; else nl_before-driven
    breaks, align-under-first-arg, wrap >80; grouping (make-op) preserved
[ ] group `regimes` tests + canonical-unchanged regression
[ ] full oracles green over corpus (state count)
[ ] xref + dialyzer standing; warnings_as_errors; no regressions
[ ] files changed + one-line rationale; deviations named — or "none"
```

When green, S2b is complete; A7 continues with S3 (always-break + clause + lambda).
