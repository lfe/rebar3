# 022 · Arc A7 · S2b-1 — regime classification + InData (behavior-neutral)

> Target: Sonnet 4.6 + `erlang-guidelines`. First half of S2b. **Behavior-neutral
> scaffolding** — adds the regime decision + quote-context plumbing; the
> break-preserving *renderer* is S2b-2. Output must stay byte-identical. **Stop and
> report** if blocked.
>
> **Output discipline:** Edit in place; don't reprint files; terse prose; build
> incrementally; run tests; report briefly. This slice is small — keep it small.

## 0. Orientation

1. `CLAUDE.md`; `erlang-guidelines` (`11-anti-patterns`).
2. `cc-prompt-s2b-regimes.md` (shared ref) §1 (regimes) and §2 (InData).
3. `src/r3lfe_formatter.erl`; CST has `nl_before/1`, `multiline/1` (S2a).

## 1. Scope — add the decision + the context, change NO output

### (a) `regime/2`
Add an internal `regime(Node, InData) -> canonical | break_preserving` per the
shared ref §1:
- `InData =:= true` → `break_preserving`.
- `tuple` / `binary` node → `break_preserving`.
- `list` whose head is in the specform table or is a def-form
  (`classify_head` → `{specform,_}` / `defform`) → `canonical`.
- `map` → `canonical`.
- any other `list` (plain call / unknown head / non-symbol head) → `break_preserving`.
- (leaves/prefixed: n/a — they don't take a regime; only containers do.)

Export it (or a test-only export) so it can be unit-tested.

### (b) Thread `InData :: boolean()`
Thread an `InData` flag through the printer recursion (default `false` at top
level), per shared ref §2:
- entering a `quote`/`quasiquote` prefix → inner printed with `InData = true`;
- entering an `unquote`/`unquote_splicing` prefix → inner with `InData = false`;
- everything else passes `InData` through unchanged.

### (c) Do NOT change rendering
Compute `regime/2` and thread `InData`, but **route every node through the current
renderer regardless** — `break_preserving` nodes use exactly today's path. So the
formatter's output is **byte-for-byte identical** to before this slice. (S2b-2
will branch `break_preserving` to the new renderer.)

## 2. Tests

- Unit-test `regime/2` on representative nodes (parse a snippet, grab the node):
  `(case …)` → canonical; `(defun …)` → canonical; `#m(…)` → canonical;
  `(foo a b)` plain call → break_preserving; `#(a b)` tuple → break_preserving;
  a node under a quote (`InData=true`) → break_preserving even if its head is
  `if`; a node under an unquote inside a quasiquote → its regime computed with
  `InData=false`.
- **No-output-change:** assert the formatter output is byte-identical on the
  corpus / the full `r3lfe_formatter_SUITE` stays green unchanged.

## 3. Constraints

Pure engine; `xref`/`dialyzer` standing; `warnings_as_errors`. No CST/lexer change.
No output change.

## 4. Ledger

```
Arc A7·S2b-1 — classify + InData
[ ] regime/2 implemented per shared ref §1; exported for test
[ ] InData threaded (quote/quasiquote → true; unquote/-splicing → false)
[ ] NO output change: formatter byte-identical; full formatter suite green
[ ] regime/2 unit tests (canonical vs break_preserving incl. quote/unquote ctx)
[ ] xref + dialyzer standing; warnings_as_errors; no regressions
[ ] files changed + one-line rationale; deviations named — or "none"
```

Stop here. The renderer is S2b-2.
