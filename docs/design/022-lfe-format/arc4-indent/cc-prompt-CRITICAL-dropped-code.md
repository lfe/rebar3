# 022 · CRITICAL — formatter dropped functioning code (token loss)

> Target: Sonnet 4.6 + `erlang-guidelines`. **RELEASE-BLOCKER. Highest priority.**
> Do this before any style work. **Stop and report** the moment you have a
> reproduction + root cause — do not also fix style issues in the same pass.

## 0. The report

Running the real formatter on LFE example code (from the `lfe` examples — e.g.
the file containing `universal-server`, and/or the GPS planner with `make-op`),
Duncan observed that **a whole working form was removed from the output**.
Specifically a `receive` form disappeared:

```
;; present in the INPUT, ABSENT from the formatted OUTPUT:
(defun universal-server ()
  (receive
    ((tuple 'become server-function)
     (funcall server-function))))
```

If real, this is **token loss** — the formatter must NEVER drop, add, or reorder a
token. It also means our **token-preservation oracle and the A6·S1 corpus sweep
failed to catch it**, which is itself a defect we must explain.

## 1. Step 1 — reproduce and CONFIRM (don't assume)

The pasted diff may have concatenated two files; verify before chasing ghosts.

1. Find the offending input(s). Search the vendored LFE examples in the repo
   (e.g. `_build/default/lib/lfe/examples/*.lfe`, and any `_integration` copies)
   for `universal-server` and for `make-op`. Note the exact file(s).
2. For each candidate file, run `r3lfe_formatter:format/1` on its contents and
   check **token-preservation directly**: the multiset+order of significant
   tokens (via `r3lfe_format_cst:significant_tokens/1`, or re-lex both sides with
   `r3lfe_format_lexer`) of the **input** must equal that of the **output**.
3. Report: does any file's output drop/alter/reorder significant tokens? Paste the
   smallest failing input fragment and the before/after.

- If **no** token loss occurs → report that (the diff was misleading); then jump
  to step 4 only for the oracle-coverage question, and stop.
- If token loss **is** real → continue to steps 2–4.

## 2. Step 2 — minimize and root-cause

Reduce to the **smallest** input that loses a token. Candidate suspects to probe
(LFE constructs in those examples that our clean-room lexer/CST might mishandle):
- the **cons-dot / improper list** `( … . tail)` — does the lexer treat a
  standalone `.` as the cons operator, or wrongly as an ordinary symbol? (LFE's
  `lfe_scan` makes `.` its own token; ours may not.) A mishandled `.` could
  desync the parse.
- empty `()` as a *value* in a k/v sequence (e.g. `del-list ()`).
- `receive` / `after`, deeply nested forms, `#(...)` tuples in data position.
Identify the exact construct and the exact place tokens are lost (lexer? CST
parse? printer?).

## 3. Step 3 — fix + permanent regression

Fix the root cause minimally. Add the minimized input as a **permanent
regression** (corpus fixture + an explicit token-preservation assertion). Keep all
existing oracles green.

## 4. Step 4 — explain & close the ORACLE GAP (mandatory)

This is as important as the fix. Answer in the report:
- **Was the offending file in the A6·S1 corpus sweep?** If not → the sweep's file
  discovery has a coverage gap; widen it to include these examples (and state the
  new count).
- **If it was swept, why did token-preservation pass?** That would mean the oracle
  itself is broken (e.g. comparing the wrong thing, or `significant_tokens`
  derived from the same broken CST on both sides so a drop is invisible). If the
  oracle can be fooled, **fix the oracle** so it compares the formatted output
  re-lexed *from scratch* against the original re-lexed from scratch — never two
  derivations of the same CST.

## 5. Ledger

```
CRITICAL — dropped code
[ ] reproduction: exact file(s) + smallest failing fragment (or "not reproducible
    — tokens preserved", with evidence)
[ ] root cause identified (lexer / CST / printer) — named precisely
[ ] fix applied minimally; before/after token-preservation now equal
[ ] minimized input added as permanent regression (fixture + assertion)
[ ] ORACLE GAP explained: was it swept? if yes, why missed? oracle/sweep fixed so
    this class cannot pass again
[ ] full suite green; xref/dialyzer at standing level
[ ] files changed + one-line rationale
```

Report as soon as you have steps 1–2 (repro + root cause) — we may want to react
before you finish the fix.
