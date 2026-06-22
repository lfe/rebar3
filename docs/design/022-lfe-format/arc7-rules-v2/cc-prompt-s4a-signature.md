# 022 · Arc A7 · S4a — def-forms never alone + narrow fix2 (CC spec)

> Target: Sonnet 4.6 + `erlang-guidelines`. First S4 sub-slice. **Implementation
> spec.** **Stop and report** if blocked.
>
> **Output discipline:** Edit in place; don't reprint files; terse prose; run
> tests; report briefly.

## 0. Orientation

1. `CLAUDE.md`; `erlang-guidelines` (`11-anti-patterns`).
2. `cc-prompt-s4.md` (shared ref); `formatting-rules.md` §3.1.
3. `src/r3lfe_formatter.erl` — the specform/defform distinguished-args handling and
   the A4·S1·fix2 comment guard (`any_dist_has_comment` / the fallback that pushes
   distinguished args to the body / opener-alone).

## 1. The bug (regression from A4·S1·fix2)

A trailing comment on a def-form's name or arglist currently triggers fix2's
fallback, exploding the signature one-token-per-line:

```
;; input                              ;; WRONG (current)
(defun start_star ;; comment          (defun
  …body…)                               start_star ;; comment
                                        …)

(defun star (x) ;; comment            (defun
  …body…)                               star
                                        (x) ;; comment
                                        …)
```

A trailing comment on the **last** signature item (the name in a match-clause
defun; the arglist in a signature defun) is **safe** — the comment ends the head
line and the body goes below. fix2 over-reached: only a comment on a *non-last*
distinguished arg risks swallowing the next item.

## 2. The fix (rules §3.1)

1. **A trailing comment on the LAST distinguished arg is safe.** Render the
   distinguished args on the head line as normal, emit that last item's trailing
   comment at the end of the head line, then the body below at +2. Do **not** fall
   back to the body/opener-alone layout for this case.
2. **Only a comment on a NON-LAST distinguished arg** (a trailing comment that
   would be followed by another distinguished arg on the same line, or a leading
   comment on a distinguished arg) keeps the existing fallback.
3. **Def-forms are never alone on a line.** Ensure no path puts the
   `(defun`/`(defmacro` keyword (or the keyword + name) on a line by itself: the
   keyword + name (+ arglist) always share the head line. (The head-leading-comment
   opener-alone branch from A3 fix1 is for a comment *before the head* — leave it;
   it does not apply here.)

Concretely: refine the distinguished-args comment guard so the "safe" predicate is
"the only comment among the distinguished args is a trailing comment on the last
one" → keep on head line; otherwise the existing fallback.

## 3. Tests — `r3lfe_formatter_SUITE`

- `(defun star (x) ; c\n body)` → `(defun star (x) ; c\n  body)` (signature + comment
  on head line, body at +2). *(use a real arglist; the snippet above elided it)*
- match-clause `(defun f ; c\n ((0) 1) ((n) …))` → `(defun f ; c\n  ((0) 1)\n …)`
  (keyword + name + trailing comment on head line; clauses below).
- `defmacro` equivalents.
- a **non-last** dist-arg comment still falls back safely (no swallow) — e.g. a
  trailing comment on the name when an arglist follows on the same line in source.
- confirm no def-form output ever has `(defun` / `(defmacro` alone on a line.
- idempotency on each; full oracles over corpus green; update affected goldens.

## 4. Constraints

Pure engine; `xref`/`dialyzer` standing; `warnings_as_errors`. Comment-swallow
protection for non-last items must remain intact.

## 5. Ledger

```
Arc A7·S4a — signature never alone
[ ] trailing comment on LAST distinguished arg → safe (stays on head line, body below)
[ ] non-last dist-arg comment → existing fallback (no swallow) retained
[ ] def-forms never render the keyword/name on a line by itself
[ ] tests: defun/defmacro signature + trailing comment; match-clause; non-last case
[ ] idempotency + full oracles green; goldens updated
[ ] xref + dialyzer standing; warnings_as_errors; full suite count stated
[ ] files changed + one-line rationale; deviations named — or "none"
```

Stop here. Closing-delimiter de-indent is S4b.
