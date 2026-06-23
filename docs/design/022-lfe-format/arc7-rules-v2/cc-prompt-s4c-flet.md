# 022 · Arc A7 · S4c — flet/fletrec locals format like defuns (CC spec)

> Target: Sonnet 4.6 + `erlang-guidelines`. S4 sub-slice. **Implementation spec.**
> **Stop and report** if blocked.
>
> **Output discipline:** Edit in place; don't reprint files; terse prose; run
> tests; report briefly. **Gallery stays paused** (S6 regenerates).

## 0. Orientation

1. `CLAUDE.md`; `erlang-guidelines` (`11-anti-patterns`).
2. `cc-prompt-s4.md` (shared ref); `formatting-rules.md` §3.6.
3. `src/r3lfe_formatter.erl` — the specform N=1 distinguished-arg branch
   (~line 985, the `is_let_head` force-break of the binding list);
   `is_let_head/1` (~597); `defform_n/1` (~788); `is_arglist/1` (~752);
   `print_broken_container/3` (~168, the geometry+classify hub);
   `print_classified({specform,N}, …)` (~968).

## 1. The rule (rules §3.6)

`flet` / `flet*` / `fletrec` are **flat-if-fits** — do **not** add them to
`must_break`/`is_always_break_head`; the gallery #31 case stays on one line.

But **when an flet/fletrec breaks** (too wide to fit), its binding list must:
1. **force-break** — one local-function definition per line (like `let` already
   force-breaks its binding list), aligned under the first binding; and
2. render **each local-function binding `(name (args) body…)` like a `defun`**:
   name + arglist on the head line, body at **+2** — **not** align-under-the-arglist
   (the funcall default a plain-symbol head would otherwise get). *(corr. #9)*

```
;; WRONG (funcall align under arglist)        ;; RIGHT (defun-like, body +2)
(flet ((my-local-helper (x y)                 (flet ((my-local-helper (x y)
                         (+ (* x x)             (+ (* x x)
                            (* y y)))                (* y y))))
       …)                                        …body…)
  …body…)
```

The match-clause local-fn form `(name ((pat) body)…)` renders like a match-clause
`defun`: name on the head line, clauses at +2.

## 2. The fix

The structural insight: a flet binding `(name (args) body…)` is a `defun` minus
the keyword. Render it via the **same N-distinguished-args machinery**, with the
binding's *name* as head and N computed by the defun rule:

- **N = 2** when the 2nd child `is_arglist/1` → signature form (name + arglist on
  head line, body +2).
- **N = 1** otherwise → match-clause form (name on head line, clauses +2).

Suggested shape (CC may choose the exact factoring, but **reuse
`print_broken_container`'s geometry — do not duplicate the dangling/close/`Open`
logic**):

1. `is_flet_head/1` — mirror `is_let_head/1` for `"flet"`, `"flet*"`, `"fletrec"`.
2. `local_fn_n/1` — given a binding node, `[_Name, Arg2 | _]` & `is_arglist(Arg2)`
   → 2; else 1 (this is `defform_n`'s defun branch generalized to an arbitrary
   head; you may factor a shared helper).
3. A way to render a single binding with a **forced** head classification of
   `{specform, local_fn_n(Binding)}` instead of letting `classify_head` route its
   plain-symbol head to `funcall`. Cleanest: extract the geometry+dispatch part of
   `print_broken_container` into a helper taking an optional `ForcedClass`
   (`undefined` = classify normally), then call it with the forced class for flet
   bindings. The `{specform,N}` renderer already does "N distinguished args on head
   line, body at +2" — which is exactly defun-like — so no new layout code.
4. In the specform N=1 distinguished-arg branch, add an `is_flet_head(Head)` arm
   alongside the existing `is_let_head(Head)` arm: force-break the binding list and
   render **each element** via the forced-`{specform,N}` path (one per line, aligned
   under the first binding). Do not route flet binding elements through the generic
   element renderer (that would give them funcall alignment).

Keep `let`/`let*` behavior unchanged; only flet-family bindings get defun-like
element rendering.

## 3. Tests — `r3lfe_formatter_SUITE` (new `flet_locals` group; add to `all/0`)

- **flat-if-fits unchanged:** gallery #31 input stays one line.
- **breaks → defun-like body +2:** an flet whose single local fn is wide enough to
  force a break → `(name (args)` on the head line, body at +2 under the binding's
  content indent (assert exact output).
- **multiple locals → one per line, aligned under first binding;** each defun-like.
- **match-clause local fn** `(name ((0) 1) ((n) …))` → name on head line, clauses +2.
- **fletrec** behaves identically to flet.
- **idempotency** on every case (assert exact, never `contains()`); full oracles
  (idempotency, token/comment-preservation raw-lexer, AST-equivalence) over corpus.
- update any affected goldens with a one-line rationale.

## 4. Constraints

Pure engine; `xref`/`dialyzer` standing; `warnings_as_errors`. Idempotent;
token-/comment-preserving; AST-equivalent. `let`/`let*` rendering unchanged.
flet/fletrec must **not** enter the always-break set (flat-if-fits preserved).

## 5. Ledger

```
Arc A7·S4c — flet/fletrec locals as defuns (rules §3.6)
[ ] flet/flet*/fletrec NOT added to must_break (flat-if-fits preserved; #31 unchanged)
[ ] is_flet_head/1 added; let/let* behavior unchanged
[ ] when flet breaks: binding list force-broken, one local fn per line, aligned under first
[ ] each local fn rendered defun-like: N=2 signature (name+args head, body +2); N=1 match-clause
[ ] geometry reused from print_broken_container (no duplicated close/dangling/Open logic)
[ ] tests: flat-unchanged, breaks-defun-like, multi-local, match-clause, fletrec, idempotency
[ ] full oracles green over corpus; goldens updated + rationale
[ ] xref + dialyzer standing; warnings_as_errors; full project suite count stated
[ ] files changed + one-line rationale; deviations named — or "none"
```

Stop here. try symmetry + try clauses is S4d.
