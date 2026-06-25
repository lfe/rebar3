# LFE Formatter — Authoritative Formatting Rules (v2)

> Status: **for review** (Duncan). Synthesized from real-world testing feedback
> (2026-06-15). This is the single source of truth for the formatter's behavior
> and **supersedes** conflicting bits of `rebar3-lfe-provider.md` §3 and the
> A3/A4 prompts. Once approved, it drives a focused implementation arc (A7).

## 1. Core model — knowledge-gated

The formatter treats a form one of two ways depending on whether it *understands*
that form:

**Known forms** — the special-form table, def-forms, maps, and clauses. The
formatter **owns the layout**: it imposes the canonical shape, and it **adds or
fixes breaks** (e.g. an `if` written flat is broken; a wrongly-broken `let` is
re-laid-out). The author's break positions inside a known form are **not**
preserved — they are re-imposed.

**Unknown forms** — plain function calls, record-generated macros (`make-op`,
`match-person`, …), and user macros. The formatter **preserves the author's break
positions**; it only normalizes indentation (align-under-first-arg) and wraps any
line over 80 columns. Rationale: their grouping encodes intent the formatter
cannot recover without macro expansion / a populated `$ENV` (e.g. that `make-op`
takes k/v pairs).

Consequences (accepted, by design): output is **canonical for known forms** and
**author-layout-dependent for unknown forms**; it remains **idempotent**
throughout. This intentionally revises the original "full reflow" decision —
unknown forms are now break-preserving.

## 2. Known forms: always-break vs flat-if-fits

**Always-break** (break onto multiple lines even when it would fit in 80):

`if`, `let`, `let*`, `case`, `cond`, `maybe`, `receive`, `try`, `progn`; maps
(`#m`); def-forms with a body; match/case clauses with a non-trivial body (§3.3);
`export` / `import` (§3.5).

**Flat-if-fits** (stay on one line when small; canonical layout once broken or
over 80):

`lambda` (with the structure nuance, §3.2), `when` (standalone), list/binary
comprehensions (`lc`/`bc`), the `(: mod fn …)` call form, and all plain/unknown
calls.

## 3. Form-specific rules

### 3.1 def-forms
- Name **and** arg-spec stay on the head line — whether the arg-spec is an
  arglist `(…)`, an empty `()`, or a **single symbol** (the `defmacro` whole-args
  form, e.g. `(defmacro orelse args …)`). Body breaks below at +2. *(corr. #2)*
- **`defun`/`defmacro` are NEVER on a line by themselves.** The `(defun`/
  `(defmacro` keyword + the name (+ arg-spec) always share the head line. A
  **trailing comment** on the name or the arglist (i.e. on the *last* item of the
  signature) is emitted at the end of the head line, with the body below — it must
  **not** trigger the opener-alone fallback. *(corr. — `(defun start_star ;c` and
  `(defun star (x) ;c` were each exploded one-token-per-line)*
  - This **refines A4·S1·fix2**: a trailing comment on the *last* distinguished
    arg is safe (the comment ends the line; body follows below). Only a comment on
    a *non-last* distinguished arg risks swallowing the next item and needs the
    fallback. Narrow fix2 accordingly.
- `defun`/`defmacro` with an **empty arglist** `()` that fits → stays flat (the
  constant idiom, `(defun +my-pi+ () 3.14)`). *(unchanged)*

### 3.2 lambda
- **Flat** iff: exactly one body form, that form is itself flat (contains no
  always-break form), and the whole lambda fits in 80.
- **Break** iff: more than one body form (implicit `progn` → one per line), **or**
  the single body form is structural (a map / `case` / `let` / … that itself
  breaks), **or** it overflows.
- `(lambda (x y) (+ x y))` → flat; `(lambda () (do-thing))` → flat;
  `(lambda (x) (a) (b))` → break; `(lambda (x) #m(…))` → break.

### 3.3 Clauses (case clauses; `defun`/`match-lambda` match-clauses)
- The enclosing `case`/`cond`/match-`defun` **always breaks** (clauses one per
  line).
- A clause `(pattern body…)` is **flat** iff its body is a **single trivial
  datum** (atom, number, quoted atom/keyword) and it fits — e.g. `(0 'zero)`,
  `(1 'one)`.
- Otherwise **break**: pattern on its line, body below at +2 — even for a single
  call body, e.g. `((tuple 'ok v) (store v))`. *(corr. #6, #8)*
- A `(when …)` guard stays on the pattern's line; body below. *(A4·S3d)*

### 3.4 Maps & proplists
- Map `#m(…)`: one **k/v pair** per line when broken (always-break). *(S3a)*
- Proplist (a list of 2-tuples): each pair is already a single element, so
  ordinary one-element-per-line covers it — no special k/v logic.
- A flat alternating k/v sequence in an **arbitrary call** (`make-op …`) is **not**
  detectable → unknown form → break-preserved. *(corr. #7 note)*
- **Open question:** force-break a *detected* all-2-tuple proplist even when it
  would fit flat? Default: **no** (leave it to break-preservation).

### 3.4a Closing delimiters never de-indent
- A closing paren is **never** moved to a shallower indent than the content it
  closes. We hug the close to the last token wherever possible (the Lisp norm).
  The *only* time a close goes on its own line is when the preceding line is a
  comment (a `)` can't follow `; …` — it would be swallowed); in that case the
  close is indented to the **same level as that preceding comment/content line**,
  **not** de-indented to the form's opening column. *(corr. — the mnesia-demo
  `export` close `))` de-indented from the items'/comment's column.)* Changes
  `close_section`'s lone-close indentation. (Routes to S4.)

### 3.5 export / import
- Always **one entry per line**, regardless of count (force-break; not
  flat-if-fits). *(corr. #4, #12)*
- **Item indent = +1, aligned under the keyword** *(decided 2026-06-23, Duncan;
  matches xrepl-store.lfe and the mnesia-demo)*. `export` items sit one column
  past the `(export` open paren (under the `e`), **not** the body +2 used by other
  special forms. The same +1-under-keyword rule applies at every level of
  `import` (clauses under `import`; `from`/`rename` entries under their keyword).
- **Entries sorted alphabetically** by name, then arity. **This is the one rule
  that reorders tokens** — a deliberate, semantics-preserving exception to the
  never-reorder invariant.
  - `export`: sort the `(name arity)` entries.
  - `import` `(from M …)`: sort the `(name arity)` entries within each clause; the
    module `M` stays on the clause head line.
  - `import` `(rename M …)`: sort the `((name arity) new-name)` pairs by the
    **old** name then arity.
  - The order of the `from`/`rename`/`deprecated` **clauses themselves is
    preserved** (not reordered); only entries *within* a clause are sorted.
  - `(export all)` / non-`(name arity)` entries: left as-is (no sort).
  - **Commented entries suppress the sort** *(S5b decision, 2026-06-23; ratify):*
    if **any** entry carries a comment (leading **or** trailing), the whole list's
    order is **preserved**, not sorted — such a comment typically marks an intentional
    category grouping (`;; Callbacks`, `;; Public API`), and reordering would both
    scramble the grouping and force a third oracle carve-out (the
    comment-preservation oracle, which checks comment *order*). Suppressing the
    sort keeps that oracle strict.
- **Oracle carve-out** (both oracles break under reordering): the
  token-preservation oracle weakens from token *sequence* to token *multiset*
  (catches add/drop/mutate, ignores order); the AST oracle normalizes
  export/import entry order on both sides before comparison (so it remains the
  ordering authority everywhere *except* the carved-out entries). *(corr. #4)*
- Closing delimiters follow §3.4a (no de-indent; align with the last item/comment).
- **Layout (import), worked example:**

  ```
  (import
   (from lists
    (all 2)
    (any 2)
    (member 2))
   (rename lists
    ((all 2) every)
    ((any 2) some)
    ((filter 2) find-all)))
  ```

### 3.6 flet / fletrec / let-function / letrec-function
- Local function definitions format like `defun`s: name + args on the head line,
  body at +2 (not align-under-the-arglist). *(corr. #9)*

### 3.7 try
- **Full symmetry** *(corr. #10; shape finalized 2026-06-23, Duncan)*. `try`
  always breaks. The `try` keyword sits **alone** on its line; the protected body
  expr **and** every `case`/`catch`/`after` section sit at **+2**; each section
  keyword sits **alone** on its line with its contents below at **+4**. Even small
  sections break (symmetry over flat-if-fits); the body expr is never kept on the
  `try` head line.
- `case`/`catch` sections hold clauses directly (no test expr) → rendered via
  `render_clause` (resolves the S3b-2 deferral). `after` holds body forms →
  rendered as a body (one per line), not as clauses.

  ```
  (try
    (foo x)
    (case
      ((tuple 'ok v) v))
    (catch
      ((tuple 'error reason)
       (error reason)))
    (after
      (cleanup)))
  ```

### 3.8 Cons-dot / improper lists
- A standalone `.` (the cons operator, surrounded by whitespace) is **not** a
  symbol. Keep `(a . b)` / `(head . ,tail)` together; never break around the dot.
  *(corr. #1)* The lexer must distinguish the cons-dot from a `.` inside a symbol
  (`a.b.c` pseudo-package names). **(Likely the same root cause as the critical
  dropped-code bug — see `arc4-indent/cc-prompt-CRITICAL-dropped-code.md`.)**

### 3.9 Comments at the head of a data list
- A leading comment as the first element of a data list stays on the opener line:
  `'(;; comment` then elements aligned under the first element. *(corr. #3;
  reconcile with the A3 head-leading-comment handling, which currently puts the
  opener alone.)*

## 4. Architecture impact

- **CST (A2):** must record, for **unknown** forms, where the author placed line
  breaks (which children begin a new line) so the printer can reproduce them.
  Known forms ignore this. (The lexer already emits newline tokens; A2 records
  per-child "a newline preceded me at this level".)
- **Printer (A3/A4):** flat-eligibility / must-break gains "the author broke this
  unknown form," plus the §2–§3 per-form rules. The `align-under-first-arg`
  continuation rule still applies to broken unknown calls.
- **Supersedes** the previous over-collapsing behavior. The **gallery must be
  regenerated** (several entries change — e.g. `if` now breaks).

## 5. Invariants (unchanged)

Idempotent; comment-preserving; AST-equivalent; token-preserving **except** the
deliberate `export`/`import` alphabetical reorder (carved out of the oracle).

## 6. Proposed implementation arc (A7) — for planning after approval

1. **A7·S1** — cons-dot fix + the critical dropped-code investigation (may be one
   root cause). *(blocking; already drafted as the CRITICAL prompt.)*
2. **A7·S2** — knowledge-gated core: CST records unknown-form breaks; printer's
   two regimes (known = canonical, unknown = break-preserving).
3. **A7·S3** — always-break additions (`if`, `progn`, `receive`, `try`, `maybe`)
   + the clause body rule + the lambda structure rule.
4. **A7·S4** — signature-on-head-line (#2) incl. **def-forms never alone + narrow
   fix2** (§3.1), **closing delimiters never de-indent** (§3.4a), flet/fletrec-as-
   defun (#9), try symmetry (#10), data-list head comment (#3).
5. **A7·S5** — exports always-break + alphabetical sort + oracle carve-out.
6. **A7·S6** — regenerate the gallery; full oracle + property + e2e sweep.
