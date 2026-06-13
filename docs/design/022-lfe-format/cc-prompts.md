# 022 — CC prompts for `rebar3 lfe format`

Companion to `rebar3-lfe-provider.md`. Hand these to CC **one arc at a time**.
Each arc lives in its own subdir: `arcN-<slug>/cc-prompt.md` (the spec) and
`arcN-<slug>/cc-prompt-fixM-<slug>.md` (scoped fixes). A2 is ready now; A3–A6 are
drafts to be tightened after the prior arc's report is graded.

Status: **A1 CLOSED** ✅ · **A2 CLOSED** ✅ · **A3 READY** · A4–A6 drafts.

Every prompt assumes CC will, at the top of the session:
- read `CLAUDE.md` (project conventions; `warnings_as_errors` is ON),
- load the `erlang-guidelines` skill and read `11-anti-patterns.md` first,
- read `rebar3-lfe-provider.md` (the design),
- treat the existing providers as the house style.

Shared non-negotiables to restate in each handoff:
- The formatter **must never reorder, insert, or drop a token** — only
  whitespace/newlines/indentation may change.
- **Comments must be preserved** (line `;` and block `#| |#`).
- `-spec` every exported function; code must pass `rebar3 xref` and
  `rebar3 dialyzer` clean and compile under `warnings_as_errors`.
- Report against the arc's ledger rows at the end; name any deferral, no silent
  drops.

---

## Arc A1 — Lossless, comment-preserving tokenizer  *(CLOSED ✅)*

> Spec: `arc1-lexer/cc-prompt.md`; fix iteration: `arc1-lexer/cc-prompt-fix1-tqstring.md`.
> Shipped `src/r3lfe_format_lexer.erl` with the API A2 builds on:
> `tokens/1 -> {ok,[token()]} | {error,term()}`, `to_iolist/1`, and accessors
> `kind/1` `text/1` `line/1` `col/1`. `token()` is opaque; `kind()` enumerates
> the token kinds. The stream is **flat** and includes `whitespace`, `newline`,
> `line_comment`, and `block_comment` as first-class tokens.

**Context.** You are adding an LFE source formatter to the `rebar3_lfe` plugin.
The foundation is a tokenizer that, unlike LFE's own `lfe_scan` (which throws
comments and whitespace away), keeps **everything**. Read
`docs/design/022-lfe-format-provider.md` sections 2 and 4.1, and study
`_build/default/lib/lfe/src/lfe_scan.erl` (or the `lfe` dep source) — you will
**adapt its literal-tokenizing logic**, not reinvent it. Do not call `lfe_scan`
at runtime; we need positions and trivia it discards.

**Task.** Create `src/r3lfe_format_lexer.erl` exposing:

```erlang
-spec tokens(binary() | string()) -> {ok, [token()]} | {error, term()}.
-spec to_iolist([token()]) -> iolist().   %% inverse: concat of verbatim texts
```

Design a `token()` type (suggested: a record `#tok{kind, text, line, col}` with
an exported opaque or transparent type, plus accessor funs). Produce a **flat**
stream that includes whitespace, newline, and blank-line tokens as first-class
items, so that reconstruction is trivial. Comment attachment to AST nodes is a
*later* arc (A2) — A1 just emits faithful tokens.

**Token kinds to cover** (enumerate from the LFE guide / `lfe_scan`):
- structural: `( ) [ ]`
- literal openers: `#(` (tuple), `#m(` / `#M(` (map), `#b(` / `#B(` (binary),
  `#.(` (read-eval)
- prefixes: `'` `` ` `` `,` `,@` and the deprecated `#'name/arity`
- atoms/symbols including bar-quoted `|…|` (with escaped `\|`)
- numbers in every base (`123`, `#b101`, `#*101`, `#o377`, `#d99`, `#xFF`,
  `#2r1010`, `#36rZ`), floats (`1.0e10`), and char literals `#\a`, `#\x1f42d;`
- strings: normal `"…"` (with escapes), binary `#"…"`, and **triple-quoted**
  `"""…"""` / `#"""…"""` (verbatim, multi-line)
- comments: line `;…\n` and block `#| … |#` (may span lines; not nestable)
- trivia: runs of spaces/tabs, single newlines, and blank-line runs

**The one guarantee that defines done:** for any input,
`iolist_to_binary(to_iolist(element(2, tokens(Src)))) =:= unicode_binary(Src)`,
**byte-for-byte**. This is the primary test.

**Tests.** Add `test/r3lfe_format_lexer_SUITE.erl` (Common Test, matching repo
convention; tight eunit blocks inside the module are fine too). Include:
- a round-trip property/corpus covering **every** kind above, plus real LFE files
  (point it at the repo's own `_integration` `.lfe` files and a handful of
  hand-written edge cases: empty file, file with no trailing newline, file that
  is only a comment, CRLF if you choose to support it — otherwise document
  LF-only);
- kind-classification assertions on tricky cases (`#\;` char vs `;` comment;
  `#"…"` binary string vs `#(…)`; `|a;b|` bar-symbol containing a semicolon;
  triple-quote containing `"` and `#|`).

**Ledger rows to report against:** see design §8 (compile clean, tests added &
passing, lossless oracle green, no regressions, xref+dialyzer clean, files+rationale).

**Out of scope for A1:** any tree building, any pretty-printing, any provider.
Stop at a proven-lossless token stream.

---

## Arc A2 — CST with comment attachment  *(CLOSED ✅)*

> Spec: `arc2-cst/cc-prompt.md`. Shipped `src/r3lfe_format_cst.erl`. API A3 builds
> on: `parse/1 -> {ok, cst_document()} | {error,term()}`; node accessors `type/1`
> `open/1` `close/1` `prefix/1` `children/1` `leading/1` `trailing/1`
> `dangling/1`; `document_children/1` `document_dangling/1`; plus
> `significant_tokens/1` and `comments/1` (oracle helpers). Types renamed
> `cst_node/0` / `cst_document/0` (avoid built-in `node/0` clash). `trivia()` =
> `{comment, token()} | blank`. **A3 constraint surfaced here:** prefix nodes
> (`'` `` ` `` `,` `,@` `#'`) must print *glued* to their inner node (no space) —
> `#' foo/2` is rejected by the reader.

**Task.** Create `src/r3lfe_format_cst.erl`: consume `r3lfe_format_lexer:tokens/1`
and build a tree of nodes (`list`/`tuple`/`map`/`binary`/`eval`/`atom`/`number`/
`string`/`char`/`symbol`/`prefixed`), recording bracket style (`(` vs `[`).
Attach each comment and blank-line run to a node as **leading**, **trailing**, or
**dangling** trivia.

**Acceptance (corrected from the old draft — byte-for-byte belongs to A1, not
A2).** (1) **Token-preservation**: the significant (non-trivia) tokens extracted
from the CST equal the lexer's significant token sequence — no reorder/drop/add.
(2) **Comment-preservation**: the ordered comment texts in the CST equal those in
the lexer stream. (3) **AST-equivalence**: a trivia-stripped serialization of the
CST, read back through the real `lfe_scan`/`lfe_parse`, equals the original
parse. Plus attachment unit tests: `(foo) ; bar` → trailing on `foo`; `;;; sec`
above a `defun` → leading; comment before a lone `)` → dangling.

---

## Arc A3 — Printer core (generic, no special forms)  *(DRAFT)*

**Task.** Create `src/r3lfe_formatter.erl` with
`-spec format(binary()|string()) -> {ok, iolist()} | {error, term()}` that
lexes → builds CST → prints. Implement the "try flat, else break" algebra:
print a form on one line if it fits in 80 cols, else break with +2 nesting,
one argument per line. Emit leading comments on their own line at current indent;
a trailing comment forces a line break after its node; block comments verbatim.
Blank-line policy: ≤1 blank between forms, collapse runs. **No** special-form
indent table yet — every head uses the generic rule.

**Acceptance.** Idempotency (`format(format(x)) == format(x)`) and
AST-equivalence on the corpus; visually reasonable output on generic forms;
comments never lost (round-trip the corpus and diff comment sets).

---

## Arc A4 — Special-form indentation + alignment  *(DRAFT — highest craft)*

**Task.** Port the `lfe-indent.el` indent table (design §3) into the printer's
break/nest decisions: `defform` for `def*`/`defun`; `specform N` for
`case`/`let`/`if`/`lambda`/`receive`/`try`/`catch`/`call`/`:`/etc.; default
funcall alignment; head-is-a-list vertical alignment. Add the style-guide
alignment rules: `let`/`let*` one binding per line aligned under the first;
`cond`/`case` one clause per line; `defun` pattern-match clauses compact;
docstrings on their own line; maps/tuples/proplists **not** column-aligned.

**Acceptance.** Golden-file tests: the LFE style-guide examples (and a curated
fixture set) format to their canonical form; the formatter is a fixed point on
them; AST-equivalence and comment preservation still hold.

---

## Arc A5 — Provider + CLI wiring  *(DRAFT)*

**Task.** Create `src/r3lfe_prv_format.erl` modeled on `r3lfe_prv_clean` /
`r3lfe_prv_eval` (design §6). Declare `--check`/`-c` (boolean) and `--path`/`-p`
(string). `do/1`: build the file list (`--path` file-or-dir, else
`r3lfe_config:get_src_dirs/1` → `r3lfe_package:discover_files/1`); for each file
read → `r3lfe_formatter:format/1` → write-if-changed (normal) or collect-changed
(check). Return `{ok,State}` normally; in `--check`, `{error,…}` (non-zero)
listing files that would change. Register `r3lfe_prv_format` in `rebar3_lfe.erl`.

**Acceptance.** `test/r3lfe_prv_format_SUITE.erl` (CT) covering: in-place format
of a temp project, `--check` clean vs dirty (exit code), `--path` to a single
file and to a dir, default src_dir resolution. `rebar3 lfe format --check` runs
clean against the repo's `_integration` project after a one-time format pass.

---

## Arc A6 — Hardening, docs, release  *(DRAFT)*

**Task.** Edge cases (empty files, `#.(…)`, unicode symbols/strings, missing
trailing newline, very long atoms/strings that can't fit 80 cols — must not
loop or crash). Add a PropEr idempotency property (generate or sample real LFE,
assert fixed point + AST-equivalence). Update README + plugin docs with the
`format` command. Add a CHANGELOG entry. Bump `src/rebar3_lfe.app.src` `vsn` to
`0.5.5`.

**Acceptance.** Property tests green; full `rebar3 ct`/`eunit`/`xref`/`dialyzer`
clean; docs and CHANGELOG updated; version bumped.

---

## Working rhythm

1. Hand CC the arc prompt. 2. CC implements + self-reviews + reports against the
ledger. 3. Duncan shares the report here; planner verifies each ledger row
against the actual artifacts (independent audit), grades, requests fixes.
4. Iterate to full acceptance (≤5 iterations), close the arc, tighten the next
draft prompt with what we learned. 5. After A6, cut **rebar3_lfe 0.5.5**.
