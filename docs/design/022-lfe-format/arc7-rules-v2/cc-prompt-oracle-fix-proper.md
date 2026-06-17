# 022 · A7 · oracle finish — PropEr token-preservation must use raw lexer tokens

> Target: Sonnet 4.6. **Tiny test-only fix.** The dropped-code investigation
> hardened 2 of 3 oracle sites to compare raw lexer tokens; this finishes the 3rd.

## 0. The gap

The dropped-code investigation re-anchored token-preservation to raw
`r3lfe_format_lexer:tokens/1` so a `parse`-level drop can't blind the comparison
(self-referential oracle). Two sites were fixed — `r3lfe_formatter_SUITE`'s
`sig_pairs/1` (≈L486) and `sweep_sig_pairs/1` (≈L1821). **One was missed:**
`r3lfe_properties.erl` `fmt_sig_pairs/1` (≈L256–260) still does
`parse → r3lfe_format_cst:significant_tokens/1`, so the PropEr property
`fmt_oracle_tokens` remains blind to a parse-level token drop.

## 1. The fix

Rewrite `fmt_sig_pairs/1` in `test/r3lfe_properties.erl` to compare **raw lexer
non-trivia tokens**, exactly like `r3lfe_formatter_SUITE:sig_pairs/1`:

```erlang
fmt_sig_pairs(Bin) ->
    {ok, Toks} = r3lfe_format_lexer:tokens(Bin),
    Trivia = [whitespace, newline, line_comment, block_comment],
    [{r3lfe_format_lexer:kind(T), r3lfe_format_lexer:text(T)}
     || T <- Toks,
        not lists:member(r3lfe_format_lexer:kind(T), Trivia)].
```

Do **not** change `fmt_comments/1` (comment-preservation legitimately uses
`cst:comments/1`) or `fmt_oracle_ast/1` (AST-equivalence legitimately uses the
real reader). Only the token-preservation helper changes.

## 2. Verify

- The PropEr suite still passes.
- Sanity that the new helper is strictly more sensitive: it now lists tokens from
  the lexer directly, so a hypothetical formatter/parse drop would diff. (A quick
  way to confirm wiring: temporarily make the helper drop the first token and see
  the property fail; then revert.)
- Full `rebar3 ct` + PropEr green.

## 3. Ledger

```
A7 oracle finish — PropEr token helper
[ ] fmt_sig_pairs/1 uses raw lexer non-trivia tokens (no parse); matches SUITE
[ ] fmt_comments/1 and fmt_oracle_ast/1 unchanged
[ ] PropEr + full ct green; (optional) sensitivity spot-check noted
[ ] files changed + one-line rationale
```
