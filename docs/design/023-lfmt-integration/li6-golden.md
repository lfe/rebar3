# LI-6 parity golden — r3lfe_formatter output before lfmt swap

Generated from `r3lfe_formatter:format/1` before the dep swap (Step 0).
Hashes are `erlang:phash2/1` of the output binary (portable within the
same OTP version); byte counts are exact. After the swap, format the same
inputs through the provider and compare hashes + byte counts — any diff is
output drift and must be disclosed per LI-6.

## Gallery entries (selected coverage: always-break, try, match-lambda,
## defun-match, defmodule export/import, comments, wide call, commented export)

| Entry | Input hash | Output hash | Output bytes |
|-------|-----------|-------------|--------------|
| #1  `foo-bar`             | 2758070 | 3CD39DD | 8   |
| #18 `(if …)`              | 21D4F6E | 1DA5C5  | 41  |
| #22 `(receive …)`         | 43F680E | 71D2779 | 67  |
| #23 `(try …)`             | 6685C3E | 1084A4E | 114 |
| #25 `(match-lambda …)`    | 238B494 | 3618EBA | 39  |
| #35 `(defun ack …)`       | 72D08E4 | 1B50548 | 105 |
| #38 `(defmodule maths …)` | D42A63  | 77FD96F | 77  |
| #39 `(defmodule client …)`| 2F04D29 | 9F24D3  | 138 |
| #48 leading comment       | 64965B9 | 6C53004 | 43  |
| #49 trailing comment      | 340C79A | 1128630 | 34  |
| #51 dangling comment      | 4D5FED  | 125FE6E | 45  |
| #53 head trailing comment | 5622550 | 6A71750 | 27  |
| #56 wide call             | 7FECADA | 4D232DD | 167 |
| #63 commented export      | 700BE7  | 526BFF6 | 50  |

## File corpus

| File | Output hash | Output bytes |
|------|-------------|--------------|
| `test/r3lfe_format_lexer_SUITE_data/tq_corpus.lfe` | 3F1799D | 264 |
| `./_integration/myproj/src/myproj.lfe`             | 6D5D432 | 262 |
| `./_integration/myproj/test/myproj-tests.lfe`      | 1605503 | 207 |
| `./_integration/myproj/include/records.lfe`         | 1A10693 | 32  |

## Verification script (post-swap)

```erlang
%% Run via escript after the swap; compare these hashes against lfmt:format/1 output.
%% Any difference → disclose per LI-6 drift caveat.
fmt(Input) ->
    case lfmt:format(Input) of
        {ok, IO} -> unicode:characters_to_binary(IO);
        {error, R} -> iolist_to_binary(io_lib:format("ERR:~p", [R]))
    end.
```
