# rebar3_lfe -- standing session instructions

`rebar3_lfe` is the rebar3 plugin for LFE projects. It provides the
`rebar3 lfe ...` command namespace, LFE compilation integration, project
templates, release/escript/run helpers, and the LFE formatter work under
`docs/design/022-lfe-format/`.

- Work on the active release branch unless the operator says otherwise.
  Current observed branch: `release/0.5.x`.
- Read `README.md`, `docs/commands.md`, and the relevant design note under
  `docs/design/` before making non-trivial behavior changes.
- For Erlang source changes, follow the repository's existing rebar3 provider
  style and run focused Common Test suites where practical.
- For template smoke checks, remember that generated projects compile through
  normal `rebar3 compile`; the templates install LFE compilation as a
  pre-compile hook.
- `Makefile` targets that touch template integration may create or push the
  `integration-testing` branch. Do not run those targets casually; reproduce
  them in `/tmp` when a branch push is not explicitly desired.
- **Commit footer convention (operator override, 2026-08-08):** every future
  assistant-authored commit message includes these trailers:
  `Co-authored-by: Codex <noreply@openai.com>` and
  `Co-authored-by: Billo AI <ai-engineering@billo.systems>`.
