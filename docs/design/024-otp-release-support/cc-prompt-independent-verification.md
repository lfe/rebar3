# 024 - OTP release support verification prompt

You are Codex CLI working in:

```bash
cd ~/lab/lfe/rebar3_lfe
git status --short --branch
```

Task: independently verify the 0.5.x OTP release support bug report:

> "OTP release support broken in 0.5.x"

Important scope: this means OTP releases built and run by relx/rebar3, not the
general idea of "software release" publishing.

## Collaboration Contract

This is a ledgered verification task under the collaboration framework.

Roles:

- **CC**: you are the independent implementer/investigator. You create the
  fixture, run commands, collect evidence, and write the report.
- **CDC**: Codex Desktop/user-side review will verify your report against the
  artifacts and command evidence after you hand it back.

Rules:

- Read this prompt completely before running commands.
- Work against the ledger below. Do not silently skip a row.
- Update the ledger evidence as you work, not only at the end.
- Treat `done` as proposed-done until CDC reproduces it.
- Do not modify production code unless explicitly asked. This is an
  independent verification/reporting task.
- If a row cannot be completed, mark it `deferred` with a concrete reason and
  re-entry condition.
- If a row becomes irrelevant, mark it `no-op` with a specific rationale.
- At close, walk every row in the report. Missing rows are a defect in the
  report, not an acceptable omission.
- Report exact commands, exit statuses, relevant output, temporary paths, and
  any required environment exceptions.
- If sandboxing blocks Erlang distribution, say so and either rerun that
  lifecycle command in an allowed environment or mark the affected row
  `deferred` with the re-entry condition.
- Do not overclaim. Distinguish "direct relx release works" from
  "`rebar3_lfe` wrapper works"; they are separate ledger rows.

Evidence strength labels:

- `asserted`: claim only, no evidence. Not valid for closure.
- `attested`: you ran the command and recorded evidence. This is CC's normal
  closing strength.
- `reproduced`: CDC or another independent verifier reran the verify command.
- `reconciled`: reproduced and checked against the broader repo/CI/release
  state.

## Context

The current branch is expected to be `release/0.5.x`. Recent work changed LFE
dependencies to `{lfe, "~> 2.0"}` and bumped `src/rebar3_lfe.app.src` to
`0.5.8`. Do not assume published Hex behavior is identical to the current local
branch unless you explicitly test both.

The release-related provider modules are:

- `src/r3lfe_prv_release.erl`
- `src/r3lfe_prv_run_release.erl`

The current release template is:

- `priv/templates/rebar.config.release.tpl`
- `priv/templates/lfe-release.template`

Existing release tests are mostly helper-level and do not fully prove a real
OTP release lifecycle:

- `test/r3lfe_prv_release_SUITE.erl`
- `test/r3lfe_prv_run_release_SUITE.erl`

## Required Test Fixture

Create a fresh temporary OTP umbrella project outside the repo, under
`/private/tmp` or `/tmp`.

The fixture must contain multiple LFE applications under `apps/`, for example:

```text
apps/
  otp_root/
    src/otp_root.app.src
    src/otp_root_app.lfe
    src/otp_root_sup.lfe
    src/otp_root_server.lfe
  otp_lib_a/
    src/otp_lib_a.app.src
    src/otp_lib_a.lfe
  otp_lib_b/
    src/otp_lib_b.app.src
    src/otp_lib_b.lfe
```

Requirements:

- `otp_root` is an OTP application with `{mod, {otp_root_app, []}}`.
- `otp_root_app:start/2` starts a supervisor.
- The supervisor starts a `gen_server`.
- The server exposes `ping/0`.
- `ping/0` calls into both sibling libraries, so release boot proves all three
  local apps are loaded and running together.
- `rebar.config` has relx config for a release named `otp_rel`.
- Use `{lfe, "~> 2.0"}` in `deps`.
- Use the local `rebar3_lfe` branch as the plugin, preferably through
  `_checkouts/rebar3_lfe` or another auditable local mechanism.

Avoid relying only on provider unit tests. The core question is whether a
generated OTP release can be built and run.

## Ledger

Fill this table in as you work. Preserve every row in the final report.

| ID | Criterion | Verify | Significance | Origin | Status | Evidence | Notes |
|----|-----------|--------|--------------|--------|--------|----------|-------|
| OTP-1 | Workspace state recorded before testing | `git status --short --branch`; `git rev-parse --short HEAD`; `rebar3 --version`; `erl -eval 'io:format("~s~n", [erlang:system_info(otp_release)]), halt().' -noshell` | correctness | prompt | open | | |
| OTP-2 | Fixture is a real multi-app OTP umbrella with root app plus two sibling LFE libs | `find <fixture>/apps -maxdepth 3 -type f | sort`; inspect `.app.src` and `.lfe` modules | serious | prompt | open | | |
| OTP-3 | Fixture uses LFE 2.x-compatible dependency and local current-branch plugin | inspect fixture `rebar.config`; inspect `_checkouts` or equivalent plugin mechanism | correctness | prompt | open | | |
| OTP-4 | `rebar3 compile` compiles all LFE apps and writes module lists into generated `.app` files | `rebar3 compile`; inspect `_build/default/lib/*/ebin/*.app` | serious | prompt | open | | |
| OTP-5 | `rebar3 lfe release` assembles a relx OTP release | `rebar3 lfe release`; inspect `_build/default/rel/otp_rel/bin/otp_rel` and release files | serious | prompt | open | | |
| OTP-6 | Direct release script starts and responds to health commands | direct `otp_rel daemon`, `otp_rel ping`, `otp_rel status`, `otp_rel stop` | serious | prompt | open | | |
| OTP-7 | Direct release script can evaluate a call into the running LFE supervision tree | direct `otp_rel eval 'otp_root_server:ping().'` returns the expected value | serious | prompt | open | | |
| OTP-8 | Direct release script supports an argument-bearing `rpc` baseline | direct `otp_rel rpc otp_root_server ping '[]'` or corrected relx syntax if this form differs | correctness | prompt | open | | |
| OTP-9 | `rebar3 lfe run-release` wrapper can start, ping/status, and stop the release | wrapper `start`, `ping`, `status`, `stop`, with direct `ping` confirmation as needed | serious | prompt | open | | |
| OTP-10 | `run-release eval` forwards its expression argument correctly | compare direct `eval 'otp_root_server:ping().'` with wrapper `eval 'otp_root_server:ping().'` | serious | local finding | open | | |
| OTP-11 | `run-release rpc` forwards all required arguments correctly | compare direct `rpc ...` with wrapper `rpc ...` | correctness | local finding | open | | |
| OTP-12 | Findings are tied to code-level causes where possible | inspect `src/r3lfe_prv_run_release.erl` and cite exact lines for confirmed defects | correctness | prompt | open | | |
| OTP-13 | Report includes row-by-row closure and no silent drops | compare this opening ledger row count to final report ledger walk | correctness | ledger discipline | open | | |
| OTP-14 | Cleanup is verified | direct `otp_rel ping` after stop returns not-running, or equivalent process check | serious | prompt | open | | |

## Commands To Exercise

Run and record exact commands, exit status, and important output.

Build path:

```bash
rebar3 compile
rebar3 lfe release
```

Inspect generated app metadata:

```bash
find _build/default/lib -maxdepth 3 -type f -name '*.app' | sort
sed -n '1,120p' _build/default/lib/otp_root/ebin/otp_root.app
sed -n '1,120p' _build/default/lib/otp_lib_a/ebin/otp_lib_a.app
sed -n '1,120p' _build/default/lib/otp_lib_b/ebin/otp_lib_b.app
```

Direct release script baseline:

```bash
_build/default/rel/otp_rel/bin/otp_rel daemon
_build/default/rel/otp_rel/bin/otp_rel ping
_build/default/rel/otp_rel/bin/otp_rel eval 'otp_root_server:ping().'
_build/default/rel/otp_rel/bin/otp_rel stop
```

Plugin wrapper lifecycle:

```bash
rebar3 lfe run-release start
rebar3 lfe run-release ping
rebar3 lfe run-release status
rebar3 lfe run-release eval 'otp_root_server:ping().'
rebar3 lfe run-release stop
```

Also test at least one argument-bearing command besides `eval`, such as:

```bash
_build/default/rel/otp_rel/bin/otp_rel rpc otp_root_server ping '[]'
rebar3 lfe run-release rpc otp_root_server ping '[]'
```

## Known Finding To Verify Or Refute

One local exploratory run found that the direct release script works, including:

```text
_build/default/rel/otp_rel/bin/otp_rel eval 'otp_root_server:ping().'
=> {ok, "alpha-bravo"}
```

But the wrapper appeared to drop command arguments:

```text
rebar3 lfe run-release eval 'otp_root_server:ping().'
=> Running release command: eval
=> {error, "Incomplete form (missing .<cr>)??"}
```

Check `src/r3lfe_prv_run_release.erl`, especially:

- `get_command/1`
- `build_command_line/2`

The suspected defect is that only the first command word is passed to the
release script, so `eval`, `rpc`, `upgrade`, `downgrade`, and similar subcommands
lose their required arguments.

## Things To Watch

- If Erlang distribution fails with `Protocol 'inet_tcp': register/listen error:
  eperm`, that is likely the execution sandbox blocking local distribution
  sockets. Re-run only the release lifecycle commands in an environment allowed
  to open local Erlang distribution ports, and document that boundary.
- relx may warn that source code is not found for LFE modules, e.g.
  `Source code not found: otp_root_server.erl`. Treat that as a warning unless
  release assembly or runtime behavior fails.
- `rebar3 lfe run-release <cmd>` depends on `{lfe, release}`, so each wrapper
  command may rebuild the release. Record this behavior and whether it is merely
  slow/noisy or actually harmful.
- Be careful to stop any release node you start.

## Report Format

Write the final report to:

```text
docs/design/024-otp-release-support/report.md
```

Use this structure:

```markdown
# OTP Release Support Verification Report

Date:
Repo:
Branch:
Commit:
Tester:
Host OTP/Rebar3:
Fixture path:
Review role: CC attestation for CDC reproduction

## Executive Summary

- Verdict:
- Confirmed bugs:
- Not bugs / environmental issues:
- Recommended next step:
- Rows done/deferred/no-op:

## Fixture

Describe the temporary project layout and include the exact `rebar.config`
relx section. State whether dependencies/plugins came from Hex, local checkout,
or both.

## Ledger Closure

Walk every row from the opening ledger:

| ID | Final status | Evidence strength | Evidence | Notes |
|----|--------------|-------------------|----------|-------|
| OTP-1 |  |  |  |  |
| OTP-2 |  |  |  |  |
| OTP-3 |  |  |  |  |
| OTP-4 |  |  |  |  |
| OTP-5 |  |  |  |  |
| OTP-6 |  |  |  |  |
| OTP-7 |  |  |  |  |
| OTP-8 |  |  |  |  |
| OTP-9 |  |  |  |  |
| OTP-10 |  |  |  |  |
| OTP-11 |  |  |  |  |
| OTP-12 |  |  |  |  |
| OTP-13 |  |  |  |  |
| OTP-14 |  |  |  |  |

For each `done` row, evidence strength should usually be `attested`; CDC will
upgrade rows to `reproduced` only after independent reruns. Do not use
`asserted` for any final `done` row.

## Command Evidence

For each command, include:

- command
- exit status
- relevant output
- pass/fail assessment

At minimum cover:

- `rebar3 compile`
- `rebar3 lfe release`
- direct release `daemon`, `ping`, `eval`, `rpc`, `stop`
- wrapper `run-release start`, `ping`, `status`, `eval`, `rpc`, `stop`

## Findings

Order by severity. Include file/line references for any code-level defect.
For each confirmed defect, state:

- symptom
- minimal reproduction command
- direct-release baseline result, if applicable
- wrapper result, if applicable
- suspected root cause
- affected user workflow
- severity

Use severity labels: `critical`, `serious`, `correctness`, `polish`.

## Proposed Fix

Describe the smallest code change that should fix the confirmed issue. Include
test recommendations, especially a real e2e test under `test/e2e/`.

## Report Back To CDC

End the report with a short handoff block addressed to CDC:

```text
CDC handoff:
- report path:
- fixture path:
- branch/commit tested:
- highest-severity finding:
- rows needing reproduction:
- rows deferred, with re-entry condition:
- temp release stopped: yes/no, evidence:
```

This handoff should let CDC reproduce the strongest finding without reading the
whole report first.

## Cleanup

State how you confirmed the release node was stopped and where temporary files
were left, if kept.
```

## Final Response In Chat

After writing `report.md`, reply in chat with:

- the report path
- the fixture path
- a one-paragraph verdict
- the ledger row counts
- any blocked/deferred rows and their re-entry condition
- confirmation that the OTP release node was stopped

Do not paste the whole report into chat unless asked.
