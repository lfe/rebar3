# OTP Release Support Verification Report

Date: 2026-08-08
Repo: /Users/oubiwann/lab/lfe/rebar3_lfe
Branch: release/0.5.x
Commit: 58882f9
Tester: Codex CLI
Host OTP/Rebar3: Erlang/OTP 28, rebar3 3.27.0 on Erts 16.1.1
Fixture path: /private/tmp/rebar3-lfe-otp-release-verify-20260808
Review role: CC attestation for CDC reproduction

## Executive Summary

- Verdict: direct relx/rebar3 OTP release support works for a real multi-app LFE umbrella on this branch, but `rebar3 lfe run-release` is broken for argument-bearing release commands.
- Confirmed bugs: `run-release eval ...` and `run-release rpc ...` drop all arguments after the first command word. The wrapper also masks non-zero release-script exit statuses and returns exit 0 after warning.
- Not bugs / environmental issues: initial direct release startup inside the managed sandbox failed with `Protocol 'inet_tcp': register/listen error: eperm`; rerunning lifecycle commands outside the sandbox succeeded. relx source-not-found warnings for LFE modules did not prevent assembly or runtime behavior.
- Recommended next step: change `r3lfe_prv_run_release` to preserve and quote/pass all command arguments, and return an error when the underlying release command exits non-zero. Add an e2e release fixture test for direct-vs-wrapper `eval` and `rpc`.
- Rows done/deferred/no-op: 14 done, 0 deferred, 0 no-op.

## Fixture

The fixture is a temporary OTP umbrella at `/private/tmp/rebar3-lfe-otp-release-verify-20260808`.

It contains three LFE applications:

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

`otp_root` is an OTP application with `{mod, {otp_root_app, []}}`. Its application callback starts `otp_root_sup`; the supervisor starts `otp_root_server`; `otp_root_server:ping/0` calls `otp_lib_a:word/0` and `otp_lib_b:word/0` and returns `{ok, "alpha-bravo"}`.

Dependencies/plugins:

- `{lfe, "~> 2.0"}` came from cached Hex package `lfe-2.2.2`.
- `rebar3_lfe` came from local checkout symlink `_checkouts/rebar3_lfe -> /Users/oubiwann/lab/lfe/rebar3_lfe`.
- `lfmt-0.4.0`, needed by the local plugin, came from the cached Hex package.

Exact relx section:

```erlang
{relx, [
    {release, {otp_rel, "0.1.0"}, [
        otp_root,
        otp_lib_a,
        otp_lib_b,
        lfe,
        sasl
    ]},

    {sys_config, "./config/sys.config"},
    {vm_args, "./config/vm.args"},

    {dev_mode, true},
    {include_erts, false},
    {extended_start_script, true}
]}.
```

## Ledger Closure

| ID | Final status | Evidence strength | Evidence | Notes |
|----|--------------|-------------------|----------|-------|
| OTP-1 | done | attested | `git status --short --branch` exit 0: `## release/0.5.x...origin/release/0.5.x` and `?? docs/design/024-otp-release-support/`; `git rev-parse --short HEAD` exit 0: `58882f9`; `rebar3 --version` exit 0: `rebar 3.27.0 on Erlang/OTP 28 Erts 16.1.1`; OTP command exit 0: `28`. | Workspace had the design directory untracked before report writing. |
| OTP-2 | done | attested | `find /private/tmp/rebar3-lfe-otp-release-verify-20260808/apps -maxdepth 3 -type f | sort` exit 0 listed the 8 required app source files. Inspected `otp_root.app.src`, `otp_root_app.lfe`, `otp_root_sup.lfe`, `otp_root_server.lfe`, and sibling library modules. | Fixture is a real multi-app umbrella. |
| OTP-3 | done | attested | `sed -n '1,180p' rebar.config` showed `{lfe, "~> 2.0"}`, `{project_plugins, [rebar3_lfe]}`, provider hook, and relx config. `find _checkouts -maxdepth 1 -type l -ls` showed `_checkouts/rebar3_lfe -> /Users/oubiwann/lab/lfe/rebar3_lfe`. | Local current-branch plugin is auditable. |
| OTP-4 | done | attested | `rebar3 compile` exit 0 compiled `otp_lib_a`, `otp_lib_b`, `otp_root_sup`, `otp_root_app`, and `otp_root_server`. Generated `.app` files contained `{modules,[otp_lib_a]}`, `{modules,[otp_lib_b]}`, and `{modules,[otp_root_app,otp_root_server,otp_root_sup]}`. | Cached Hex fallback was used after download errors. |
| OTP-5 | done | attested | `rebar3 lfe release` exit 0 ended with `Release successfully assembled: _build/default/rel/otp_rel`. `ls -l _build/default/rel/otp_rel/bin/otp_rel` showed executable mode; `find _build/default/rel/otp_rel -maxdepth 3 -type f` listed bin and release files. | LFE source-not-found warnings were non-blocking. |
| OTP-6 | done | attested | Direct `_build/default/rel/otp_rel/bin/otp_rel daemon` outside sandbox exit 0; direct `ping` exit 0 output `pong`; direct `status` exit 0 with no output; direct `stop` exit 0. | First in-sandbox daemon attempt hung and log showed `inet_tcp ... eperm`; lifecycle commands were rerun outside sandbox. |
| OTP-7 | done | attested | Direct `_build/default/rel/otp_rel/bin/otp_rel eval 'otp_root_server:ping().'` exit 0 output `{ok, "alpha-bravo"}`. | Proves running supervision tree and both sibling libs. |
| OTP-8 | done | attested | Direct `_build/default/rel/otp_rel/bin/otp_rel rpc otp_root_server ping '[]'` exit 0 output `{ok, "alpha-bravo"}`. | Direct argument-bearing RPC baseline works. |
| OTP-9 | done | attested | `rebar3 lfe run-release start` exit 0, direct `ping` after start output `pong`; wrapper `ping` exit 0 output `pong`; wrapper `status` exit 0 with no status text; wrapper `stop` exit 0; final direct `ping` exit 1 output `Node is not running!`. | Wrapper single-word lifecycle commands work, but every wrapper command rebuilds the release first. |
| OTP-10 | done | attested | Direct eval baseline returned `{ok, "alpha-bravo"}`. Wrapper `rebar3 lfe run-release eval 'otp_root_server:ping().'` exit 0 output `Running release command: eval` then `{error, "Incomplete form (missing .<cr>)??"}`. | Criterion failed; defect confirmed. |
| OTP-11 | done | attested | Direct rpc baseline returned `{ok, "alpha-bravo"}`. Wrapper `rebar3 lfe run-release rpc otp_root_server ping '[]'` process exit 0, but output showed `Running release command: rpc`, `erl_call: wrong format of apply string (1)`, and `Command exited with status: 1`. | Criterion failed; defect confirmed. Wrapper masks underlying non-zero status. |
| OTP-12 | done | attested | `nl -ba src/r3lfe_prv_run_release.erl` showed `get_command/1` keeps `[Command | _]` at lines 119-127, `build_command_line/2` joins only `[ReleaseScript, Command]` at lines 219-224, and `collect_output/1` warns then returns `ok` for non-zero exit status at lines 283-287 and 291-298. | Code-level cause confirmed. |
| OTP-13 | done | attested | Opening ledger had 14 rows, OTP-1 through OTP-14. This report walks all 14 rows. | No silent drops. |
| OTP-14 | done | attested | Final `_build/default/rel/otp_rel/bin/otp_rel ping` outside sandbox exit 1 output `Node is not running!`. | Release node stopped. |

## Command Evidence

### Workspace/toolchain

- Command: `git status --short --branch`
  Exit status: 0
  Output: `## release/0.5.x...origin/release/0.5.x`; `?? docs/design/024-otp-release-support/`
  Assessment: pass.
- Command: `git rev-parse --short HEAD`
  Exit status: 0
  Output: `58882f9`
  Assessment: pass.
- Command: `rebar3 --version`
  Exit status: 0
  Output: `rebar 3.27.0 on Erlang/OTP 28 Erts 16.1.1`
  Assessment: pass.
- Command: `erl -eval 'io:format("~s~n", [erlang:system_info(otp_release)]), halt().' -noshell`
  Exit status: 0
  Output: `28`
  Assessment: pass.

### Fixture inspection

- Command: `find /private/tmp/rebar3-lfe-otp-release-verify-20260808/apps -maxdepth 3 -type f | sort`
  Exit status: 0
  Relevant output: listed `otp_root.app.src`, `otp_root_app.lfe`, `otp_root_sup.lfe`, `otp_root_server.lfe`, `otp_lib_a.app.src`, `otp_lib_a.lfe`, `otp_lib_b.app.src`, `otp_lib_b.lfe`.
  Assessment: pass.
- Command: `find /private/tmp/rebar3-lfe-otp-release-verify-20260808/_checkouts -maxdepth 1 -type l -ls`
  Exit status: 0
  Output: `_checkouts/rebar3_lfe -> /Users/oubiwann/lab/lfe/rebar3_lfe`
  Assessment: pass.

### Build path

- Command: `rebar3 compile`
  Exit status: 0
  Relevant output: fetched `lfmt v0.4.0` and `lfe v2.2.2` from cached Hex tarballs after download errors; compiled `lfe`; compiled 3 LFE applications and all five fixture LFE modules; compiled `otp_lib_b`, `otp_lib_a`, `otp_root`.
  Assessment: pass.
- Command: `find _build/default/lib -maxdepth 3 -type f -name '*.app' | sort`
  Exit status: 0
  Relevant output: listed generated `.app` files for `lfe`, `otp_lib_a`, `otp_lib_b`, and `otp_root`.
  Assessment: pass.
- Command: `sed -n '1,120p' _build/default/lib/otp_root/ebin/otp_root.app`
  Exit status: 0
  Relevant output: `{modules,[otp_root_app,otp_root_server,otp_root_sup]}`.
  Assessment: pass.
- Command: `sed -n '1,120p' _build/default/lib/otp_lib_a/ebin/otp_lib_a.app`
  Exit status: 0
  Relevant output: `{modules,[otp_lib_a]}`.
  Assessment: pass.
- Command: `sed -n '1,120p' _build/default/lib/otp_lib_b/ebin/otp_lib_b.app`
  Exit status: 0
  Relevant output: `{modules,[otp_lib_b]}`.
  Assessment: pass.
- Command: `rebar3 lfe release`
  Exit status: 0
  Relevant output: `Assembling release otp_rel-0.1.0...`; LFE source-not-found warnings; `Release successfully assembled: _build/default/rel/otp_rel`.
  Assessment: pass.

### Direct release script baseline

- Command: `_build/default/rel/otp_rel/bin/otp_rel daemon`
  Exit status: 130 for first sandbox attempt; then 0 outside sandbox.
  Relevant output: sandbox attempt produced no command output and was interrupted; logs contained `Protocol 'inet_tcp': register/listen error: eperm`. Outside-sandbox rerun exited 0.
  Assessment: pass outside sandbox; sandbox environment cannot run Erlang distribution lifecycle.
- Command: `_build/default/rel/otp_rel/bin/otp_rel ping`
  Exit status: 0
  Output: `pong`
  Assessment: pass.
- Command: `_build/default/rel/otp_rel/bin/otp_rel status`
  Exit status: 0
  Output: none.
  Assessment: pass with note that relx status was silent on this host.
- Command: `_build/default/rel/otp_rel/bin/otp_rel eval 'otp_root_server:ping().'`
  Exit status: 0
  Output: `{ok, "alpha-bravo"}`
  Assessment: pass.
- Command: `_build/default/rel/otp_rel/bin/otp_rel rpc otp_root_server ping '[]'`
  Exit status: 0
  Output: `{ok, "alpha-bravo"}`
  Assessment: pass.
- Command: `_build/default/rel/otp_rel/bin/otp_rel stop`
  Exit status: 0
  Output: none.
  Assessment: pass.
- Command: `_build/default/rel/otp_rel/bin/otp_rel ping`
  Exit status: 1
  Output: `Node is not running!`
  Assessment: pass for cleanup.

### Plugin wrapper lifecycle

Each wrapper command rebuilt the release first because `r3lfe_prv_run_release` declares `{deps, [{?NAMESPACE, release}]}`. The repeated prelude included compiling the local plugin, verifying deps, compiling the three LFE apps, assembling `otp_rel-0.1.0`, and the same relx LFE source-not-found warnings.

- Command: `rebar3 lfe run-release start`
  Exit status: 0
  Relevant output: `Running release command: start`; relx warning that `start` is deprecated and replaced by `daemon`.
  Assessment: pass.
- Command: `_build/default/rel/otp_rel/bin/otp_rel ping`
  Exit status: 0
  Output: `pong`
  Assessment: pass; confirms wrapper start created a running node.
- Command: `rebar3 lfe run-release ping`
  Exit status: 0
  Relevant output: `Running release command: ping`; `pong`.
  Assessment: pass.
- Command: `rebar3 lfe run-release status`
  Exit status: 0
  Relevant output: `Running release command: status`; no status text.
  Assessment: pass with silent-status caveat.
- Command: `rebar3 lfe run-release eval 'otp_root_server:ping().'`
  Exit status: 0
  Relevant output: `Running release command: eval`; `{error, "Incomplete form (missing .<cr>)??"}`.
  Assessment: fail; wrapper drops eval expression argument.
- Command: `rebar3 lfe run-release rpc otp_root_server ping '[]'`
  Exit status: 0
  Relevant output: `Running release command: rpc`; `erl_call: wrong format of apply string (1)`; `Command exited with status: 1`.
  Assessment: fail; wrapper drops RPC arguments and masks underlying non-zero status.
- Command: `rebar3 lfe run-release stop`
  Exit status: 0
  Relevant output: `Running release command: stop`.
  Assessment: pass.
- Command: `_build/default/rel/otp_rel/bin/otp_rel ping`
  Exit status: 1
  Output: `Node is not running!`
  Assessment: pass for cleanup.

## Findings

### serious: `run-release` drops arguments for argument-bearing commands

- Symptom: `rebar3 lfe run-release eval 'otp_root_server:ping().'` does not pass the expression to the release script.
- Minimal reproduction command: `rebar3 lfe run-release eval 'otp_root_server:ping().'`
- Direct-release baseline result: `_build/default/rel/otp_rel/bin/otp_rel eval 'otp_root_server:ping().'` returned `{ok, "alpha-bravo"}`.
- Wrapper result: wrapper printed `Running release command: eval` and returned `{error, "Incomplete form (missing .<cr>)??"}`.
- Suspected root cause: `get_command/1` discards all arguments after the first command word in `src/r3lfe_prv_run_release.erl:119-127`; `build_command_line/2` builds only `ReleaseScript Command` in `src/r3lfe_prv_run_release.erl:219-224`.
- Affected user workflow: any `run-release` command needing arguments, including `eval`, `rpc`, `rpcterms`, `upgrade`, and `downgrade`.
- Severity: serious.

### correctness: `run-release rpc` drops required module/function/args and masks failure

- Symptom: `rebar3 lfe run-release rpc otp_root_server ping '[]'` invokes only `rpc`.
- Minimal reproduction command: `rebar3 lfe run-release rpc otp_root_server ping '[]'`
- Direct-release baseline result: `_build/default/rel/otp_rel/bin/otp_rel rpc otp_root_server ping '[]'` returned `{ok, "alpha-bravo"}`.
- Wrapper result: wrapper printed `Running release command: rpc`, `erl_call: wrong format of apply string (1)`, and `Command exited with status: 1`, but the outer `rebar3` process exited 0.
- Suspected root cause: same argument truncation at `src/r3lfe_prv_run_release.erl:119-127` and `src/r3lfe_prv_run_release.erl:219-224`; non-zero status is warned and converted to `ok` at `src/r3lfe_prv_run_release.erl:283-287` and `src/r3lfe_prv_run_release.erl:291-298`.
- Affected user workflow: scripted release management cannot rely on wrapper exit status for argument-bearing command failures.
- Severity: correctness.

### polish: wrapper lifecycle commands rebuild the release every time

- Symptom: every `rebar3 lfe run-release <cmd>` invocation repeated compile and release assembly before running the requested command.
- Minimal reproduction command: `rebar3 lfe run-release ping`
- Direct-release baseline result: direct `_build/default/rel/otp_rel/bin/otp_rel ping` returned immediately with `pong`.
- Wrapper result: wrapper rebuilt the release, then ran `ping`.
- Suspected root cause: `r3lfe_prv_run_release` declares `-define(DEPS, [{?NAMESPACE, release}]).` and registers `{deps, ?DEPS}` at `src/r3lfe_prv_run_release.erl:26-43`.
- Affected user workflow: release management commands are slower/noisier than direct relx scripts. This did not break the lifecycle in this fixture.
- Severity: polish.

## Proposed Fix

Smallest behavior fix:

- Change the provider to preserve `rebar_state:command_args(State)` as the command plus remaining arguments.
- Validate only the first command word.
- Build the release script invocation from the full argv list, with shell-safe quoting or by using a port spawn mode that does not require manual shell string construction.
- Return `{error, ...}` from the provider when the underlying release command exits non-zero instead of warning and returning `ok`.

Recommended tests:

- Add an e2e release fixture under `test/e2e/` that builds a small multi-app LFE umbrella release and compares direct release script behavior with wrapper behavior.
- Cover `run-release eval 'otp_root_server:ping().'` and `run-release rpc otp_root_server ping '[]'`.
- Assert both stdout and the outer `rebar3` exit status for failed underlying release commands.
- Keep helper-level unit tests for `get_command`/argument preservation and command-line construction, but do not rely on them alone.

## Report Back To CDC

CDC handoff:
- report path: docs/design/024-otp-release-support/report.md
- fixture path: /private/tmp/rebar3-lfe-otp-release-verify-20260808
- branch/commit tested: release/0.5.x / 58882f9
- highest-severity finding: serious - `rebar3 lfe run-release` drops arguments after the first command word; `eval` fails while direct release eval works
- rows needing reproduction: OTP-1 through OTP-14, especially OTP-7/OTP-8 direct baselines and OTP-10/OTP-11 wrapper failures
- rows deferred, with re-entry condition: none
- temp release stopped: yes, evidence: final `_build/default/rel/otp_rel/bin/otp_rel ping` exit 1 output `Node is not running!`

## Cleanup

The release node was stopped with `rebar3 lfe run-release stop` and verified with direct `_build/default/rel/otp_rel/bin/otp_rel ping`, which exited 1 and printed `Node is not running!`.

Temporary files were intentionally left at `/private/tmp/rebar3-lfe-otp-release-verify-20260808` for CDC reproduction. The fixture includes build artifacts, logs, and the `_checkouts/rebar3_lfe` symlink to the tested checkout.
