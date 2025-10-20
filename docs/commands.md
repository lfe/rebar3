# Command Reference

Complete reference for all r3lfe commands.

## Core Commands

### compile

Compile LFE source files.

```bash
rebar3 lfe compile [OPTIONS]
```

**Features:**
- Incremental compilation (only changed files)
- Header dependency tracking
- Package support (nested modules)
- Parallel compilation
- Progress reporting

**Options:**
- `--verbose` - Detailed output

**Examples:**
```bash
# Basic compilation
rebar3 lfe compile

# Verbose output
rebar3 lfe compile --verbose

# Specific profile
rebar3 as prod lfe compile
```

### clean

Remove compiled files.

```bash
rebar3 lfe clean
```

**What it removes:**
- All `.beam` files in `ebin/`
- Temporary package files
- Cached dependency information

**Examples:**
```bash
# Clean current project
rebar3 lfe clean

# Clean all (including deps)
rebar3 clean
```

### repl

Start an LFE REPL.

```bash
rebar3 lfe repl [OPTIONS]
```

**Features:**
- Project and deps on code path
- Optional app startup
- Script execution
- Distributed node support

**Options:**
- `--name NAME` - Long node name
- `--sname NAME` - Short node name
- `--setcookie COOKIE` - Node cookie
- `--apps APPS` - Comma-separated apps to start
- `--script PATH` - Script to run before REPL

**Examples:**
```bash
# Basic REPL
rebar3 lfe repl

# With node name
rebar3 lfe repl --sname mynode

# Start specific apps
rebar3 lfe repl --apps myapp,mylib

# Run initialization script
rebar3 lfe repl --script init.lfe
```

**In REPL:**
```lfe
> (help)                    ; Show help
> (myapp:start)             ; Call your code
> (c "src/myapp.lfe")       ; Recompile file
> (q)                       ; Quit
```

### ltest

Run tests using ltest framework.

```bash
rebar3 lfe ltest [OPTIONS]
```

**Requirements:**
- `{ltest, "~> 0.13"}` in deps

**Options:**
- `--suite NAME` - Specific test suite
- `--test NAME` - Specific test
- `--verbose` - Detailed output

**Examples:**
```bash
# Run all tests
rebar3 lfe ltest

# Run specific suite
rebar3 lfe ltest --suite integration

# Verbose output
rebar3 lfe ltest --verbose
```

### release

Build an OTP release.

```bash
rebar3 lfe release [OPTIONS]
```

**Requirements:**
- relx configuration in `rebar.config`

**Examples:**
```bash
# Build release
rebar3 lfe release

# Production release
rebar3 as prod lfe release

# Create tar
rebar3 as prod lfe release tar
```

**Configuration:**
```erlang
{relx, [
    {release, {myapp, "0.1.0"}, [
        myapp,
        sasl
    ]},
    {dev_mode, false},
    {include_erts, true}
]}.
```

### versions

Display version information.

```bash
rebar3 lfe versions
```

**Shows:**
- Project applications and versions
- LFE version
- Erlang/OTP version
- rebar3 version
- r3lfe version

**Example output:**
```
=== Project Applications ===
  myapp                0.1.0

=== Languages ===
  lfe                  2.2.0
  erlang               27
  erts                 14.2.1

=== Build Tools ===
  rebar3               3.25.0
  r3lfe               0.5.0
```

## Script Commands

### run

Run an LFE project's main/1 function.

```bash
rebar3 lfe run [OPTIONS] [-- ARGS...]
```

**Use Cases:**
- Quick testing during development
- Running utility scripts
- Ad-hoc data processing
- Project maintenance tasks

**Options:**
- `--main FILE` or `-m FILE` - Path to LFE file with main/1
- `--script FILE` or `-s FILE` - Alias for --main (compatibility)

**Configuration:**
```erlang
%% In rebar.config
{lfe, [
    {main, "scripts/process.lfe"}
]}.
```

**Examples:**
```bash
# Run configured main
rebar3 lfe run

# Specify file
rebar3 lfe run --main scripts/analyze.lfe

# Pass arguments
rebar3 lfe run -- input.txt output.txt

# Full example
rebar3 lfe run -m scripts/calc.lfe -- 1 2 3 add
```

**Main Function Signature:**
```lfe
(defmodule myscript)

(defun main (args)
  "Args is a list of binaries: [<<\"arg1\">>, <<\"arg2\">>, ...]"
  (io:format "Received ~p arguments~n" (list (length args)))
  ;; Process args
  0)  ; Return exit code
```

## Escript Commands

### escriptize

Build a standalone executable escript.

```bash
rebar3 lfe escriptize
```

**Requirements in rebar.config:**
```erlang
{escript_main_app, myapp}.
{escript_name, "myapp"}.
{escript_emu_args, "%%! +sbtu +A1\n"}.
```

**Main module requirements:**
```lfe
(defmodule myapp
  (export (main 1)))

(defun main (args)
  ;; Your logic here
  0)
```

**Output:**
```
_build/default/bin/myapp  # Standalone executable
```

**Examples:**
```bash
# Build escript
rebar3 lfe escriptize

# Run directly
./_build/default/bin/myapp arg1 arg2

# Or via run-escript
rebar3 lfe run-escript -- arg1 arg2
```

### run-escript

Execute a built escript.

```bash
rebar3 lfe run-escript [-- ARGS...]
```

**Features:**
- Automatically finds built escript
- Captures and displays output
- Passes through exit codes

**Examples:**
```bash
# Run without arguments
rebar3 lfe run-escript

# With arguments
rebar3 lfe run-escript -- --verbose --output file.txt

# Full workflow
rebar3 lfe compile
rebar3 lfe escriptize
rebar3 lfe run-escript
```

## Release Commands

### run-release

Execute commands on a built LFE OTP release.

```bash
rebar3 lfe run-release COMMAND [ARGS...]
```

**Common Commands:**
```bash
# Start/Stop
rebar3 lfe run-release start
rebar3 lfe run-release stop
rebar3 lfe run-release restart

# Status
rebar3 lfe run-release status
rebar3 lfe run-release ping

# Console Access
rebar3 lfe run-release console
rebar3 lfe run-release remote_console
rebar3 lfe run-release attach
```

**Advanced Commands:**
```bash
# Upgrades/Downgrades
rebar3 lfe run-release upgrade 1.2.0
rebar3 lfe run-release versions

# Code Evaluation
rebar3 lfe run-release eval "application:which_applications()."
```

**Typical Workflow:**
```bash
# Build and test locally
rebar3 lfe compile
rebar3 lfe release
rebar3 lfe run-release console

# Production deployment
rebar3 as prod lfe release
rebar3 lfe run-release start
rebar3 lfe run-release status
```

## Utility Commands

### confabulate

Convert LFE data files to Erlang data files.

```bash
rebar3 lfe confabulate --input FILE [OPTIONS]
```

**Purpose:**
- Configuration file conversion
- Test data generation
- Interoperability between LFE and Erlang
- Data migration tools

**Options:**
- `--input FILE` or `-i FILE` - Input LFE file (required)
- `--output FILE` or `-o FILE` - Output Erlang file (default: input.erl)
- `--force` or `-f` - Overwrite existing output file

**Examples:**
```bash
# Basic conversion
rebar3 lfe confabulate --input data.lfe

# Custom output
rebar3 lfe confabulate -i config.lfe -o app.config

# Force overwrite
rebar3 lfe confabulate -i data.lfe --force
```

**Input (LFE):**
```lfe
;; data.lfe
(#(user "alice" "alice@example.com" 'admin)
 #(user "bob" "bob@example.com" 'user))
```

**Output (Erlang):**
```erlang
%% data.erl
{user,"alice","alice@example.com",admin}.
{user,"bob","bob@example.com",user}.
```

## Global Installation

Install r3lfe globally for use across all projects:

```bash
# Add to ~/.config/rebar3/rebar.config
{plugins, [r3lfe]}.

# Use in any project
cd any-lfe-project
rebar3 lfe compile
```
