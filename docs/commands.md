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

### format

Reformat LFE source files to LFE style conventions.

```bash
rebar3 lfe format [OPTIONS]
```

**What it does:**
Reformats every `.lfe` file in the project's source directories (default: `src/`)
to the LFE style conventions: 80-column width, 2-space indentation, the
`lfe-indent.el`-derived special-form table. The formatter is:

- **Comment-preserving** — all comments (leading, trailing, block) are kept in
  place
- **Idempotent** — formatting an already-formatted file is a no-op
- **Token-preserving** — no identifiers, literals, or punctuation are added,
  removed, or altered (export/import entry sorting is the one intentional
  reorder)
- **AST-equivalent** — the formatted output parses to the same Lisp structure
- **Zero new runtime dependencies**

**Behavior (knowledge-gated layout):**
Known LFE special forms (`defun`, `let`, `case`, `cond`, `if`, `try`, etc.) are
laid out canonically per the `lfe-indent.el` table; unknown or data-oriented
forms preserve the author's line breaks (reindented to the correct column).
`export` and `import` clauses are always rendered one-per-line at +1 under the
keyword, alphabetically sorted by name then arity — sort is suppressed when any
entry carries a developer comment.

**Options:**
- `--dry-run` — print formatted output to stdout; no files written
- `--check` — CI mode: exit non-zero if any file is not already formatted; no
  files written
- `--path P` — restrict to a single `.lfe` file or a directory (recursive)

`--dry-run` and `--check` are mutually exclusive.

**Examples:**
```bash
# Format every .lfe file in the project (in place)
rebar3 lfe format

# Restrict to one directory (recursive)
rebar3 lfe format --path src/sub

# Restrict to one file
rebar3 lfe format --path src/mymodule.lfe

# Print formatted output to stdout, write nothing
rebar3 lfe format --dry-run

# CI gate — exit non-zero if any file is unformatted
rebar3 lfe format --check
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

### eval

Evaluate a single LFE expression from the command line.

```bash
rebar3 lfe eval '<expression>'
```

**Features:**
- Quick expression evaluation without starting a REPL
- Full LFE language support (lambdas, let, conditionals, etc.)
- Access to compiled project modules and dependencies
- Proper LFE-formatted output

**Use Cases:**
- Quick calculations and data transformations
- Testing function calls during development
- One-off data processing tasks
- CI/CD scripts and automation
- Debugging and exploration

**Examples:**
```bash
# Simple arithmetic
rebar3 lfe eval '(+ 1 2 3)'
# Output: 6

# Complex calculation
rebar3 lfe eval '(* 2 (+ 1 2 3 4 5 6))'
# Output: 42

# List operations
rebar3 lfe eval '(lists:map (lambda (x) (* x x)) (list 1 2 3 4 5))'
# Output: (1 4 9 16 25)

# Conditionals
rebar3 lfe eval '(if (> 5 3) 'yes 'no)'
# Output: yes

# Let bindings
rebar3 lfe eval '(let ((x 10) (y 20)) (+ x y))'
# Output: 30

# I/O operations
rebar3 lfe eval '(: io format "Hello from LFE!~n" ())'
# Output: Hello from LFE!
#         ok

# Call compiled project code
rebar3 lfe compile
rebar3 lfe eval '(myapp:version)'

# Use with pipes
echo "data" | rebar3 lfe eval '(: io get_line "")'
```

**Shell Quoting:**

The expression must be quoted to prevent shell interpretation:

```bash
# Good - single quotes
rebar3 lfe eval '(+ 1 2 3)'

# Good - escaped double quotes
rebar3 lfe eval "(+ 1 2 3)"

# Bad - unquoted (shell will interpret parentheses)
rebar3 lfe eval (+ 1 2 3)  # Error!
```

**Notes:**
- Expressions are evaluated in a clean LFE environment
- The compiled project and dependencies are available on the code path
- Side effects (I/O, file operations, etc.) are executed
- The result is printed in LFE format, not Erlang format
- Only a single expression is evaluated (use `progn` for multiple forms)

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
