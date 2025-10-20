# Quick Start Guide

Get up and running with rb3lfe in 5 minutes.

## Installation

### New Project

```bash
# Create project directory
mkdir my-lfe-app
cd my-lfe-app

# Create rebar.config
cat > rebar.config <<EOF
{plugins, [
    {rb3lfe, "0.5.0"}
]}.

{deps, [
    {lfe, "2.2.0"}
]}.
EOF

# Create source directory
mkdir src

# Create a simple module
cat > src/myapp.lfe <<EOF
(defmodule myapp)

(defun hello (name)
  (io:format "Hello, ~s!~n" (list name)))
EOF

# Compile
rebar3 lfe compile

# Start REPL
rebar3 lfe repl
```

In the REPL:
```lfe
> (myapp:hello "World")
Hello, World!
ok
```

### Existing Project

Add to your `rebar.config`:

```erlang
{plugins, [
    {rb3lfe, "0.5.0"}
]}.
```

Then:
```bash
rebar3 lfe compile
```

## Project Structure

### Simple Library

```
my-lib/
├── rebar.config
├── src/
│   ├── mylib.lfe
│   └── mylib-utils.lfe
└── test/
    └── mylib-tests.lfe
```

### With Packages (Nested Modules)

```
my-app/
├── rebar.config
├── src/
│   ├── myapp.lfe           → myapp
│   └── myapp/
│       ├── core.lfe        → myapp.core
│       └── utils/
│           └── helpers.lfe → myapp.utils.helpers
└── include/
    └── records.lfe
```

### Umbrella Project

```
my-umbrella/
├── rebar.config
└── apps/
    ├── web/
    │   ├── rebar.config
    │   └── src/
    │       └── web.lfe
    └── db/
        ├── rebar.config
        └── src/
            └── db.lfe
```

## Common Tasks

### Compiling

```bash
# Compile all files
rebar3 lfe compile

# Compile specific profile
rebar3 as prod lfe compile

# Verbose output
rebar3 lfe compile --verbose
```

### Cleaning

```bash
# Remove beam files
rebar3 lfe clean

# Remove everything (including deps)
rebar3 clean
```

### REPL

```bash
# Start REPL with project loaded
rebar3 lfe repl

# Start with specific apps
rebar3 lfe repl --apps myapp,mylib

# Run script before REPL
rebar3 lfe repl --script init.lfe
```

### Testing

```bash
# Run tests (requires ltest dependency)
rebar3 lfe ltest

# Run specific suite
rebar3 lfe ltest --suite my-suite

# Verbose output
rebar3 lfe ltest --verbose
```

### Releases

```bash
# Build release
rebar3 lfe release

# Build tar
rebar3 as prod lfe release tar
```

## Configuration

### Compiler Options

```erlang
{lfe_opts, [
    debug_info,      % Include debug info
    verbose,         % Verbose compilation
    {i, "include"},  % Additional include dir
    {outdir, "ebin"} % Output directory
]}.
```

### First Files

Compile certain files before others:

```erlang
{lfe_first_files, [
    "src/records.lfe",
    "src/macros.lfe"
]}.
```

### Source Directories

```erlang
{src_dirs, [
    "src",
    "extra_src"
]}.
```

### Include Directories

```erlang
{lfe_include_dirs, [
    "include",
    "extra_include"
]}.
```

## Next Steps

- Read the [Command Reference](commands.md)
- Learn about [Packages](packages.md)
- Check out [Examples](../examples/)
- See [Configuration](configuration.md) for advanced options
