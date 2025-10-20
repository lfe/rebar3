# Main Script Example

Runnable LFE script with main/1 function.

## Structure

```
main-script/
├── README.md
├── rebar.config
└── src/
    └── process.lfe  # Contains main/1
```

## Usage

```bash
# Run with default settings
rebar3 lfe run

# Pass arguments
rebar3 lfe run -- input.txt output.txt

# Specify different script
rebar3 lfe run --main src/another.lfe
```

## Implementation

**src/process.lfe:**

```lfe
(defmodule process
  (export (main 1)))

(defun main (args)
  "Process command line arguments"
  (lfe_io:format "Processing ~p arguments~n" (list (length args)))

  (case args
    ((list input output)
     (process-file input output))
    (_
     (show-usage)))

  0)  ; Exit code

(defun process-file (input output)
  (lfe_io:format "Reading from: ~s~n" (list input))
  (lfe_io:format "Writing to: ~s~n" (list output))
  ;; Your processing logic here
  'ok)

(defun show-usage ()
  (lfe_io:format "Usage: rebar3 lfe run -- INPUT OUTPUT~n"))
```

## Configuration

**rebar.config:**

```erlang
{plugins, [{rb3lfe, "0.5.0"}]}.
{deps, [{lfe, "2.2.0"}]}.

%% Set default script
{lfe, [
    {main, "src/process.lfe"}
]}.
```

## See Also

- [Simple Library Example](../simple-lib/) - For basic library structure
- [Command Reference](../../docs/commands.md#run)
