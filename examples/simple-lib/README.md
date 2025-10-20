# Simple Library Example

Basic LFE library with tests.

## Usage

```bash
# Compile
rebar3 lfe compile

# Run tests
rebar3 lfe ltest

# Start REPL
rebar3 lfe repl
```

## In REPL

```lfe
> (mylib:add 2 3)
5
> (mylib:multiply 4 5)
20
> (mylib:greet "World")
Hello, World!
ok
```
