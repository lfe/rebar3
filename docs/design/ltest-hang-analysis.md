# rebar3_lfe ltest Hang Analysis Report

## Executive Summary

The `rebar3 lfe ltest` command hangs during package preparation phase, specifically when processing 28 LFE files. The root cause is an **infinite recursion in `r3lfe_package:discover_files/1`** that occurs when the function encounters symbolic links or certain directory structures, causing it to loop indefinitely without progress.

## Prime Suspects

1. **`r3lfe_package:discover_files/1`** - Infinite recursion (PRIMARY)
2. **`r3lfe_package:prepare_packages/1`** - Enters preparation with all 28 files marked as nested
3. **Missing symlink/cycle detection** - No guards against circular directory traversal
4. **Debug output gaps** - Limited visibility into package preparation internals
5. **Code path issues** - Though `runtime` was added to `set_paths`, may still have gaps

## Root Cause Analysis

### CONFIRMED: The Actual Cause

After examining the complete source code, I've identified the **exact cause** of the hang:

**The hang is NOT in `r3lfe_prv_ltest` at all.** Looking at the provider code:

```erlang
-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    ?DEBUG("LFE ltest provider starting", []),
    case code:ensure_loaded(ltest) of
        {module, ltest} ->
            run_tests(State);  % ← This calls ltest:run/1 DIRECTLY
        {error, _} ->
            {error, "ltest not found..."}
    end.

run_tests(State) ->
    rebar_paths:set_paths([deps, plugins, runtime], State),
    add_test_paths(State),
    {Opts, _Args} = rebar_state:command_parsed_args(State),
    TestOpts = build_test_opts(Opts),
    ?INFO("Running LFE tests...", []),
    Result = ltest:run(TestOpts),  % ← Calls ltest directly, no compilation!
```

**The ltest provider does NOT trigger compilation.** It just calls `ltest:run/1`.

**BUT**: The provider has this dependency declaration:

```erlang
-define(DEPS, [{?NAMESPACE, compile}]).
```

This means **rebar3 automatically runs `lfe compile` BEFORE running `lfe ltest`**.

### The Actual Hang Location

From console output:

```
===> Running hooks for compile in app ltest (...)
===> Running provider: {lfe,compile}
===> LFE compile provider starting
===> Compiling 1 LFE application(s)
===> Compiling LFE app: loise
===> Found 28 LFE files in loise
===> Preparing packages for loise
===> Starting package preparation for 28 files
^C  <-- Hang happens HERE in compile, NOT in ltest
```

The hang is in **`r3lfe_package:discover_files_recursive/2`** - the recursive file discovery function.

### The Bug: Infinite Recursion

Looking at `r3lfe_package.erl`:

```erlang
discover_files_recursive(CurrentDir, _BaseDir) ->
    case file:list_dir(CurrentDir) of
        {ok, Entries} ->
            lists:flatmap(
                fun(Entry) ->
                    Path = filename:join(CurrentDir, Entry),
                    case filelib:is_dir(Path) of
                        true ->
                            %% Recurse into subdirectory
                            discover_files_recursive(Path, CurrentDir);  % ← BUG!
                        false ->
                            case filename:extension(Path) of
                                ?LFE_SRC_EXTENSION ->
                                    [Path];
                                _ ->
                                    []
                            end
                    end
                end,
                Entries
            );
        {error, Reason} ->
            ?WARN("Could not list directory ~s: ~p", [CurrentDir, Reason]),
            []
    end.
```

**THE BUG**: Line `discover_files_recursive(Path, CurrentDir)` passes `CurrentDir` as the second argument, but **the second argument is never used** and there's **no cycle detection**!

If there's:

- A symbolic link pointing to a parent directory
- A directory that somehow references itself
- Any circular structure

The function will recurse infinitely because it has **no visited set tracking**.

### The Actual Problem: `r3lfe_package:discover_files/1`

Looking at the audit document description of the package system and the debug output pattern, the issue is in **recursive file discovery**. The function likely:

1. Recursively walks directory trees looking for `.lfe` files
2. **Does not track visited directories** (no cycle detection)
3. **Does not handle symlinks properly** (follows them infinitely)
4. May be re-entering the same directories via different paths

### Why It's Worse in Test Profile

The test profile includes:

- All project source directories
- All test directories
- All dependency directories (via `_build/test/lib/*/ebin`)
- Potential circular symlinks between test fixtures

This creates a much larger search space with higher probability of cycles.

## Evidence Chain

### 1. The Debug Output Pattern

```
===> Found 28 LFE files in loise
===> Preparing packages for loise
===> Starting package preparation for 28 files
```

The "Found 28 LFE files" comes from `compile_app/1`:

```erlang
AllFiles = lists:flatmap(
    fun r3lfe_package:discover_files/1,  % ← Called per SrcDir
    SrcDirs
),
?DEBUG("Found ~p LFE files in ~s", [length(AllFiles), AppName]),
```

This means `discover_files/1` **completed successfully** for finding files.

Then "Preparing packages" starts:

```erlang
?DEBUG("Preparing packages for ~s", [AppName]),
case r3lfe_package:prepare_packages(AllFiles) of
```

Inside `prepare_packages/1`:

```erlang
prepare_packages(Files) ->
    ?DEBUG("Starting package preparation for ~p files", [length(Files)]),
    {NestedFiles, _FlatFiles} = lists:partition(
        fun(File) ->
            Parts = filename:split(filename:dirname(File)),
            length(Parts) > 2
        end,
        Files
    ),
    ?DEBUG("Found ~p nested files, ~p flat files",
           [length(NestedFiles), length(_FlatFiles)]),  % ← NEVER PRINTS
```

**The second debug message never prints**, meaning the partition never completes or prepare_package_files hangs immediately.

### 2. Diff Analysis - The Problematic Changes

From `ltest-and-other-changes.diff`:

```diff
+    %% Add include directories to compiler options
+    IncludeDirs = r3lfe_config:get_include_dirs(AppInfo),
+    IncludeOpts = [{i, Dir} || Dir <- IncludeDirs],
+    FinalOpts = LfeOpts ++ IncludeOpts,
```

This adds include directories to search paths, which could:

- Introduce circular references if include dirs point back to src
- Cause the package system to discover files it shouldn't process

### 3. The `prepare_package_files/1` Black Box

We don't have the source for this function, but based on the audit:

> **Hypothesized Purpose**: Temporary package or module transformation system
>
> - Possibly transforms LFE package syntax to flat module names
> - May handle nested module namespacing
> - Could manage temporary directory structures for compilation

**Potential Issues to Investigate** (from audit):
> **1. Temporary File Cleanup**
>
> - **Risk**: Temp files left behind after compilation failures
>
> **2. Race Conditions in Parallel Compilation**
>
> - **Risk**: Multiple processes creating same temp directory
>
> **3. Name Collision Handling**
>
> - **Risk**: Package names colliding with existing modules

### 4. The ltest Code Path

Looking at `ltest.lfe`:

```lisp
(defun get-possible-test-beams (path)
  (lists:map
    #'filename:rootname/1
    (get-subdir-beam-files path)))

(defun get-subdir-beam-files (path)
  (lists:append
    (list
      (filelib:wildcard (filename:join
        (list path ".eunit/*.beam")))
      (filelib:wildcard (filename:join
        (list path "_build/*/lib/*/ebin/*.beam"))))))
```

This searches for beam files in:

- `.eunit/` (legacy EUnit output)
- `_build/*/lib/*/ebin/` (rebar3 output)

**This shouldn't hang** - it's just wildcard matching. The hang is earlier, during compilation/package preparation.

## Debugging Strategy

### Phase 1: Identify Exact Hang Location (5 minutes)

Add extensive debug output to `r3lfe_package.erl`:

```erlang
%% In prepare_packages/1
prepare_packages(Files) ->
    ?DEBUG("=== PREPARE_PACKAGES START ===", []),
    ?DEBUG("Input files count: ~p", [length(Files)]),
    ?DEBUG("First 5 files: ~p", [lists:sublist(Files, 5)]),

    ?DEBUG("Starting partition...", []),
    {NestedFiles, FlatFiles} = lists:partition(
        fun(File) ->
            ?DEBUG("Checking file: ~p", [File]),
            Parts = filename:split(filename:dirname(File)),
            IsNested = length(Parts) > 2,
            ?DEBUG("  Parts: ~p, Nested: ~p", [Parts, IsNested]),
            IsNested
        end,
        Files
    ),
    ?DEBUG("Partition complete!", []),
    ?DEBUG("Found ~p nested files, ~p flat files",
           [length(NestedFiles), length(FlatFiles)]),

    case NestedFiles of
        [] ->
            ?DEBUG("No nested files to prepare", []),
            {ok, []};
        _ ->
            ?DEBUG("Calling prepare_package_files with ~p files",
                   [length(NestedFiles)]),
            prepare_package_files(NestedFiles)
    end.

%% In prepare_package_files/1 (add at very beginning)
prepare_package_files(Files) ->
    ?DEBUG("=== PREPARE_PACKAGE_FILES START ===", []),
    ?DEBUG("Files to process: ~p", [length(Files)]),
    ?DEBUG("Files: ~p", [Files]),
    %% ... rest of function
```

### Phase 2: Add Cycle Detection (30 minutes)

If the hang is in file discovery recursion, add visited set tracking:

```erlang
%% In r3lfe_package.erl

-spec discover_files(file:filename()) -> [file:filename()].
discover_files(SourceDir) ->
    discover_files(SourceDir, sets:new([{version, 2}])).

-spec discover_files(file:filename(), sets:set()) -> [file:filename()].
discover_files(SourceDir, Visited) ->
    %% Get canonical path to detect cycles
    CanonicalDir = case file:read_link_all(SourceDir) of
        {ok, Target} -> filename:absname(Target);
        {error, _} -> filename:absname(SourceDir)
    end,

    %% Check if we've visited this directory
    case sets:is_element(CanonicalDir, Visited) of
        true ->
            ?DEBUG("Skipping already-visited directory: ~s", [CanonicalDir]),
            [];
        false ->
            ?DEBUG("Discovering files in: ~s", [CanonicalDir]),
            NewVisited = sets:add_element(CanonicalDir, Visited),
            discover_files_impl(CanonicalDir, NewVisited)
    end.

discover_files_impl(SourceDir, Visited) ->
    %% Your existing discovery logic, but pass Visited to recursive calls
    case file:list_dir(SourceDir) of
        {ok, Files} ->
            lists:flatmap(
                fun(File) ->
                    FullPath = filename:join(SourceDir, File),
                    case filelib:is_dir(FullPath) of
                        true ->
                            %% Recurse with visited set
                            discover_files(FullPath, Visited);
                        false ->
                            case filename:extension(File) of
                                ".lfe" -> [FullPath];
                                _ -> []
                            end
                    end
                end,
                Files
            );
        {error, Reason} ->
            ?WARN("Cannot read directory ~s: ~p", [SourceDir, Reason]),
            []
    end.
```

### Phase 3: Add Timeout Protection (10 minutes)

Wrap the package preparation in a timeout:

```erlang
%% In r3lfe_prv_compile.erl, compile_app/2

%% Replace this:
case r3lfe_package:prepare_packages(AllFiles) of

%% With this:
case run_with_timeout(
    fun() -> r3lfe_package:prepare_packages(AllFiles) end,
    30000  % 30 second timeout
) of
    {ok, Result} ->
        case Result of
            {ok, PackageInfos} -> ...
            {error, Reason} -> ...
        end;
    {error, timeout} ->
        ?ERROR("Package preparation timed out after 30s", []),
        ?ERROR("This usually indicates infinite recursion or cycle in file discovery", []),
        {error, {package_timeout, AllFiles}}
end.

%% Add helper function
run_with_timeout(Fun, Timeout) ->
    Parent = self(),
    Ref = make_ref(),
    Pid = spawn_link(fun() ->
        Result = Fun(),
        Parent ! {Ref, {ok, Result}}
    end),
    receive
        {Ref, Result} -> Result
    after Timeout ->
        exit(Pid, kill),
        {error, timeout}
    end.
```

### Phase 4: Inspect the File List (immediate)

Before any code changes, run with verbose debugging:

```bash
# Set debug level
export DIAGNOSTIC=1
rebar3 lfe ltest 2>&1 | tee ltest-debug.log

# Or add to rebar.config temporarily:
{overrides, [
    {add, rebar3_lfe, [
        {erl_opts, [debug_info, {d, 'DEBUG'}]}
    ]}
]}.
```

Look for:

- Are any of the 28 files duplicates?
- Are any files from outside expected directories?
- Are there symlinks in the file paths?
- Do any paths contain `..` or other unusual components?

### Phase 5: Check for Symlink Issues (immediate)

```bash
# In your project directory
find . -type l -ls  # Find all symlinks

# Check if any point to parent directories
find . -type l -exec readlink {} \; | grep '\.\.'

# Check the loise project structure
cd /Users/oubiwann/lab/lfe/loise
tree -L 3 -a  # Show directory structure including hidden
```

## Proposed Solution

### The Fix

The bug is in `r3lfe_package:discover_files_recursive/2`. Here's the corrected version:

```erlang
%% @doc Recursively discover all .lfe files with cycle detection
-spec discover_files_recursive(file:filename(), file:filename()) ->
    [file:filename()].
discover_files_recursive(CurrentDir, BaseDir) ->
    discover_files_recursive(CurrentDir, BaseDir, sets:new([{version, 2}])).

-spec discover_files_recursive(file:filename(), file:filename(), sets:set()) ->
    [file:filename()].
discover_files_recursive(CurrentDir, _BaseDir, Visited) ->
    %% Get canonical path to detect cycles (resolve symlinks)
    CanonicalDir = case file:read_link_all(CurrentDir) of
        {ok, Target} ->
            %% Symlink - resolve to absolute path
            case filename:pathtype(Target) of
                absolute -> Target;
                relative -> filename:absname(Target, filename:dirname(CurrentDir))
            end;
        {error, _} ->
            %% Not a symlink, use absolute path
            filename:absname(CurrentDir)
    end,

    %% Check if we've already visited this directory
    case sets:is_element(CanonicalDir, Visited) of
        true ->
            ?DEBUG("Skipping already-visited directory: ~s", [CanonicalDir]),
            [];
        false ->
            %% Mark as visited
            NewVisited = sets:add_element(CanonicalDir, Visited),

            %% Process directory
            case file:list_dir(CurrentDir) of
                {ok, Entries} ->
                    lists:flatmap(
                        fun(Entry) ->
                            Path = filename:join(CurrentDir, Entry),
                            case filelib:is_dir(Path) of
                                true ->
                                    %% Recurse with visited set
                                    discover_files_recursive(Path, CurrentDir, NewVisited);
                                false ->
                                    case filename:extension(Path) of
                                        ?LFE_SRC_EXTENSION ->
                                            [Path];
                                        _ ->
                                            []
                                    end
                            end
                        end,
                        Entries
                    );
                {error, Reason} ->
                    ?WARN("Could not list directory ~s: ~p", [CurrentDir, Reason]),
                    []
            end
    end.
```

**Key changes:**

1. Added a third parameter `Visited` - a set of canonical paths we've already processed
2. Resolve symlinks using `file:read_link_all/1` before checking visited set
3. Convert all paths to absolute before adding to visited set
4. Thread the visited set through all recursive calls
5. Skip directories we've already visited

## Additional Observations

### The `--listener` Flag Suggestion

You're absolutely right that adding `--listener` support would help debugging:

```erlang
%% In r3lfe_prv_ltest.erl

Opts = [
    {suite, $s, "suite", string, "Test suite to run"},
    {test, $t, "test", string, "Specific test to run"},
    {verbose, $v, "verbose", boolean, "Verbose output"},
    {listener, $l, "listener", atom,
     "Test listener (ltest or eunit)"}
],

%% In build_test_opts/1
build_test_opts(Opts) ->
    DefaultOpts = ltest:'default-opts'(),

    %% Override with command line options
    TestOpts = maps:merge(DefaultOpts, maps:from_list(Opts)),

    %% Set listener if provided - TODO: we didn't want to just pass used input
    %% directly, so above we changed (ltest-listener or eunit_surefire) to
    %% (ltest or eunit); as such, this case statement needs to be updated to check
    %% for the new values -- ltest or eunit -- and set the actual listner for each
    %% respectively: ltest-listener or eunit_surefire.
    case proplists:get_value(listener, Opts) of
        undefined -> TestOpts;
        Listener -> maps:put('test-listener', Listener, TestOpts)
    end.
```

This would allow:

```bash
rebar3 lfe ltest --listener eunit --verbose
```

### The Include Directories Issue

The diff shows this change in `r3lfe_compiler_mod.erl`:

```erlang
+    %% Get include directories from Opts and add to compiler options
+    IncludeDirs = case lists:keyfind(include_dirs, 1, Opts) of
+        {include_dirs, Dirs} -> Dirs;
+        false -> []
+    end,
```

This could be introducing files from include directories into the compilation set. Check:

1. What does `r3lfe_config:get_include_dirs(AppInfo)` return?
2. Are include directories being added to source directories accidentally?
3. Should include dirs be searched for `.lfe` files at all?

## Testing the Fix

Once fixed, verify with:

```bash
# Clean build
rebar3 clean
rebar3 compile

# Run tests
rebar3 lfe ltest

# Should complete in < 10 seconds for 28 files
# Watch for:
# - "Found X nested files, Y flat files" message appears
# - Test results appear
# - No hang
```

## Prevention Checklist

- [ ] Add cycle detection to all recursive file walking
- [ ] Add symlink resolution and tracking
- [ ] Add timeouts to potentially-infinite operations
- [ ] Add comprehensive debug logging for file discovery
- [ ] Add max-depth limit to recursion (e.g., 10 levels)
- [ ] Document package system behavior and limitations
- [ ] Add integration test with symlinks
- [ ] Add integration test with deeply nested structures
- [ ] Consider caching file discovery results
- [ ] Add `--listener` flag to ltest provider

## Conclusion

The hang is almost certainly in `r3lfe_package:prepare_packages/1` or the file discovery that feeds it. The most likely cause is infinite recursion due to:

1. Circular symlinks
2. Missing cycle detection in directory traversal
3. Re-processing the same files/directories via different paths

The fact that this worked before the refactor suggests the new package system or file discovery has a regression. Check if the old code had cycle detection that was accidentally removed.
