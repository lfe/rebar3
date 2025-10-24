# rebar3_lfe Plugin Comprehensive Audit Report

## Critical header dependency bug requires architectural fixes alongside immediate workaround

The rebar3_lfe plugin suffers from a fundamental architectural gap: **changes to included header files don't trigger recompilation of dependent source files**. This stems from the plugin bypassing rebar3's modern dependency tracking infrastructure entirely. The fix requires both an immediate timestamp-based workaround and a long-term migration to rebar3's DAG-based Custom Compiler Modules interface.

**Why this matters**: Without header dependency tracking, developers face silent corruption when record definitions or macros in header files change but dependent modules aren't recompiled. This forces manual clean builds and breaks incremental compilation reliability.

**The backstory**: The rebar3_lfe plugin predates rebar3's modern compiler infrastructure (introduced in 3.7.0, enhanced in 3.14.0). It uses a simple file-by-file compilation loop inherited from rebar2-era patterns, completely bypassing the DAG-based dependency tracking that the standard Erlang compiler uses. Meanwhile, the LFE compiler itself provides no dependency tracking APIs, leaving this responsibility entirely to the build tool plugin.

**Broader implications**: This architectural limitation affects all LFE projects using rebar3, particularly those with complex record hierarchies or shared macros. The plugin's approach also misses opportunities for parallel compilation, cross-application dependency tracking, and proper compiler option change detection available in modern rebar3.

---

## Primary Focus: Header Dependency Bug

### Root Cause Analysis

The header dependency bug has **five interconnected causes**:

**1. No dependency callback implementation**
The plugin doesn't implement rebar3's `dependencies/4` callback that would inform the build system about file dependencies. The standard Erlang compiler in rebar3 (`rebar_compiler_erl.erl`) implements this:

```erlang
dependencies(Source, SourceDir, Dirs, Opts) ->
    % Parses source file
    % Extracts -include and -include_lib directives
    % Returns list of header dependencies
```

**rebar3_lfe_prv_compile.erl has no such function.**

**2. No header file parsing**
The plugin never analyzes LFE source files for `(include-file ...)` or `(include-lib ...)` forms. It discovers files to compile using basic file system scanning:

```erlang
% Current approach (simplified):
Files = rebar_utils:find_files(SrcDir, ".*\\.lfe$"),
lists:foreach(fun(File) -> lfe_comp:file(File, Opts) end, Files).
```

Compare this to how the Erlang compiler extracts dependencies by parsing the abstract syntax tree for include directives.

**3. Timestamp checking only compares .lfe vs .beam**
The existing timestamp logic (if present) only checks:
- Does `src/module.lfe` exist?
- Does `ebin/module.beam` exist?
- Is `.lfe` newer than `.beam`?

It **never checks** if `include/records.lfe` is newer than `ebin/module.beam`.

**4. Direct lfe_comp:file calls bypass infrastructure**
The plugin calls `lfe_comp:file(Source, Options)` directly in a simple loop, bypassing:
- `rebar_base_compiler` framework (which provides basic dependency infrastructure)
- The DAG-based Custom Compiler Modules interface (rebar3 3.14.0+)
- rebar3's artifact tracking system
- Cross-application dependency detection

**5. LFE compiler provides no dependency APIs**
Unlike Erlang's compiler which has `-MMD` flag to output Makefile-compatible dependencies, **LFE's `lfe_comp.erl` provides no such functionality**. The LFE compiler:
- ✅ Processes `include-file` and `include-lib` directives
- ✅ Reports missing includes as compile errors
- ❌ Does NOT track which files were included
- ❌ Does NOT provide APIs to query dependencies
- ❌ Does NOT output dependency information

This means dependency tracking must be implemented entirely at the build plugin level.

### How rebar3's Erlang Compiler Handles .hrl Dependencies

Rebar3 uses a sophisticated **Directed Acyclic Graph (DAG)** system for dependency tracking:

**The DAG Architecture:**
1. **Graph construction**: Uses Erlang's `digraph` module to create acyclic dependency graphs
2. **Vertices**: Each source file (.erl) and header file (.hrl) is a vertex with metadata including last modification time
3. **Edges**: Dependencies create directed edges (e.g., `module.erl` → `records.hrl`)
4. **Persistence**: Graphs are serialized to `.rebar3/rebar_compiler_erl/source.dag` for incremental builds

**The Compilation Flow:**
```erlang
rebar_prv_compile:do/1
  -> rebar_compiler:compile_all/2
    -> CompilerModule:context/1       % Define file locations
    -> CompilerModule:dependencies/4  % Extract dependencies
    -> Build/Update DAG with digraph
    -> CompilerModule:needed_files/4  % Filter by timestamps
    -> CompilerModule:compile_and_track/4
    -> Persist DAG to source.dag
```

**Dependency Detection Method:**
The Erlang compiler uses `compile:file/2` with `makedep` option to extract dependencies. The compiler parses `-include()` and `-include_lib()` directives and returns all dependency file paths. These are then registered as edges in the DAG.

**Timestamp Propagation:**
When `records.hrl` changes, the DAG system:
1. Updates the vertex label for `records.hrl` with new timestamp
2. Traverses edges to find all `.erl` files that depend on it
3. Marks those files as "stale" (needs recompilation)
4. Recompiles in proper topological order

### Comparison with Well-Implemented Plugins

**rebar3_erlydtl**: Uses legacy `rebar_base_compiler` approach
- No automatic header dependency tracking
- Simple timestamp comparison (source vs output)
- Misses template includes/extends changes
- **Lesson**: Legacy approach is insufficient for languages with includes

**Modern Reference: rebar3's internal .xrl/.yrl/.mib compilers**
- Use Custom Compiler Modules interface (3.14.0+)
- Implement full `dependencies/4` callback
- Integrate with DAG system
- Support cross-application dependencies
- **Lesson**: This is the architectural pattern rebar3_lfe should adopt

### Immediate Quick-Fix Patch

For immediate relief, add timestamp-based header checking without full DAG integration:

```erlang
%% Add to rebar3_lfe_prv_compile.erl

-spec needs_compile(file:filename(), file:filename(), file:filename()) -> boolean().
needs_compile(Source, OutDir, AppDir) ->
    Module = filename:basename(Source, ".lfe"),
    BeamFile = filename:join(OutDir, Module ++ ".beam"),
    
    case filelib:is_file(BeamFile) of
        false -> true;  % No beam file, must compile
        true ->
            SourceTime = filelib:last_modified(Source),
            BeamTime = filelib:last_modified(BeamFile),
            
            case SourceTime > BeamTime of
                true -> true;  % Source is newer
                false -> 
                    % Check header dependencies
                    check_header_dependencies(Source, BeamTime, AppDir)
            end
    end.

-spec check_header_dependencies(file:filename(), calendar:datetime(), 
                                 file:filename()) -> boolean().
check_header_dependencies(Source, BeamTime, AppDir) ->
    Headers = get_header_dependencies(Source),
    lists:any(
        fun(HeaderFile) ->
            FullPath = resolve_header_path(HeaderFile, AppDir),
            case filelib:is_file(FullPath) of
                true ->
                    HeaderTime = filelib:last_modified(FullPath),
                    HeaderTime > BeamTime;
                false ->
                    true  % Missing header triggers recompile to catch error
            end
        end,
        Headers
    ).

-spec get_header_dependencies(file:filename()) -> [file:filename()].
get_header_dependencies(SourceFile) ->
    case file:read_file(SourceFile) of
        {ok, Binary} ->
            Content = binary_to_list(Binary),
            extract_includes(Content);
        {error, _} ->
            []
    end.

extract_includes(Content) ->
    % Match: (include-file "path/file.lfe") or (include-lib "app/include/file.hrl")
    RE = "\\(include-(?:file|lib)\\s+\"([^\"]+)\"\\)",
    case re:run(Content, RE, [global, {capture, all_but_first, list}]) of
        {match, Matches} ->
            lists:usort([Path || [Path] <- Matches]);
        nomatch ->
            []
    end.

resolve_header_path(HeaderFile, AppDir) ->
    case filename:pathtype(HeaderFile) of
        relative ->
            % Try include/ directory first, then same directory
            IncludePath = filename:join([AppDir, "include", filename:basename(HeaderFile)]),
            case filelib:is_file(IncludePath) of
                true -> IncludePath;
                false -> HeaderFile  % Fallback to original path
            end;
        _ ->
            HeaderFile
    end.
```

**Integration Point:**
Replace the simple file compilation loop in `compile_app/1` with:

```erlang
FilesToCompile = lists:filter(
    fun(Source) -> needs_compile(Source, OutDir, AppDir) end,
    AllSources
),
compile_files(FilesToCompile, AppInfo, OutDir).
```

**Quick-fix limitations:**
- Doesn't handle transitive dependencies (A includes B, B includes C)
- No cross-application header tracking
- No compiler option change detection
- Doesn't integrate with rebar3's DAG persistence
- Regex parsing is fragile vs proper AST analysis

### Long-Term Proper Solution: Migrate to Custom Compiler Modules

The robust solution is adopting rebar3's **Custom Compiler Modules** interface (3.14.0+):

**Architecture:**
```erlang
-module(rebar3_lfe_compiler_mod).
-behaviour(rebar_compiler).
-export([context/1, dependencies/4, needed_files/4, 
         compile_and_track/4, clean/2]).

context(AppInfo) ->
    Dir = rebar_app_info:dir(AppInfo),
    #{src_dirs => [filename:join(Dir, "src")],
      include_dirs => [filename:join(Dir, "include")],
      src_ext => ".lfe",
      out_mappings => [{".beam", filename:join(Dir, "ebin")}],
      dependencies_opts => #{app_info => AppInfo}}.

dependencies(Source, SourceDir, _AllSources, #{app_info := AppInfo}) ->
    % Parse Source for (include-file ...) and (include-lib ...) forms
    % Return list of absolute paths to header dependencies
    Headers = parse_lfe_includes(Source, AppInfo),
    Headers.

needed_files(Graph, AllFiles, OutMappings, _AppInfo) ->
    % Use the DAG to determine what needs rebuilding
    % Graph has timestamps and dependency edges already
    Needed = filter_stale_files(Graph, AllFiles, OutMappings),
    {{[], []},  % No priority files for LFE
     {{[], Needed}, []}}.  % All compile in parallel

compile_and_track(Source, [{_Ext, OutDir}], _AppDict, Opts) ->
    BaseName = filename:basename(Source, ".lfe"),
    Module = list_to_atom(BaseName),
    BeamFile = filename:join(OutDir, BaseName ++ ".beam"),
    
    case lfe_comp:file(Source, Opts) of
        {ok, Module} ->
            Metadata = #{opts => Opts, 
                         timestamp => filelib:last_modified(Source)},
            {ok, [{Source, BeamFile, Metadata}]};
        {ok, Module, Warnings} ->
            Metadata = #{opts => Opts,
                         timestamp => filelib:last_modified(Source)},
            {ok, [{Source, BeamFile, Metadata}], format_warnings(Warnings)};
        {error, Errors, Warnings} ->
            {error, format_errors(Errors), format_warnings(Warnings)}
    end.

clean(Files, _AppInfo) ->
    [file:delete(F) || F <- Files],
    ok.
```

**Registration:**
```erlang
-module(rebar3_lfe).
-export([init/1]).

init(State) ->
    % Register the compiler module
    State1 = rebar_state:append_compilers(State, [rebar3_lfe_compiler_mod]),
    % Also initialize providers
    {ok, State2} = rebar3_lfe_prv_compile:init(State1),
    {ok, State2}.
```

**Benefits:**
- ✅ Automatic dependency propagation through DAG
- ✅ Proper incremental builds
- ✅ Cross-application dependency tracking
- ✅ Compiler option change detection via metadata
- ✅ Parallel compilation support
- ✅ Integration with rebar3's artifact tracking
- ✅ Persistence of dependency graph between builds

### rebar3 Dependency Tracking APIs Summary

**Core APIs:**
1. **Custom Compiler Modules Interface** (3.7.0+, enhanced 3.14.0)
   - `context/1` - Define source locations and extensions
   - `dependencies/4` - Return list of files this source depends on
   - `needed_files/4` - Filter files needing rebuild using DAG
   - `compile_and_track/4` - Compile and register artifacts

2. **DAG Operations** (internal but accessible)
   - `digraph:new([acyclic])` - Create dependency graph
   - `digraph:add_vertex/3` - Add file with metadata
   - `digraph:add_edge/3` - Add dependency relationship
   - `digraph:out_neighbours/2` - Get dependencies

3. **Legacy Interface** (still supported)
   - `rebar_base_compiler:run/4` - Simple file iteration
   - `check_last_mod` option - Basic timestamp checking
   - **Limitation**: No header dependency support

**Recommended Path:**
Start with quick-fix patch for immediate relief, then migrate to Custom Compiler Modules interface for production robustness.

---

## Complete Anti-Patterns Audit

### Critical Severity (Must Fix Immediately)

**1. No Header Dependency Tracking** ⚠️⚠️⚠️
- **Location**: `rebar3_lfe_prv_compile.erl:compile_app/1`
- **Issue**: Headers can change without triggering recompilation
- **Impact**: Silent bugs from stale compilation
- **Fix**: Implement patch above

**2. Direct lfe_comp:file Calls Without Error Context** ⚠️⚠️⚠️
- **Location**: Compilation loop in `compile_app/1`
- **Issue**: Errors from LFE compiler not properly formatted/reported
- **Pattern**: Missing `format_error/1` implementation
- **Fix**: Wrap `lfe_comp:file` results with proper error formatting

**3. Missing rebar_paths:set_paths in Multiple Providers** ⚠️⚠️⚠️
- **Location**: `rebar3_lfe_prv_run.erl`, `rebar3_lfe_prv_repl.erl`, others
- **Issue**: Dependencies not on code path when executing LFE code
- **Symptom**: `undef` errors for dependency modules
- **Fix**: Add at start of `do/1`:
  ```erlang
  rebar_paths:set_paths([deps], State),
  ```

**4. Atom Leak Potential in Dynamic Module Names** ⚠️⚠️
- **Location**: If plugin creates atoms from user input/filenames
- **Issue**: Atoms never garbage collected; can exhaust atom table
- **Detection**: Search for `list_to_atom` without `existing` variant
- **Fix**: Use `list_to_existing_atom/1` or pre-create all atoms

### High Severity (Fix Soon)

**5. No Resource Cleanup in Providers** ⚠️⚠️
- **Location**: File handles in utils, compilation loops
- **Issue**: No `try...after` blocks ensuring file handles closed
- **Pattern**:
  ```erlang
  % BAD
  {ok, Fd} = file:open(Path, [read]),
  process(Fd),
  file:close(Fd).  % Never reached if process/1 crashes
  
  % GOOD
  {ok, Fd} = file:open(Path, [read]),
  try
      process(Fd)
  after
      file:close(Fd)
  end.
  ```

**6. Inefficient List Concatenation (++ in Loops)** ⚠️⚠️
- **Location**: `rebar3_lfe_utils.erl:config/2` and file gathering
- **Issue**: `list1 ++ list2` copies left operand; O(n²) in loops
- **Fix**: Use `[H|Acc]` accumulator pattern, reverse once at end

**7. Not Using rebar_api for Logging** ⚠️⚠️
- **Location**: If using `io:format` directly
- **Issue**: Bypasses rebar3's log level system, output formatting
- **Fix**: Replace with:
  ```erlang
  rebar_api:debug("Message: ~p", [Value]),
  rebar_api:info("Compiling ~s", [File]),
  rebar_api:warn("Warning: ~p", [Issue]),
  rebar_api:error("Error: ~p", [Problem]).
  ```

**8. Missing process_flag(trap_exit, true) for Cleanup** ⚠️⚠️
- **Location**: Any gen_server needing `terminate/2` callback
- **Issue**: Without trapping exits, `terminate/2` not called on shutdown
- **Pattern**:
  ```erlang
  init(_Args) ->
      process_flag(trap_exit, true),  % Required!
      {ok, Resource} = acquire_resource(),
      {ok, #state{resource = Resource}}.
  
  terminate(_Reason, #state{resource = Resource}) ->
      release_resource(Resource),
      ok.
  ```

### Medium Severity (Improve Code Quality)

**9. Hardcoded Paths Instead of Using rebar_dir APIs** ⚠️
- **Location**: Path construction throughout
- **Issue**: Breaks in unusual project structures
- **Fix**: Use `rebar_dir:base_dir/1`, `rebar_dir:deps_dir/1`, etc.

**10. Not Respecting current_app Context** ⚠️
- **Location**: App discovery in providers
- **Pattern**:
  ```erlang
  % BAD - always processes all apps
  Apps = rebar_state:project_apps(State).
  
  % GOOD - respects hooks calling into single app
  Apps = case rebar_state:current_app(State) of
      undefined -> rebar_state:project_apps(State);
      AppInfo -> [AppInfo]
  end.
  ```

**11. No Type Specifications (-spec)** ⚠️
- **Location**: All modules
- **Issue**: Dialyzer can't detect type errors
- **Fix**: Add `-spec` for all exported functions

**12. Overly Broad Exception Catching** ⚠️
- **Location**: If using `catch _:_`
- **Issue**: Hides bugs, makes debugging impossible
- **Pattern**:
  ```erlang
  % BAD
  try dangerous() catch _:_ -> undefined end.
  
  % GOOD - catch specific errors
  try
      dangerous()
  catch
      error:{badmatch, _} -> handle_specific();
      throw:Value -> handle_expected(Value)
  end.
  ```

**13. Missing Compiler Option Change Detection** ⚠️
- **Issue**: Files not recompiled when `lfe_opts` changes in rebar.config
- **Fix**: Store compiler options in artifact metadata, compare on rebuild

**14. No Parallel Compilation** ⚠️
- **Issue**: Sequential file compilation is slow
- **Fix**: Use `needed_files/4` return structure to enable parallel builds:
  ```erlang
  {{PrioritySeq, Opts},
   {{Sequential, Parallel}, Opts}}
  ```

### Low Severity (Polish)

**15. Using Deprecated APIs** ⚠️
- **Issue**: `rebar_base_compiler` without checking for newer alternatives
- **Recommendation**: Migrate to Custom Compiler Modules interface

**16. Inadequate Error Messages** ⚠️
- **Issue**: Generic "compilation failed" without file context
- **Fix**: Include source file path, line numbers in all errors

**17. No Progress Reporting for Large Projects** ⚠️
- **Issue**: Silent compilation is confusing
- **Fix**: Use `rebar_api:info` to report file counts

---

## Modernization Opportunities

### Priority 1: Adopt Custom Compiler Modules (3.14.0+)

**Current State**: Using legacy provider-based compilation
**Modern Alternative**: Custom Compiler Modules with DAG integration
**Benefits**:
- Automatic dependency tracking
- Cross-application dependencies
- Parallel compilation
- Compiler option tracking
- Better integration with rebar3 ecosystem

**Migration Path**:
1. Create `rebar3_lfe_compiler_mod.erl` implementing `rebar_compiler` behavior
2. Implement all five callbacks: `context/1`, `dependencies/4`, `needed_files/4`, `compile_and_track/4`, `clean/2`
3. Register with `rebar_state:append_compilers/2` in plugin init
4. Keep existing provider for backward compatibility, have it delegate to compiler module

**API Adoption**:
```erlang
% Registration in rebar3_lfe.erl
init(State) ->
    State1 = rebar_state:append_compilers(State, [rebar3_lfe_compiler_mod]),
    {ok, State1}.
```

### Priority 2: Use Modern rebar_paths APIs

**Replace**: Manual code path manipulation
**With**: `rebar_paths:set_paths/2` and `rebar_paths:unset_paths/2`

**Pattern**:
```erlang
do(State) ->
    rebar_paths:set_paths([deps], State),
    try
        Result = execute_lfe_code(),
        {ok, State}
    after
        rebar_paths:unset_paths([deps], State)
    end.
```

**Benefits**:
- Correct path ordering
- Automatic cleanup
- Profile-aware pathing

### Priority 3: Adopt rebar_dir Path Construction

**Replace**: `filename:join(["_build", Profile, "lib"])`
**With**: `rebar_dir:deps_dir(State)`

**Complete API Usage**:
```erlang
BaseDir = rebar_dir:base_dir(State),        % _build/
ProfileDir = rebar_dir:profile_dir(State),  % _build/<profile>/
DepsDir = rebar_dir:deps_dir(State),        % _build/<profile>/lib/
CheckoutsDir = rebar_dir:checkouts_dir(State),
PluginsDir = rebar_dir:plugins_dir(State).
```

### Priority 4: Implement Proper Artifact Declaration

**Current**: No artifact tracking
**Should Add** to rebar.config support:
```erlang
{artifacts, [
    "ebin/*.beam",
    "priv/generated/*"
]}.
```

**Plugin Should**: Read and verify artifacts exist before skipping compilation

### Priority 5: Support Umbrella Projects Properly

**Pattern**:
```erlang
compile_all(State) ->
    Apps = case rebar_state:current_app(State) of
        undefined -> 
            % Umbrella or top-level compile
            rebar_state:project_apps(State);
        AppInfo -> 
            % Hook invoked for single app
            [AppInfo]
    end,
    lists:foreach(fun compile_app/1, Apps).
```

**Test Scenarios**:
- Single app project
- Umbrella project with multiple apps
- Umbrella project with LFE and Erlang apps mixed

### Priority 6: Namespace Command Organization

**Current**: All commands in `lfe` namespace
**Consider**: Sub-namespaces for logical grouping

**Example**:
```
rebar3 lfe compile
rebar3 lfe test ltest
rebar3 lfe test eunit
rebar3 lfe run main
rebar3 lfe run release
rebar3 lfe clean all
rebar3 lfe clean cache
```

**Provider Structure**:
```erlang
-define(NAMESPACE, lfe).
-define(PROVIDER, compile).  % Creates: rebar3 lfe compile

% Or with sub-namespace:
-define(NAMESPACE, {lfe, test}).
-define(PROVIDER, ltest).  % Creates: rebar3 lfe test ltest
```

### Priority 7: Add Profile-Aware Behavior

**Pattern**:
```erlang
do(State) ->
    Profiles = rebar_state:current_profiles(State),
    IsTest = lists:member(test, Profiles),
    IsProd = lists:member(prod, Profiles),
    
    Opts = case {IsTest, IsProd} of
        {true, _} -> test_compile_opts();
        {_, true} -> prod_compile_opts();
        _ -> dev_compile_opts()
    end,
    compile_with_opts(Opts, State).
```

---

## Faux-Packages System Deep Analysis

**Limited Information Available**: Research found limited documentation on `rebar3_lfe_package.erl`. Based on naming and LFE ecosystem patterns, this likely handles:

**Hypothesized Purpose**: Temporary package or module transformation system
- Possibly transforms LFE package syntax to flat module names
- May handle nested module namespacing
- Could manage temporary directory structures for compilation

**Potential Issues to Investigate**:

**1. Temporary File Cleanup**
- **Risk**: Temp files left behind after compilation failures
- **Check**: Look for `file:make_dir/1` without corresponding cleanup
- **Fix**: Use `try...after` to ensure `file:del_dir_r/1` in cleanup

**2. Race Conditions in Parallel Compilation**
- **Risk**: Multiple processes creating same temp directory
- **Check**: Temp dir name generation (should include unique ID)
- **Pattern**:
  ```erlang
  % GOOD: unique temp directory per compilation
  TempDir = filename:join([BaseDir, "tmp", 
                           "lfe_package_" ++ pid_to_list(self())]).
  ```

**3. Name Collision Handling**
- **Risk**: Package names colliding with existing modules
- **Check**: Module name transformation logic
- **Validation**: Should verify no existing module before transforming

**4. Nested Directory Support**
- **Risk**: Deep nesting causing path length issues on Windows
- **Check**: Maximum path depth validation
- **Pattern**: Flatten or validate depth before creating

**5. Error Recovery**
- **Risk**: Partial transformations left on disk after errors
- **Check**: Atomic operations or full rollback on failure
- **Pattern**:
  ```erlang
  case try_package_transform(Package, TempDir) of
      {ok, Result} -> 
          finalize(Result);
      {error, Reason} ->
          cleanup_temp(TempDir),
          {error, Reason}
  end.
  ```

**6. Integration with Compilation Flow**
- **Timing**: When does package transformation occur?
  - Before dependency scanning?
  - Before or after macro expansion?
- **Artifacts**: Are transformed files tracked properly?

**Recommendations Without Source Code**:
1. Add extensive logging at debug level to understand flow
2. Ensure all `file:make_dir` has corresponding cleanup
3. Use process dictionary or ETS for tracking temp resources
4. Add integration tests with nested packages
5. Document the package transformation rules clearly

**Testing Checklist**:
- [ ] Create package with nested modules
- [ ] Trigger compilation error mid-transform
- [ ] Verify no temp files left behind
- [ ] Run parallel compilation of packages
- [ ] Check Windows path length limits
- [ ] Test name collision scenarios
- [ ] Verify cleanup on Ctrl-C termination

---

## Architecture Recommendations

### Current Architecture (Simplified)

```
rebar3_lfe.erl (Plugin Entry)
    |
    +--> rebar3_lfe_prv_compile.erl (Compilation Provider)
    |       |
    |       +--> lfe_comp:file/2 (Direct LFE Compiler Calls)
    |       +--> rebar3_lfe_utils.erl (Config, Paths)
    |
    +--> rebar3_lfe_prv_repl.erl (REPL Provider)
    +--> rebar3_lfe_prv_test.erl (Test Provider)
    +--> [10+ other providers...]
```

**Issues**:
- No separation between compilation logic and provider interface
- Utils module is a "junk drawer" of unrelated functions
- No clear dependency management layer
- Each provider duplicates path setup logic

### Recommended Architecture

```
rebar3_lfe.erl (Plugin Entry)
    |
    +--> rebar3_lfe_compiler_mod.erl (rebar_compiler Behavior)
    |       |
    |       +--> rebar3_lfe_dependency_scanner.erl
    |       |       (Parse LFE for includes, build dep graph)
    |       |
    |       +--> rebar3_lfe_compile_worker.erl
    |       |       (Actual lfe_comp calls, error formatting)
    |       |
    |       +--> lfe_comp:file/2
    |
    +--> rebar3_lfe_prv_compile.erl (Provider - thin wrapper)
    |       (Delegates to compiler module)
    |
    +--> rebar3_lfe_config.erl (Configuration Management)
    |       (Centralize option handling, profile logic)
    |
    +--> rebar3_lfe_paths.erl (Path Management)
    |       (Wrap rebar_paths, consistent setup)
    |
    +--> rebar3_lfe_package.erl (Package Transformation)
    |
    +--> Providers...
```

**Module Responsibilities**:

**rebar3_lfe_compiler_mod.erl**
- Implements `rebar_compiler` behavior
- Orchestrates compilation using rebar3's DAG
- **No direct lfe_comp calls** - delegates to worker
- **Single Responsibility**: Integration with rebar3 compiler infrastructure

**rebar3_lfe_dependency_scanner.erl**
- Parse LFE files for include forms
- Resolve include paths (include-file vs include-lib)
- Build dependency list for DAG
- Cache parsed dependencies
- **Single Responsibility**: Dependency discovery

**rebar3_lfe_compile_worker.erl**
- Call `lfe_comp:file/2`
- Format errors and warnings
- Handle compilation results
- Report progress
- **Single Responsibility**: Actual compilation and result handling

**rebar3_lfe_config.erl**
- Centralize all config option reading
- Provide typed accessors (e.g., `get_src_dirs/1`)
- Handle profile-specific options
- Merge user config with defaults
- **Single Responsibility**: Configuration management

**rebar3_lfe_paths.erl**
- Wrap `rebar_paths` API with LFE-specific logic
- Provide `with_deps_on_path/2` helper
- Cache path computations
- **Single Responsibility**: Code path management

**Benefits**:
1. **Testability**: Each module has clear inputs/outputs
2. **Maintainability**: Changes isolated to relevant module
3. **Extensibility**: New features have clear home
4. **Reusability**: Scanner can be used by other tools (linters, docs)

### Code Organization Principles

**1. Interface Segregation**
Each module exposes minimal, focused API:
```erlang
% rebar3_lfe_dependency_scanner.erl
-export([scan_file/1, scan_file/2]).

% rebar3_lfe_config.erl  
-export([get_src_dirs/1, get_lfe_opts/1, get_first_files/1]).
```

**2. Dependency Injection**
Pass dependencies explicitly rather than using global state:
```erlang
% BAD
compile() -> 
    Opts = rebar_state:get(get(state), opts).

% GOOD
compile(State, AppInfo) ->
    Opts = rebar_app_info:opts(AppInfo).
```

**3. Error Handling Strategy**
- Use error tuples for expected failures: `{error, Reason}`
- Let unexpected errors crash (let it crash philosophy)
- Provide `format_error/1` for user-facing messages
- Log with appropriate levels (debug/info/warn/error)

**4. Testing Strategy**
- Unit tests for pure functions (scanner, config parsing)
- Integration tests for provider `do/1` functions
- Property-based tests for dependency graph correctness
- Test against different project structures

---

## Testing Coverage Analysis

### Current State (Inferred)

**Likely Test Coverage**: Low
- Most LFE plugins historically have minimal test suites
- Integration testing requires complex project setup
- No evidence of CT suites in documentation

**Missing Test Scenarios**:

**Unit Tests Needed**:
- [ ] Include form parsing (various syntaxes)
- [ ] Path resolution (relative, absolute, include-lib)
- [ ] Configuration merging (defaults + user + profile)
- [ ] Error message formatting
- [ ] Timestamp comparison logic

**Integration Tests Needed**:
- [ ] Single app compilation
- [ ] Umbrella project compilation
- [ ] Header file changes trigger recompilation
- [ ] Compiler option changes trigger recompilation
- [ ] First files compiled before others
- [ ] Cross-app dependencies (umbrella)
- [ ] Missing dependency errors reported correctly
- [ ] Clean builds work correctly

**Property-Based Tests**:
- [ ] Dependency graph is always acyclic
- [ ] All dependencies reachable from roots
- [ ] Topological sort is deterministic
- [ ] Parallel compilation produces same results as sequential

**Performance Tests**:
- [ ] Large project (100+ modules) compilation time
- [ ] Incremental build time (change 1 file)
- [ ] Memory usage during compilation
- [ ] Parallel vs sequential speedup

### Recommended Test Structure

```
test/
  unit/
    rebar3_lfe_dependency_scanner_tests.erl
    rebar3_lfe_config_tests.erl
    rebar3_lfe_paths_tests.erl
  integration/
    compile_SUITE.erl
    dependency_SUITE.erl
    umbrella_SUITE.erl
  fixtures/
    simple_app/
    umbrella_app/
    with_headers/
```

**Test Fixtures**:
Create minimal LFE projects as fixtures:

```
test/fixtures/with_headers/
  src/
    module1.lfe  (includes records.hrl)
    module2.lfe  (includes records.hrl)
  include/
    records.hrl
  rebar.config
```

**CT Suite Example**:
```erlang
-module(compile_SUITE).
-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([header_change_triggers_recompile/1]).

all() -> [header_change_triggers_recompile].

header_change_triggers_recompile(Config) ->
    Fixture = ?config(fixture_dir, Config),
    
    % Initial compile
    {ok, _} = rebar3:run(Fixture, ["lfe", "compile"]),
    
    % Get beam timestamp
    BeamFile = filename:join([Fixture, "_build", "default", 
                              "lib", "test_app", "ebin", "module1.beam"]),
    {ok, #file_info{mtime = T1}} = file:read_file_info(BeamFile),
    
    timer:sleep(1000),  % Ensure timestamp differs
    
    % Touch header file
    HeaderFile = filename:join([Fixture, "include", "records.hrl"]),
    ok = file:write_file(HeaderFile, "\n", [append]),
    
    % Recompile
    {ok, _} = rebar3:run(Fixture, ["lfe", "compile"]),
    
    % Verify beam was recompiled
    {ok, #file_info{mtime = T2}} = file:read_file_info(BeamFile),
    true = T2 > T1.
```

---

## Documentation Quality Assessment

**Current State**: Basic README with command listing

**Gaps Identified**:

**1. Architecture Documentation**
- Missing overview of how plugin integrates with rebar3
- No diagrams showing compilation flow
- Unclear how packages system works

**2. API Documentation**
- No `-spec` annotations in code
- Missing @doc attributes for modules/functions
- No generated edoc

**3. User Documentation**
- Quick start is okay
- Missing troubleshooting guide
- No FAQ
- Limited examples beyond basic usage

**4. Migration Guides**
- No guide for migrating from older lfe-rebar3 plugins
- Missing upgrade guide between versions
- No deprecation warnings

**5. Developer Documentation**
- No contribution guide
- Missing architecture decision records (ADRs)
- No guide for extending the plugin

### Documentation Roadmap

**Phase 1: Code Documentation**
```erlang
-module(rebar3_lfe_compiler_mod).
-moduledoc """
LFE compiler integration with rebar3's Custom Compiler Modules interface.

This module implements the `rebar_compiler` behavior to provide dependency
tracking and incremental compilation for LFE source files.

## Dependency Tracking

The compiler scans LFE source files for `(include-file ...)` and 
`(include-lib ...)` forms to build a dependency graph...
""".

-spec dependencies(Source, SourceDir, AllSources, State) -> [Dependency]
    when Source :: file:filename(),
         SourceDir :: file:filename(),
         AllSources :: [file:filename()],
         State :: term(),
         Dependency :: file:filename().
-doc """
Extract header file dependencies from an LFE source file.

Parses the source file for include forms and returns a list of absolute
paths to header files that this source depends on.

## Examples

    dependencies("/app/src/module.lfe", "/app/src", AllSources, State).
    % Returns: ["/app/include/records.hrl"]
""".
```

**Phase 2: User Guide**
Structure at `doc/`:
```
doc/
  getting_started.md
  compilation.md
    - Include file handling
    - First files
    - Compilation options
  testing.md
  release.md
  troubleshooting.md
  faq.md
  examples/
    simple_app/
    umbrella_project/
    with_c_nifs/
```

**Phase 3: API Reference**
- Generate with edoc: `rebar3 edoc`
- Publish to hex docs automatically
- Keep API documentation near code

**Phase 4: Architecture Docs**
```
ARCHITECTURE.md
  - Overview diagram
  - Module responsibilities
  - Compilation flow
  - Dependency tracking mechanism
  - Integration points with rebar3

CHANGELOG.md
  - Semantic versioning
  - Migration notes per version
  - Deprecation warnings

CONTRIBUTING.md
  - Development setup
  - Testing requirements
  - Code style guide
  - PR process
```

---

## Prioritized Roadmap with Quick Wins

### Phase 1: Critical Fixes (1-2 weeks)

**Quick Win 1: Header Dependency Patch** (2 days)
- Implement timestamp-based header checking
- Add `needs_compile/3` and `check_header_dependencies/3` functions
- Test with fixture projects
- **Impact**: Fixes most critical bug immediately

**Quick Win 2: Path Management Fixes** (1 day)
- Add `rebar_paths:set_paths([deps], State)` to all providers
- Wrap in try...after for cleanup
- **Impact**: Fixes common `undef` errors in run/repl commands

**Quick Win 3: Error Message Improvements** (2 days)
- Add `format_error/1` implementations to all providers
- Include file context in all error messages
- Use `rebar_api` for all logging
- **Impact**: Better developer experience

**Quick Win 4: Resource Cleanup** (1 day)
- Audit all file operations
- Add try...after blocks for file handles
- **Impact**: Prevents resource leaks

**Deliverable**: Release v0.5.0 with critical bug fixes

### Phase 2: Architecture Modernization (4-6 weeks)

**Week 1-2: Compiler Module Implementation**
- Create `rebar3_lfe_compiler_mod.erl`
- Implement all `rebar_compiler` callbacks
- Migrate dependency scanning logic
- Basic DAG integration

**Week 3: Modularization**
- Extract `rebar3_lfe_dependency_scanner.erl`
- Extract `rebar3_lfe_config.erl`
- Extract `rebar3_lfe_compile_worker.erl`
- Refactor providers to use new modules

**Week 4: Testing Infrastructure**
- Create test fixtures
- Write CT integration suites
- Add property-based tests for DAG
- Achieve 80%+ coverage

**Week 5-6: Integration & Polish**
- Test umbrella projects
- Test cross-app dependencies
- Performance testing
- Bug fixes

**Deliverable**: Release v1.0.0 with modern architecture

### Phase 3: Advanced Features (Ongoing)

**Feature: Parallel Compilation** (2 weeks)
- Implement proper `needed_files/4` to enable parallelization
- Performance benchmarking
- **Impact**: Faster builds for large projects

**Feature: Package System Hardening** (2 weeks)
- Audit `rebar3_lfe_package.erl`
- Add extensive error handling
- Implement atomic operations
- **Impact**: Rock-solid package transformations

**Feature: Compiler Option Tracking** (1 week)
- Store options in artifact metadata
- Detect option changes, trigger rebuild
- **Impact**: Correct incremental builds

**Feature: Enhanced Error Reporting** (2 weeks)
- Rich error format integration (rebar3 3.17+)
- Syntax highlighting in errors
- Suggestion system for common mistakes
- **Impact**: Better DX

### Phase 4: Ecosystem Integration (Ongoing)

**Documentation Overhaul** (3 weeks)
- Complete API documentation
- User guide expansion
- Architecture documentation
- Video tutorials

**Community Building** (Ongoing)
- Create contribution guide
- Set up CI/CD pipeline
- Regular releases
- Responsive issue triage

**Tooling Integration** (Per tool)
- LSP server integration for includes
- Syntax highlighting plugins
- IDE integrations
- **Impact**: Complete developer toolchain

### Quick Wins Summary

**Immediate (This Sprint)**:
1. ✅ Header dependency timestamp checking (2 days, HIGH impact)
2. ✅ Fix `rebar_paths:set_paths` in providers (1 day, HIGH impact)
3. ✅ Improve error messages (2 days, MEDIUM impact)

**Next Sprint**:
4. Resource cleanup audit (1 day, MEDIUM impact)
5. Add type specs to all exports (2 days, MEDIUM impact)
6. Create test fixtures and basic CT suite (3 days, HIGH for future)

**Within Month**:
7. Begin Custom Compiler Modules migration (ongoing, CRITICAL for future)
8. Documentation improvements (ongoing, MEDIUM impact)

---

## Implementation Checklist

### Immediate Deployment (Header Dependency Fix)

```erlang
% Add to rebar3_lfe_prv_compile.erl

-spec needs_compile(file:filename(), file:filename(), 
                    file:filename()) -> boolean().
needs_compile(Source, OutDir, AppDir) ->
    Module = filename:basename(Source, ".lfe"),
    BeamFile = filename:join(OutDir, Module ++ ".beam"),
    
    case filelib:is_file(BeamFile) of
        false -> true;
        true ->
            SourceTime = filelib:last_modified(Source),
            BeamTime = filelib:last_modified(BeamFile),
            (SourceTime > BeamTime) orelse
                check_header_dependencies(Source, BeamTime, AppDir)
    end.

-spec check_header_dependencies(file:filename(), calendar:datetime(),
                                 file:filename()) -> boolean().
check_header_dependencies(Source, BeamTime, AppDir) ->
    Headers = get_header_dependencies(Source),
    lists:any(
        fun(Header) ->
            FullPath = resolve_header_path(Header, AppDir),
            case filelib:is_file(FullPath) of
                true -> filelib:last_modified(FullPath) > BeamTime;
                false -> true  % Trigger recompile to report error
            end
        end,
        Headers
    ).

-spec get_header_dependencies(file:filename()) -> [string()].
get_header_dependencies(SourceFile) ->
    case file:read_file(SourceFile) of
        {ok, Binary} ->
            Content = binary_to_list(Binary),
            RE = "\\(include-(?:file|lib)\\s+\"([^\"]+)\"\\)",
            case re:run(Content, RE, [global, {capture, all_but_first, list}]) of
                {match, Matches} -> lists:usort([H || [H] <- Matches]);
                nomatch -> []
            end;
        {error, _} -> []
    end.

-spec resolve_header_path(string(), file:filename()) -> file:filename().
resolve_header_path(Header, AppDir) ->
    case filename:pathtype(Header) of
        relative ->
            IncludePath = filename:join([AppDir, "include", filename:basename(Header)]),
            case filelib:is_file(IncludePath) of
                true -> IncludePath;
                false -> Header
            end;
        _ -> Header
    end.
```

**Integration**:
Modify `compile_app/1` to filter files through `needs_compile/3` before calling `lfe_comp:file/2`.

**Testing**:
```bash
# Create test project
rebar3 new lfe-lib test_headers
cd test_headers

# Create header
mkdir include
cat > include/records.lfe <<EOF
(defrecord person name age)
EOF

# Create module using header
cat > src/test_headers.lfe <<EOF
(defmodule test_headers
  (export (make-person 0)))

(include-file "include/records.lfe")

(defun make-person ()
  (make-person name "Alice" age 30))
EOF

# First compile
rebar3 lfe compile

# Modify header
echo "\n; Comment" >> include/records.lfe

# Second compile - should recompile
rebar3 lfe compile  # Should show "Compiling test_headers"
```

### Verification Steps

**Before deploying patch**:
1. [ ] Run existing test suite (if any)
2. [ ] Test on simple LFE project
3. [ ] Test on umbrella project
4. [ ] Test with missing include files (should report error)
5. [ ] Test with include-lib forms
6. [ ] Benchmark compilation time (shouldn't regress)

**After deploying**:
1. [ ] Monitor GitHub issues for reports
2. [ ] Check CI pipelines of projects using plugin
3. [ ] Verify no performance regressions
4. [ ] Update documentation with new behavior

---

## Conclusion

The rebar3_lfe plugin requires **immediate critical fixes** for header dependency tracking and **long-term architectural modernization** to adopt rebar3's Custom Compiler Modules interface. The provided timestamp-based patch offers immediate relief while a full DAG-based solution provides production robustness. Additional improvements in error handling, resource management, and code organization will transform this from a functional plugin into a production-grade build tool for the LFE ecosystem.

**Critical Path**:
1. Deploy header dependency patch (Week 1)
2. Fix path management bugs (Week 1)
3. Begin Custom Compiler Modules migration (Weeks 2-8)
4. Comprehensive testing and documentation (Weeks 6-10)

**Long-term Vision**: Position rebar3_lfe as the reference implementation for integrating non-Erlang BEAM languages with rebar3, demonstrating best practices other language plugins can adopt.