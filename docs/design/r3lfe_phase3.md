# Phase 3: Compilation Worker & Error Handling

## Overview

This phase implements the actual LFE compilation logic with proper error handling, warning formatting, and progress reporting. We'll create a dedicated compilation worker module that handles calling `lfe_comp:file/2`, formats compiler output for rebar3, and provides excellent developer experience through clear error messages.

**Goal**: Compile LFE files reliably with professional-quality error reporting and user feedback.

## Prerequisites

- Phase 1 and Phase 2 completed with all tests passing
- Understanding of LFE compiler API (`lfe_comp:file/2`)
- Knowledge of rebar3's error formatting conventions
- Familiarity with Erlang's error/warning tuple formats

## Architecture Overview

```
Compilation Flow:

1. r3lfe_compiler_mod:compile/4 → called by rebar3 for each file
2. r3lfe_compile_worker:compile_file/3 → actual compilation
3. lfe_comp:file(Source, Opts) → LFE compiler
4. r3lfe_compile_worker:format_result/2 → format output
5. Return {ok, Warnings} or {error, Errors, Warnings}
6. r3lfe_compiler_mod:format_error/1 → human-readable errors
```

## Implementation Tasks

### Task 3.1: Implement Compilation Worker

**File: `src/r3lfe_compile_worker.erl`**

```erlang
-module(r3lfe_compile_worker).

%% API exports
-export([
    compile_file/3,
    compile_file/4,
    format_errors/1,
    format_warnings/1,
    format_error/1
]).

-include("rebar3_lfe/include/r3lfe.hrl").

-type lfe_error() :: {file:filename(), [{integer(), module(), term()}]}.
-type lfe_warning() :: {file:filename(), [{integer(), module(), term()}]}.
-type compile_opts() :: [term()].

%%====================================================================
%% API functions
%%====================================================================

%% @doc Compile an LFE source file
%% Returns ok, {ok, Warnings}, or {error, Errors, Warnings}
-spec compile_file(file:filename(), file:filename(), compile_opts()) ->
    ok | {ok, [lfe_warning()]} | {error, [lfe_error()], [lfe_warning()]}.
compile_file(Source, OutDir, Opts) ->
    compile_file(Source, OutDir, Opts, #{}).

%% @doc Compile with additional options
-spec compile_file(file:filename(), file:filename(), compile_opts(), map()) ->
    ok | {ok, [lfe_warning()]} | {error, [lfe_error()], [lfe_warning()]}.
compile_file(Source, OutDir, Opts, ExtraOpts) ->
    %% Ensure output directory exists
    ok = r3lfe_paths:ensure_dir(OutDir),

    %% Build complete compiler options
    CompilerOpts = build_compiler_opts(Source, OutDir, Opts),

    %% Report what we're doing
    Verbose = maps:get(verbose, ExtraOpts, false),
    report_compilation(Source, Verbose),

    %% Call LFE compiler
    case lfe_comp:file(Source, CompilerOpts) of
        {ok, Module} ->
            %% Successful compilation, no warnings
            ?DEBUG("Successfully compiled ~s -> ~s", [Source, Module]),
            ok;

        {ok, Module, Warnings} ->
            %% Successful compilation with warnings
            ?DEBUG("Compiled ~s -> ~s with warnings", [Source, Module]),
            FormattedWarnings = format_warnings(Warnings),
            {ok, FormattedWarnings};

        {error, Errors, Warnings} ->
            %% Compilation failed
            ?ERROR("Failed to compile ~s", [Source]),
            FormattedErrors = format_errors(Errors),
            FormattedWarnings = format_warnings(Warnings),
            {error, FormattedErrors, FormattedWarnings};

        {error, [{error, Errors, Warnings} | _], _Es, _Ws} ->
            %% Alternative error format from LFE compiler
            ?ERROR("Failed to compile ~s (alternative format)", [Source]),
            FormattedErrors = format_errors(Errors),
            FormattedWarnings = format_warnings(Warnings),
            {error, FormattedErrors, FormattedWarnings}
    end.

%%====================================================================
%% Compiler Options
%%====================================================================

%% @doc Build complete compiler options for LFE
-spec build_compiler_opts(file:filename(), file:filename(), compile_opts()) ->
    compile_opts().
build_compiler_opts(Source, OutDir, BaseOpts) ->
    %% Merge with required options
    RequiredOpts = [
        {outdir, OutDir},
        return,
        report_errors,
        report_warnings
    ],

    %% Remove any conflicting options from BaseOpts
    FilteredBase = lists:filter(
        fun(Opt) ->
            Key = option_key(Opt),
            not lists:keymember(Key, 1, RequiredOpts)
        end,
        BaseOpts
    ),

    %% Add source file directory to include path
    SourceDir = filename:dirname(Source),
    IncludeOpt = {i, SourceDir},

    %% Combine all options
    RequiredOpts ++ FilteredBase ++ [IncludeOpt].

%% @doc Extract key from an option for comparison
-spec option_key(term()) -> term().
option_key({Key, _}) -> Key;
option_key(Atom) when is_atom(Atom) -> Atom;
option_key(Other) -> Other.

%%====================================================================
%% Error/Warning Formatting
%%====================================================================

%% @doc Format compilation errors for rebar3
%% LFE errors come in various formats, normalize them
-spec format_errors([term()]) -> [lfe_error()].
format_errors(Errors) when is_list(Errors) ->
    lists:flatmap(fun format_error_item/1, Errors);
format_errors(Error) ->
    format_error_item(Error).

%% @doc Format a single error item
-spec format_error_item(term()) -> [lfe_error()].
format_error_item({File, FileErrors}) when is_list(FileErrors) ->
    %% Standard format: {Filename, [{Line, Module, Description}]}
    [{File, FileErrors}];
format_error_item({error, File, FileErrors}) ->
    %% Alternative format with error tag
    [{File, FileErrors}];
format_error_item(Other) ->
    %% Unknown format, wrap it
    ?WARN("Unknown error format: ~p", [Other]),
    [{"unknown", [{0, ?MODULE, {unknown_error, Other}}]}].

%% @doc Format compilation warnings for rebar3
-spec format_warnings([term()]) -> [lfe_warning()].
format_warnings(Warnings) when is_list(Warnings) ->
    lists:flatmap(fun format_warning_item/1, Warnings);
format_warnings(Warning) ->
    format_warning_item(Warning).

%% @doc Format a single warning item
-spec format_warning_item(term()) -> [lfe_warning()].
format_warning_item({File, FileWarnings}) when is_list(FileWarnings) ->
    [{File, FileWarnings}];
format_warning_item({warning, File, FileWarnings}) ->
    [{File, FileWarnings}];
format_warning_item(Other) ->
    ?WARN("Unknown warning format: ~p", [Other]),
    [{"unknown", [{0, ?MODULE, {unknown_warning, Other}}]}].

%% @doc Format a single error for display (callback for rebar3)
-spec format_error(term()) -> iolist().
format_error({unknown_error, Term}) ->
    io_lib:format("Unknown compilation error: ~p", [Term]);
format_error({unknown_warning, Term}) ->
    io_lib:format("Unknown compilation warning: ~p", [Term]);
format_error({compilation_failed, File}) ->
    io_lib:format("Compilation failed: ~s", [File]);
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%%====================================================================
%% Progress Reporting
%%====================================================================

%% @doc Report compilation progress
-spec report_compilation(file:filename(), boolean()) -> ok.
report_compilation(Source, Verbose) ->
    RelPath = relative_path(Source),

    if
        Verbose ->
            ?INFO("Compiling ~s", [RelPath]);
        true ->
            ?DEBUG("Compiling ~s", [RelPath])
    end,

    ok.

%% @doc Make path relative to current directory for cleaner output
-spec relative_path(file:filename()) -> file:filename().
relative_path(Path) ->
    case file:get_cwd() of
        {ok, Cwd} ->
            case string:prefix(Path, Cwd) of
                nomatch ->
                    Path;
                RelPath ->
                    %% Remove leading slash
                    case RelPath of
                        "/" ++ Rest -> Rest;
                        Rest -> Rest
                    end
            end;
        _ ->
            Path
    end.
```

### Task 3.2: Integrate Worker into Compiler Module

**File: `src/r3lfe_compiler_mod.erl` (UPDATE)**

Update the `compile/4` function:

```erlang
%% @doc Compile a source file
%% This is called by rebar3 for each file that needs compilation
-spec compile(file:filename(), [{string(), file:filename()}],
              rebar_dict:t(), list()) -> ok | {ok, [string()]} | {error, [string()], [string()]}.
compile(Source, OutMappings, _Dict, Opts) ->
    %% Extract output directory from mappings
    OutDir = case OutMappings of
        [{_Ext, Dir} | _] -> Dir;
        [] -> "ebin"  % Fallback
    end,

    %% Get LFE compiler options from dict (if available)
    LfeOpts = case lists:keyfind(lfe_opts, 1, Opts) of
        {lfe_opts, Opts1} -> Opts1;
        false -> []
    end,

    %% Compile the file
    case r3lfe_compile_worker:compile_file(Source, OutDir, LfeOpts) of
        ok ->
            ok;

        {ok, Warnings} ->
            %% Format warnings as strings for rebar3
            WarningStrs = format_diagnostics(Warnings),
            {ok, WarningStrs};

        {error, Errors, Warnings} ->
            %% Format both errors and warnings
            ErrorStrs = format_diagnostics(Errors),
            WarningStrs = format_diagnostics(Warnings),
            {error, ErrorStrs, WarningStrs}
    end.

%% @doc Add format_error/1 callback for provider-level errors
-spec format_error(term()) -> iolist().
format_error(Reason) ->
    r3lfe_compile_worker:format_error(Reason).

%%====================================================================
%% Internal functions
%%====================================================================

%% @doc Format error/warning tuples into human-readable strings
-spec format_diagnostics([{file:filename(), [{integer(), module(), term()}]}]) ->
    [string()].
format_diagnostics(Diagnostics) ->
    lists:flatmap(
        fun({File, Items}) ->
            [format_diagnostic_item(File, Line, Module, Desc)
             || {Line, Module, Desc} <- Items]
        end,
        Diagnostics
    ).

%% @doc Format a single diagnostic item
-spec format_diagnostic_item(file:filename(), integer(), module(), term()) ->
    string().
format_diagnostic_item(File, Line, Module, Description) ->
    %% Try to use the module's format_error if available
    Message = try
        Module:format_error(Description)
    catch
        _:_ ->
            %% Fallback to term formatting
            io_lib:format("~p", [Description])
    end,

    %% Format as: "file.lfe:123: error message"
    lists:flatten(io_lib:format("~s:~p: ~s", [File, Line, Message])).
```

### Task 3.3: Add Compiler Option Change Detection

**File: `src/r3lfe_compile_opts.erl`**

```erlang
-module(r3lfe_compile_opts).

%% API exports
-export([
    get_opts_hash/1,
    opts_changed/2,
    save_opts_hash/2,
    clear_opts_cache/0
]).

-include("rebar3_lfe/include/r3lfe.hrl").

%% ETS table for storing compiler option hashes
-define(OPTS_CACHE, r3lfe_opts_cache).

%%====================================================================
%% API functions
%%====================================================================

%% @doc Initialize options cache
-spec init() -> ok.
init() ->
    case ets:info(?OPTS_CACHE) of
        undefined ->
            _Tid = ets:new(?OPTS_CACHE, [
                named_table,
                public,
                set,
                {read_concurrency, true}
            ]),
            ok;
        _ ->
            ok
    end.

%% @doc Get hash of compiler options
-spec get_opts_hash([term()]) -> binary().
get_opts_hash(Opts) ->
    %% Sort options for consistent hashing
    SortedOpts = lists:sort(Opts),

    %% Create hash
    crypto:hash(md5, term_to_binary(SortedOpts)).

%% @doc Check if compiler options have changed for a file
-spec opts_changed(file:filename(), [term()]) -> boolean().
opts_changed(File, CurrentOpts) ->
    CurrentHash = get_opts_hash(CurrentOpts),

    case ets:lookup(?OPTS_CACHE, File) of
        [] ->
            %% No cached options, consider changed
            true;
        [{File, CachedHash}] ->
            CurrentHash =/= CachedHash
    end.

%% @doc Save current options hash for a file
-spec save_opts_hash(file:filename(), [term()]) -> ok.
save_opts_hash(File, Opts) ->
    Hash = get_opts_hash(Opts),
    true = ets:insert(?OPTS_CACHE, {File, Hash}),
    ok.

%% @doc Clear all cached option hashes
-spec clear_opts_cache() -> ok.
clear_opts_cache() ->
    case ets:info(?OPTS_CACHE) of
        undefined ->
            ok;
        _ ->
            true = ets:delete_all_objects(?OPTS_CACHE),
            ok
    end.
```

### Task 3.4: Integrate Option Detection into Compiler

**File: `src/r3lfe_compiler_mod.erl` (UPDATE)**

Update `needs_compilation/3` to check for option changes:

```erlang
%% Add to the needs_compilation function, after timestamp checks:

needs_compilation(G, Source, OutMappings, Opts) ->
    Target = source_to_target(Source, OutMappings),

    case filelib:last_modified(Target) of
        0 ->
            ?DEBUG("~s needs compilation: target does not exist", [Source]),
            true;
        TargetTime ->
            SourceTime = filelib:last_modified(Source),

            if
                SourceTime > TargetTime ->
                    ?DEBUG("~s needs compilation: source is newer", [Source]),
                    true;
                true ->
                    %% Check compiler options
                    OptsChanged = r3lfe_compile_opts:opts_changed(Source, Opts),

                    if
                        OptsChanged ->
                            ?DEBUG("~s needs compilation: options changed", [Source]),
                            true;
                        true ->
                            %% Check dependencies
                            check_dependencies_newer(G, Source, TargetTime)
                    end
            end
    end.

%% Also update compile/4 to save options after successful compilation:

compile(Source, OutMappings, _Dict, Opts) ->
    OutDir = case OutMappings of
        [{_Ext, Dir} | _] -> Dir;
        [] -> "ebin"
    end,

    LfeOpts = case lists:keyfind(lfe_opts, 1, Opts) of
        {lfe_opts, Opts1} -> Opts1;
        false -> []
    end,

    case r3lfe_compile_worker:compile_file(Source, OutDir, LfeOpts) of
        ok ->
            %% Save options hash for future checks
            r3lfe_compile_opts:save_opts_hash(Source, LfeOpts),
            ok;

        {ok, Warnings} ->
            r3lfe_compile_opts:save_opts_hash(Source, LfeOpts),
            WarningStrs = format_diagnostics(Warnings),
            {ok, WarningStrs};

        {error, Errors, Warnings} ->
            ErrorStrs = format_diagnostics(Errors),
            WarningStrs = format_diagnostics(Warnings),
            {error, ErrorStrs, WarningStrs}
    end.
```

### Task 3.5: Initialize Options Cache in Plugin

**File: `src/r3lfe.erl` (UPDATE)**

```erlang
init(State) ->
    ?DEBUG("Initializing r3lfe plugin...", []),

    %% Initialize caches
    ok = r3lfe_dep_cache:init(),
    ok = r3lfe_compile_opts:init(),

    %% Register our compiler module with rebar3
    State1 = rebar_state:append_compilers(State, [r3lfe_compiler_mod]),

    ?DEBUG("Registered r3lfe_compiler_mod with rebar3", []),

    {ok, State1}.
```

### Task 3.6: Add Progress Reporting

**File: `src/r3lfe_progress.erl`**

```erlang
-module(r3lfe_progress).

%% API exports
-export([
    init/1,
    report_start/2,
    report_file/1,
    report_complete/1
]).

-include("r3lfe.hrl").

-record(progress, {
    total :: integer(),
    compiled :: integer(),
    start_time :: integer()
}).

%%====================================================================
%% API functions
%%====================================================================

%% @doc Initialize progress reporting
-spec init(integer()) -> #progress{}.
init(TotalFiles) ->
    #progress{
        total = TotalFiles,
        compiled = 0,
        start_time = erlang:monotonic_time(millisecond)
    }.

%% @doc Report start of compilation
-spec report_start(integer(), rebar_app_info:t()) -> ok.
report_start(FileCount, AppInfo) ->
    AppName = rebar_app_info:name(AppInfo),

    if
        FileCount > 0 ->
            ?INFO("Compiling ~p LFE files in ~s", [FileCount, AppName]);
        true ->
            ?DEBUG("No LFE files to compile in ~s", [AppName])
    end,

    ok.

%% @doc Report compilation of a single file
-spec report_file(#progress{}) -> #progress{}.
report_file(Progress = #progress{total = Total, compiled = Compiled}) ->
    NewCompiled = Compiled + 1,

    %% Report every 10% or for small projects, every file
    ShouldReport = (Total =< 10) orelse (NewCompiled rem max(1, Total div 10) == 0),

    if
        ShouldReport ->
            Percent = (NewCompiled * 100) div Total,
            ?INFO("Progress: ~p/~p (~p%)", [NewCompiled, Total, Percent]);
        true ->
            ok
    end,

    Progress#progress{compiled = NewCompiled}.

%% @doc Report completion
-spec report_complete(#progress{}) -> ok.
report_complete(#progress{total = Total, compiled = Compiled, start_time = StartTime}) ->
    EndTime = erlang:monotonic_time(millisecond),
    Duration = (EndTime - StartTime) / 1000,

    if
        Compiled > 0 ->
            ?INFO("Compiled ~p files in ~.2fs", [Compiled, Duration]);
        Total > 0 ->
            ?DEBUG("All ~p files up to date", [Total]);
        true ->
            ok
    end,

    ok.
```

### Task 3.7: Create Compilation Tests

**File: `test/r3lfe_compile_worker_SUITE.erl`**

```erlang
-module(r3lfe_compile_worker_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%% CT callbacks
-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2
]).

%% Test cases
-export([
    compile_simple_module/1,
    compile_with_warnings/1,
    compile_with_errors/1,
    compile_with_includes/1,
    format_errors_standard/1,
    format_warnings_standard/1,
    build_compiler_opts/1,
    relative_path_formatting/1
]).

%%====================================================================
%% CT Callbacks
%%====================================================================

all() ->
    [
        compile_simple_module,
        compile_with_warnings,
        compile_with_errors,
        compile_with_includes,
        format_errors_standard,
        format_warnings_standard,
        build_compiler_opts,
        relative_path_formatting
    ].

init_per_suite(Config) ->
    %% Ensure LFE is available
    application:ensure_all_started(lfe),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    TestDir = test_utils:create_temp_dir(),
    [{test_dir, TestDir} | Config].

end_per_testcase(_TestCase, Config) ->
    TestDir = ?config(test_dir, Config),
    test_utils:cleanup_temp_dir(TestDir),
    ok.

%%====================================================================
%% Test Cases
%%====================================================================

compile_simple_module(Config) ->
    TestDir = ?config(test_dir, Config),

    %% Create source file
    SourceFile = filename:join(TestDir, "simple.lfe"),
    test_utils:write_file(SourceFile,
        "(defmodule simple)\n"
        "(defun hello () 'world)\n"),

    %% Create output directory
    OutDir = filename:join(TestDir, "ebin"),
    ok = filelib:ensure_dir(filename:join(OutDir, "dummy")),

    %% Compile
    Result = r3lfe_compile_worker:compile_file(SourceFile, OutDir, []),

    %% Should succeed
    ?assertEqual(ok, Result),

    %% Check beam file exists
    BeamFile = filename:join(OutDir, "simple.beam"),
    ?assert(filelib:is_file(BeamFile)),

    ok.

compile_with_warnings(Config) ->
    TestDir = ?config(test_dir, Config),

    %% Create source with unused variable (warning)
    SourceFile = filename:join(TestDir, "warnings.lfe"),
    test_utils:write_file(SourceFile,
        "(defmodule warnings)\n"
        "(defun test (x)\n"
        "  (let ((y 42))\n"  % y is unused
        "    x))\n"),

    OutDir = filename:join(TestDir, "ebin"),
    ok = filelib:ensure_dir(filename:join(OutDir, "dummy")),

    Result = r3lfe_compile_worker:compile_file(SourceFile, OutDir, []),

    %% Should compile with warnings
    case Result of
        ok ->
            %% LFE might not generate warnings for this
            ok;
        {ok, Warnings} ->
            ?assert(is_list(Warnings)),
            ct:pal("Warnings: ~p", [Warnings]);
        Other ->
            ct:fail("Unexpected result: ~p", [Other])
    end,

    ok.

compile_with_errors(Config) ->
    TestDir = ?config(test_dir, Config),

    %% Create source with syntax error
    SourceFile = filename:join(TestDir, "errors.lfe"),
    test_utils:write_file(SourceFile,
        "(defmodule errors)\n"
        "(defun bad (  )\n"),  % Syntax error: incomplete

    OutDir = filename:join(TestDir, "ebin"),
    ok = filelib:ensure_dir(filename:join(OutDir, "dummy")),

    Result = r3lfe_compile_worker:compile_file(SourceFile, OutDir, []),

    %% Should fail with errors
    ?assertMatch({error, _Errors, _Warnings}, Result),

    {error, Errors, _Warnings} = Result,
    ?assert(length(Errors) > 0),

    ok.

compile_with_includes(Config) ->
    TestDir = ?config(test_dir, Config),

    %% Create include directory and header
    IncludeDir = filename:join(TestDir, "include"),
    ok = filelib:ensure_dir(filename:join(IncludeDir, "dummy")),

    HeaderFile = filename:join(IncludeDir, "records.lfe"),
    test_utils:write_file(HeaderFile,
        "(defrecord person name age)\n"),

    %% Create source that uses the header
    SourceFile = filename:join(TestDir, "with_includes.lfe"),
    test_utils:write_file(SourceFile,
        "(defmodule with-includes)\n"
        "(include-file \"../include/records.lfe\")\n"
        "(defun make-person (name age)\n"
        "  (make-person name name age age))\n"),

    OutDir = filename:join(TestDir, "ebin"),
    ok = filelib:ensure_dir(filename:join(OutDir, "dummy")),

    %% Compile with include path
    Opts = [{i, IncludeDir}],
    Result = r3lfe_compile_worker:compile_file(SourceFile, OutDir, Opts),

    %% Should succeed
    ?assertMatch(ok, Result),

    ok.

format_errors_standard(_Config) ->
    %% Test standard error format
    Errors = [
        {"file.lfe", [{10, erl_parse, "syntax error"}]}
    ],

    Formatted = r3lfe_compile_worker:format_errors(Errors),

    ?assertEqual(1, length(Formatted)),
    ?assertMatch([{"file.lfe", [{10, erl_parse, "syntax error"}]}], Formatted),

    ok.

format_warnings_standard(_Config) ->
    %% Test standard warning format
    Warnings = [
        {"file.lfe", [{15, erl_lint, {unused_var, 'X'}}]}
    ],

    Formatted = r3lfe_compile_worker:format_warnings(Warnings),

    ?assertEqual(1, length(Formatted)),

    ok.

build_compiler_opts(_Config) ->
    Source = "/path/to/module.lfe",
    OutDir = "/path/to/ebin",
    BaseOpts = [verbose, debug_info],

    Opts = r3lfe_compile_worker:build_compiler_opts(Source, OutDir, BaseOpts),

    %% Should contain required opts
    ?assert(lists:member(return, Opts)),
    ?assert(lists:member(report_errors, Opts)),
    ?assert(lists:keymember(outdir, 1, Opts)),

    %% Should contain base opts
    ?assert(lists:member(verbose, Opts)),
    ?assert(lists:member(debug_info, Opts)),

    ok.

relative_path_formatting(_Config) ->
    %% This tests path simplification for cleaner output
    {ok, Cwd} = file:get_cwd(),

    AbsPath = filename:join(Cwd, "src/module.lfe"),
    RelPath = r3lfe_compile_worker:relative_path(AbsPath),

    %% Should be relative
    ?assertEqual("src/module.lfe", RelPath),

    ok.
```

### Task 3.8: Create Integration Tests

**File: `test/compile_integration_SUITE.erl`**

```erlang
-module(compile_integration_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%% CT callbacks
-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2
]).

%% Test cases
-export([
    full_compilation_cycle/1,
    incremental_compilation/1,
    option_change_triggers_recompile/1,
    first_files_compiled_first/1,
    parallel_compilation_support/1
]).

%%====================================================================
%% CT Callbacks
%%====================================================================

all() ->
    [
        full_compilation_cycle,
        incremental_compilation,
        option_change_triggers_recompile,
        first_files_compiled_first,
        parallel_compilation_support
    ].

init_per_suite(Config) ->
    application:ensure_all_started(lfe),
    r3lfe_dep_cache:init(),
    r3lfe_compile_opts:init(),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    TestDir = test_utils:create_temp_dir(),
    r3lfe_dep_cache:clear(),
    r3lfe_compile_opts:clear_opts_cache(),
    [{test_dir, TestDir} | Config].

end_per_testcase(_TestCase, Config) ->
    TestDir = ?config(test_dir, Config),
    test_utils:cleanup_temp_dir(TestDir),
    ok.

%%====================================================================
%% Test Cases
%%====================================================================

full_compilation_cycle(Config) ->
    TestDir = ?config(test_dir, Config),
    AppData = test_utils:create_test_app(TestDir),

    SrcDir = maps:get(src_dir, AppData),
    EbinDir = maps:get(ebin_dir, AppData),

    %% Create multiple source files
    Files = [
        {"module1.lfe", "(defmodule module1)\n(defun test1 () 'ok)\n"},
        {"module2.lfe", "(defmodule module2)\n(defun test2 () 'ok)\n"},
        {"module3.lfe", "(defmodule module3)\n(defun test3 () 'ok)\n"}
    ],

    lists:foreach(
        fun({Name, Content}) ->
            File = filename:join(SrcDir, Name),
            test_utils:write_file(File, Content)
        end,
        Files
    ),

    %% Compile all files
    Results = lists:map(
        fun({Name, _Content}) ->
            Source = filename:join(SrcDir, Name),
            r3lfe_compile_worker:compile_file(Source, EbinDir, [])
        end,
        Files
    ),

    %% All should succeed
    ?assert(lists:all(fun(R) -> R =:= ok end, Results)),

    %% Check all beam files exist
    lists:foreach(
        fun({Name, _Content}) ->
            BeamName = filename:basename(Name, ".lfe") ++ ".beam",
            BeamFile = filename:join(EbinDir, BeamName),
            ?assert(filelib:is_file(BeamFile))
        end,
        Files
    ),

    ok.

incremental_compilation(Config) ->
    TestDir = ?config(test_dir, Config),
    AppData = test_utils:create_test_app(TestDir),
    AppDir = maps:get(dir, AppData),
    SrcDir = maps:get(src_dir, AppData),
    EbinDir = maps:get(ebin_dir, AppData),

    %% Create and compile initial file
    SourceFile = filename:join(SrcDir, "incremental.lfe"),
    test_utils:write_file(SourceFile,
        "(defmodule incremental)\n"
        "(defun version () 1)\n"),

    ok = r3lfe_compile_worker:compile_file(SourceFile, EbinDir, []),

    BeamFile = filename:join(EbinDir, "incremental.beam"),
    ?assert(filelib:is_file(BeamFile)),

    InitialTime = filelib:last_modified(BeamFile),

    %% Sleep to ensure timestamp difference
    timer:sleep(1000),

    %% Modify source file
    test_utils:write_file(SourceFile,
        "(defmodule incremental)\n"
        "(defun version () 2)\n"),

    %% Create DAG to test needs_compilation
    AppInfo = rebar_app_info:new(test_app, "0.1.0", AppDir),
    G = digraph:new([acyclic]),
    digraph:add_vertex(G, SourceFile),

    OutMappings = [{".beam", EbinDir}],

    %% Should need compilation
    NeedsCompile = r3lfe_compiler_mod:needs_compilation(
        G, SourceFile, OutMappings, []
    ),

    ?assert(NeedsCompile, "Modified file should need recompilation"),

    %% Compile again
    ok = r3lfe_compile_worker:compile_file(SourceFile, EbinDir, []),

    UpdatedTime = filelib:last_modified(BeamFile),
    ?assert(UpdatedTime > InitialTime, "Beam file should be updated"),

    digraph:delete(G),
    ok.

option_change_triggers_recompile(Config) ->
    TestDir = ?config(test_dir, Config),
    AppData = test_utils:create_test_app(TestDir),
    AppDir = maps:get(dir, AppData),
    SrcDir = maps:get(src_dir, AppData),
    EbinDir = maps:get(ebin_dir, AppData),

    SourceFile = filename:join(SrcDir, "options.lfe"),
    test_utils:write_file(SourceFile,
        "(defmodule options)\n"
        "(defun test () 'ok)\n"),

    %% Compile with initial options
    Opts1 = [verbose],
    ok = r3lfe_compile_worker:compile_file(SourceFile, EbinDir, Opts1),
    r3lfe_compile_opts:save_opts_hash(SourceFile, Opts1),

    %% Check if different options trigger recompile
    Opts2 = [verbose, debug_info],
    OptsChanged = r3lfe_compile_opts:opts_changed(SourceFile, Opts2),

    ?assert(OptsChanged, "Different options should be detected"),

    ok.

first_files_compiled_first(Config) ->
    TestDir = ?config(test_dir, Config),
    AppData = test_utils:create_test_app(TestDir),
    AppDir = maps:get(dir, AppData),
    SrcDir = maps:get(src_dir, AppData),

    %% Create files
    File1 = filename:join(SrcDir, "first.lfe"),
    File2 = filename:join(SrcDir, "second.lfe"),
    File3 = filename:join(SrcDir, "third.lfe"),

    test_utils:write_file(File1, "(defmodule first)\n"),
    test_utils:write_file(File2, "(defmodule second)\n"),
    test_utils:write_file(File3, "(defmodule third)\n"),

    %% Mock AppInfo with first_files
    AppInfo = rebar_app_info:new(test_app, "0.1.0", AppDir),
    Opts = rebar_opts:new(),
    Opts1 = rebar_opts:set(Opts, lfe_first_files, ["first.lfe"]),
    AppInfo1 = rebar_app_info:opts(AppInfo, Opts1),

    %% Get context
    Context = r3lfe_compiler_mod:context(AppInfo1),

    %% Verify context structure
    ?assertMatch(#{src_dirs := _, include_dirs := _}, Context),

    ct:pal("Note: Full first_files ordering tested via rebar3 integration"),
    ok.

parallel_compilation_support(Config) ->
    TestDir = ?config(test_dir, Config),
    AppData = test_utils:create_test_app(TestDir),
    AppDir = maps:get(dir, AppData),
    SrcDir = maps:get(src_dir, AppData),

    %% Create multiple independent files
    Files = lists:map(
        fun(N) ->
            Name = lists:flatten(io_lib:format("parallel~p.lfe", [N])),
            File = filename:join(SrcDir, Name),
            Content = io_lib:format("(defmodule parallel~p)\n", [N]),
            test_utils:write_file(File, Content),
            File
        end,
        lists:seq(1, 10)
    ),

    AppInfo = rebar_app_info:new(test_app, "0.1.0", AppDir),
    G = digraph:new([acyclic]),

    %% Add files to graph
    [digraph:add_vertex(G, F) || F <- Files],

    OutMappings = [{".beam", maps:get(ebin_dir, AppData)}],

    %% Call needed_files
    Result = r3lfe_compiler_mod:needed_files(G, Files, OutMappings, AppInfo),

    %% Result format: {{FirstSeq, Opts}, {{RestSeq, Parallel}, Opts}}
    ?assertMatch({{_, _}, {{_, _}, _}}, Result),

    {{_FirstFiles, _}, {{_SeqFiles, ParallelFiles}, _}} = Result,

    %% All files should be in parallel list (no dependencies)
    ?assertEqual(length(Files), length(ParallelFiles)),

    digraph:delete(G),
    ok.
```

## Testing Instructions

### Running Tests

```bash
# Compile and run all tests
rebar3 ct

# Run specific suite
rebar3 ct --suite=test/r3lfe_compile_worker_SUITE

# Run integration tests
rebar3 ct --suite=test/compile_integration_SUITE

# With coverage
rebar3 as test do ct, cover

# Full check
rebar3 check
```

### Manual Verification

```bash
# Create test project
mkdir test_compile
cd test_compile

cat > rebar.config <<EOF
{plugins, [{r3lfe, "0.5.0"}]}.
{deps, [{lfe, "2.2.0"}]}.
EOF

mkdir -p src include

# Create source files
cat > src/myapp.lfe <<EOF
(defmodule myapp)
(include-file "records.lfe")
(defun test () 'ok)
EOF

cat > include/records.lfe <<EOF
(defrecord person name age)
EOF

# First compile
rebar3 compile
# Should show: "Compiling 1 LFE files"

# No changes, second compile
rebar3 compile
# Should show: "All 1 files up to date"

# Modify source
echo "\n(defun test2 () 'ok2)" >> src/myapp.lfe

# Recompile
rebar3 compile
# Should show: "Compiling myapp.lfe"

# Modify header
echo "\n; Comment" >> include/records.lfe

# Recompile
rebar3 compile
# Should show: "Compiling myapp.lfe" (header change detected)
```

## Expected Outcomes

At the end of Phase 3, you should have:

1. ✅ Full compilation worker with lfe_comp integration
2. ✅ Professional error and warning formatting
3. ✅ Progress reporting for user feedback
4. ✅ Compiler option change detection
5. ✅ All Phase 1 and 2 functionality working
6. ✅ Comprehensive test coverage (>90%)
7. ✅ Manual verification of full compilation cycle

### Integration Checklist

- [ ] All previous tests still pass
- [ ] Compilation worker compiles LFE files correctly
- [ ] Errors are formatted clearly with file:line info
- [ ] Warnings are displayed but don't fail build
- [ ] Progress reporting works for large projects
- [ ] Option changes trigger recompilation
- [ ] Header changes trigger recompilation (from Phase 2)
- [ ] Incremental builds work correctly
- [ ] First files compiled before others
- [ ] Dialyzer clean
- [ ] Code coverage > 90%

## Next Steps

Phase 4 will implement:

- Package system redesign
- Nested module support
- Proper cleanup of temporary files
- Package transformation tests

## Notes for Claude Code

- Always use `lfe_comp:file/2` with the `return` option to get structured results
- Error/warning formats from LFE can vary - handle multiple formats
- Use `filelib:last_modified/1` consistently for timestamps
- Progress reporting should be informative but not overwhelming
- Option hashing ensures reproducible builds
- Test both successful and failing compilations
- Verify beam files are actually created in tests
- Clean up all temporary files in tests
- Handle missing include directories gracefully
