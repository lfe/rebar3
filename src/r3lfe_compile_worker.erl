-module(r3lfe_compile_worker).

%% API exports
-export([
    compile_file/3,
    compile_file/4,
    format_errors/1,
    format_warnings/1,
    format_error/1,
    build_compiler_opts/3,
    relative_path/1
]).

-include("r3lfe.hrl").

-type lfe_error() :: {file:filename(), [{integer(), module(), term()}]}.
-type lfe_warning() :: {file:filename(), [{integer(), module(), term()}]}.
-type compile_opts() :: [term()].

-export_type([lfe_error/0, lfe_warning/0, compile_opts/0]).

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
    %% LFE compiler can return various formats
    case lfe_comp:file(Source, CompilerOpts) of
        {ok, _Module, []} ->
            %% Successful compilation, no warnings (LFE returns empty list)
            ?DEBUG("Successfully compiled ~s", [Source]),
            ok;

        {ok, Module} ->
            %% Successful compilation, no warnings (alternative format)
            ?DEBUG("Successfully compiled ~s -> ~s", [Source, Module]),
            ok;

        {ok, Module, Warnings} when Warnings =/= [] ->
            %% Successful compilation with warnings
            ?DEBUG("Compiled ~s -> ~s with warnings", [Source, Module]),
            FormattedWarnings = format_warnings(Warnings),
            {ok, FormattedWarnings};

        {error, Errors, Warnings} ->
            %% Compilation failed
            ?ERROR("Failed to compile ~s", [Source]),
            ?ERROR("Errors: ~p", [Errors]),
            FormattedErrors = format_errors(Errors),
            FormattedWarnings = format_warnings(Warnings),
            {error, FormattedErrors, FormattedWarnings};

        {error, [], FileErrors, []} when is_list(FileErrors) ->
            %% LFE error format: {error, [], [{File, Errors}], []}
            ?ERROR("Failed to compile ~s", [Source]),
            ?ERROR("File Errors: ~p", [FileErrors]),
            FormattedErrors = format_errors(FileErrors),
            {error, FormattedErrors, []};

        {error, NestedErrors, [], []} when is_list(NestedErrors) ->
            %% LFE nested error format: {error, [{error, FileErrors, []}], [], []}
            %% This happens with certain compiler warnings treated as errors
            ?ERROR("Failed to compile ~s", [Source]),
            ?ERROR("Nested Errors: ~p", [NestedErrors]),
            %% Extract file errors from nested format
            FileErrors = lists:flatmap(
                fun({error, Errs, _Warns}) -> Errs;
                   (Other) -> [Other]
                end,
                NestedErrors
            ),
            FormattedErrors = format_errors(FileErrors),
            {error, FormattedErrors, []}
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
report_compilation(Source, _Verbose) ->
    RelPath = relative_path(Source),
    ?INFO("Compiling ~s", [RelPath]),
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
