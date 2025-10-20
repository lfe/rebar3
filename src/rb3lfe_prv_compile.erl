-module(rb3lfe_prv_compile).
-behaviour(provider).

-export([
    init/1,
    do/1,
    format_error/1
]).

-include_lib("rebar3_lfe/include/rb3lfe.hrl").

-define(PROVIDER, compile).
-define(DEPS, [{default, lock}]).

%%====================================================================
%% Provider API
%%====================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Description = "Compile LFE source files",

    Provider = providers:create([
        {namespace, ?NAMESPACE},
        {name, ?PROVIDER},
        {module, ?MODULE},
        {bare, true},
        {deps, ?DEPS},
        {example, "rebar3 lfe compile"},
        {opts, []},
        {short_desc, Description},
        {desc, info(Description)}
    ]),

    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    ?DEBUG("LFE compile provider starting", []),

    %% Set up code paths
    rebar_paths:set_paths([deps], State),

    try
        %% Get all project apps
        Apps = case rebar_state:current_app(State) of
            undefined ->
                %% Compiling all apps (umbrella or top-level)
                rebar_state:project_apps(State);
            AppInfo ->
                %% Compiling single app (via hook)
                [AppInfo]
        end,

        ?INFO("Compiling ~p LFE application(s)", [length(Apps)]),

        %% Compile each app
        lists:foreach(
            fun(AppInfo) ->
                compile_app(AppInfo, State)
            end,
            Apps
        ),

        {ok, State}
    catch
        throw:{error, Reason} ->
            {error, format_error(Reason)};
        error:Reason:Stack ->
            ?ERROR("Compilation failed with error: ~p", [Reason]),
            ?DEBUG("Stack trace: ~p", [Stack]),
            {error, format_error({compilation_error, Reason})}
    end.

-spec format_error(term()) -> iolist().
format_error({compilation_error, Reason}) ->
    io_lib:format("Compilation failed: ~p", [Reason]);
format_error({package_error, Reason}) ->
    io_lib:format("Package preparation failed: ~p", [Reason]);
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%%====================================================================
%% Internal functions
%%====================================================================

-spec compile_app(rebar_app_info:t(), rebar_state:t()) -> ok.
compile_app(AppInfo, State) ->
    AppName = rebar_app_info:name(AppInfo),
    ?DEBUG("Compiling LFE app: ~s", [AppName]),

    %% Ensure the .app.src is compiled to .app first
    rebar_otp_app:compile(State, AppInfo),

    %% Get source directories
    SrcDirs = rb3lfe_config:get_src_dirs(AppInfo),
    OutDir = rb3lfe_config:get_out_dir(AppInfo),

    %% Ensure output directory exists
    ok = rb3lfe_paths:ensure_dir(OutDir),

    %% Discover all LFE files
    AllFiles = lists:flatmap(
        fun rb3lfe_package:discover_files/1,
        SrcDirs
    ),

    case AllFiles of
        [] ->
            ?DEBUG("No LFE files found in ~s", [AppName]),
            ok;
        _ ->
            ?DEBUG("Found ~p LFE files in ~s", [length(AllFiles), AppName]),

            %% Prepare package files
            case rb3lfe_package:prepare_packages(AllFiles) of
                {ok, PackageInfos} ->
                    try
                        compile_files(AllFiles, PackageInfos, AppInfo, State)
                    after
                        %% Always cleanup packages
                        rb3lfe_package:cleanup_packages(PackageInfos)
                    end;
                {error, Reason} ->
                    throw({error, {package_error, Reason}})
            end
    end.

-spec compile_files([file:filename()], [map()],
                    rebar_app_info:t(), rebar_state:t()) -> ok.
compile_files(AllFiles, PackageInfos, AppInfo, _State) ->
    %% Build map of source -> temp for packages
    PackageMap = maps:from_list([
        {maps:get(source_file, Info), maps:get(temp_file, Info)}
        || Info <- PackageInfos
    ]),

    %% Determine which files to compile
    %% For package files, compile the temp file instead
    FilesToCompile = lists:map(
        fun(File) ->
            case maps:get(File, PackageMap, undefined) of
                undefined -> File;  % Not a package, use original
                TempFile -> TempFile  % Package, use temp
            end
        end,
        AllFiles
    ),

    %% Get first files
    FirstFiles = rb3lfe_config:get_first_files(AppInfo),

    %% Separate first files from rest
    {First, Rest} = lists:partition(
        fun(File) ->
            %% Check if base name matches any first file
            BaseName = filename:basename(File),
            lists:any(
                fun(FirstFile) ->
                    filename:basename(FirstFile) =:= BaseName
                end,
                FirstFiles
            )
        end,
        FilesToCompile
    ),

    %% Compile in order: first files, then rest
    OrderedFiles = First ++ Rest,

    %% Get compiler options
    LfeOpts = rb3lfe_config:get_lfe_opts(AppInfo),
    OutDir = rb3lfe_config:get_out_dir(AppInfo),

    %% Initialize progress
    Progress = rb3lfe_progress:init(length(OrderedFiles)),
    rb3lfe_progress:report_start(length(OrderedFiles), AppInfo),

    %% Compile each file
    {_FinalProgress, Results} = lists:foldl(
        fun(File, {Prog, Acc}) ->
            Result = rb3lfe_compile_worker:compile_file(File, OutDir, LfeOpts),
            NewProg = rb3lfe_progress:report_file(Prog),
            {NewProg, [Result | Acc]}
        end,
        {Progress, []},
        OrderedFiles
    ),

    rb3lfe_progress:report_complete(Progress),

    %% Check for errors
    Errors = [R || R <- Results, element(1, R) =:= error],

    case Errors of
        [] ->
            ok;
        _ ->
            throw({error, {compilation_failed, length(Errors)}})
    end.

-spec info(string()) -> iolist().
info(Description) ->
    io_lib:format(
        "~n~s~n"
        "~n"
        "Compiles all LFE source files in the project. Supports:~n"
        "  - Flat module structure (src/*.lfe)~n"
        "  - Nested packages (src/my/package/*.lfe)~n"
        "  - Mixed structures~n"
        "  - Header file dependency tracking~n"
        "  - Incremental compilation~n"
        "~n"
        "LFE compiler options can be specified via 'lfe_opts' or 'erl_opts'~n"
        "in rebar.config.~n",
        [Description]
    ).
