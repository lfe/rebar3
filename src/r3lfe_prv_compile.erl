-module(r3lfe_prv_compile).
-behaviour(provider).

-export([
    init/1,
    do/1,
    format_error/1
]).

%% For testing
-export([
    info/1,
    get_dep_include_dirs/1
]).

-include_lib("rebar3_lfe/include/r3lfe.hrl").

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

    %% Set up code paths (include plugins for LFE compiler access)
    rebar_paths:set_paths([deps, plugins], State),

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

-spec format_error(term()) -> string().
format_error({compilation_failed, Count}) ->
    lists:flatten(io_lib:format("Compilation failed for ~p file(s)", [Count]));
format_error({compilation_error, Reason}) ->
    lists:flatten(io_lib:format("Compilation failed: ~p", [Reason]));
format_error({package_error, Reason}) ->
    lists:flatten(io_lib:format("Package preparation failed: ~p", [Reason]));
format_error(Reason) ->
    lists:flatten(io_lib:format("~p", [Reason])).

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
    SrcDirs = r3lfe_config:get_src_dirs(AppInfo),
    OutDir = r3lfe_config:get_out_dir(AppInfo),

    %% Ensure output directory exists
    ok = r3lfe_paths:ensure_dir(OutDir),

    %% Add app's ebin to code path so include-lib can find it
    %% This must be done AFTER rebar_otp_app:compile creates the .app file
    code:add_patha(OutDir),

    %% Ensure all dependency applications are loaded
    %% This is critical for include-lib to work (it uses code:lib_dir/1)
    ensure_dep_apps_loaded(State),

    %% Discover all LFE files, preserving which source directory each came from
    AllFilesWithSrcDirs = lists:flatmap(
        fun(SrcDir) ->
            Files = r3lfe_package:discover_files(SrcDir),
            %% Tag each file with its source directory
            [{File, SrcDir} || File <- Files]
        end,
        SrcDirs
    ),

    case AllFilesWithSrcDirs of
        [] ->
            ?DEBUG("No LFE files found in ~s", [AppName]),
            ok;
        _ ->
            ?DEBUG("Found ~p LFE files in ~s", [length(AllFilesWithSrcDirs), AppName]),

            %% Check if we have any nested files (packages)
            HasPackages = lists:any(
                fun({File, SrcDir}) ->
                    r3lfe_package:is_nested_file(File, SrcDir)
                end,
                AllFilesWithSrcDirs
            ),

            case HasPackages of
                true ->
                    %% Use manual compilation with package transformation
                    ?DEBUG("Found nested packages, using manual compilation", []),
                    compile_with_packages(AllFilesWithSrcDirs, AppInfo, State);
                false ->
                    %% Use rebar3's incremental compilation infrastructure
                    ?DEBUG("No packages found, using rebar3 incremental compilation", []),
                    compile_with_rebar3(AppInfo)
            end
    end.

%% @doc Compile using rebar3's infrastructure for incremental builds
-spec compile_with_rebar3(rebar_app_info:t()) -> ok.
compile_with_rebar3(AppInfo) ->
    try
        %% Use rebar3's compiler infrastructure
        %% This gives us incremental builds, DAG tracking, and proper dependency management
        rebar_compiler:compile_all([r3lfe_compiler_mod], AppInfo),
        ok
    catch
        error:{badmatch, {error, _}} = Error ->
            ?ERROR("Compilation failed: ~p", [Error]),
            throw({error, compilation_failed});
        error:Error:Stack ->
            ?ERROR("Compilation failed: ~p", [Error]),
            ?DEBUG("Stack trace: ~p", [Stack]),
            throw({error, compilation_failed})
    end.

%% @doc Compile with package transformation (manual flow)
-spec compile_with_packages([{file:filename(), file:filename()}],
                           rebar_app_info:t(), rebar_state:t()) -> ok.
compile_with_packages(AllFilesWithSrcDirs, AppInfo, State) ->
    %% Prepare package files
    ?DEBUG("Preparing packages", []),
    case r3lfe_package:prepare_packages(AllFilesWithSrcDirs) of
        {ok, PackageInfos} ->
            try
                compile_files(AllFilesWithSrcDirs, PackageInfos, AppInfo, State)
            after
                %% Always cleanup packages
                r3lfe_package:cleanup_packages(PackageInfos)
            end;
        {error, Reason} ->
            throw({error, {package_error, Reason}})
    end.

-spec compile_files([{file:filename(), file:filename()}], [map()],
                    rebar_app_info:t(), rebar_state:t()) -> ok.
compile_files(AllFilesWithSrcDirs, PackageInfos, AppInfo, State) ->
    %% Build a map of source -> temp for packages
    PackageMap = maps:from_list([
        {maps:get(source_file, Info), maps:get(temp_file, Info)}
        || Info <- PackageInfos
    ]),

    %% Determine which files to compile
    %% For package files, compile the temp file instead
    %% Keep the source directory associated with each file
    FilesToCompile = lists:map(
        fun({File, SrcDir}) ->
            case maps:get(File, PackageMap, undefined) of
                undefined -> {File, SrcDir};  % Not a package, use original
                TempFile -> {TempFile, SrcDir}  % Package, use temp
            end
        end,
        AllFilesWithSrcDirs
    ),

    %% Get first files
    FirstFiles = r3lfe_config:get_first_files(AppInfo),

    %% Separate first files from rest
    {First, Rest} = lists:partition(
        fun({File, _SrcDir}) ->
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
    LfeOpts = r3lfe_config:get_lfe_opts(AppInfo),
    OutDir = r3lfe_config:get_out_dir(AppInfo),

    %% Add include directories to compiler options
    IncludeDirs = r3lfe_config:get_include_dirs(AppInfo),
    
    %% ALSO add all dependency include directories
    %% This is critical for include-lib to work with test dependencies like ltest
    DepIncludeDirs = get_dep_include_dirs(State),
    
    AllIncludeDirs = lists:usort(IncludeDirs ++ DepIncludeDirs),
    IncludeOpts = [{i, Dir} || Dir <- AllIncludeDirs],
    FinalOpts = LfeOpts ++ IncludeOpts,

    %% Initialize progress
    Progress = r3lfe_progress:init(length(OrderedFiles)),
    r3lfe_progress:report_start(length(OrderedFiles), AppInfo),

    %% Compile each file (we only need the File, not SrcDir, for compilation)
    {_FinalProgress, Results} = lists:foldl(
        fun({File, _SrcDir}, {Prog, Acc}) ->
            Result = r3lfe_compile_worker:compile_file(File, OutDir, FinalOpts),
            NewProg = r3lfe_progress:report_file(Prog),
            {NewProg, [Result | Acc]}
        end,
        {Progress, []},
        OrderedFiles
    ),

    r3lfe_progress:report_complete(Progress),

    %% Check for errors
    Errors = [R || R <- Results, element(1, R) =:= error],

    case Errors of
        [] ->
            ok;
        _ ->
            throw({error, {compilation_failed, length(Errors)}})
    end.

%% @doc Get include directories from all dependencies
%% This allows include-lib directives to find files in dependency include dirs
-spec get_dep_include_dirs(rebar_state:t()) -> [file:filename()].
get_dep_include_dirs(State) ->
    %% Get all dependencies (includes both regular deps and test profile deps)
    AllDeps = rebar_state:all_deps(State),
    
    lists:flatmap(
        fun(DepAppInfo) ->
            DepDir = rebar_app_info:dir(DepAppInfo),
            IncludeDir = filename:join(DepDir, "include"),
            case filelib:is_dir(IncludeDir) of
                true -> 
                    ?DEBUG("Adding dependency include dir: ~s", [IncludeDir]),
                    [IncludeDir];
                false -> 
                    []
            end
        end,
        AllDeps
    ).

%% @doc Ensure all dependency applications are available for include-lib
%% This adds dependency ebin directories to the code path so code:lib_dir/1 works
-spec ensure_dep_apps_loaded(rebar_state:t()) -> ok.
ensure_dep_apps_loaded(State) ->
    AllDeps = rebar_state:all_deps(State),
    
    lists:foreach(
        fun(DepAppInfo) ->
            AppName = rebar_app_info:name(DepAppInfo),
            EbinDir = rebar_app_info:ebin_dir(DepAppInfo),
            
            %% Add to code path - this is what code:lib_dir/1 uses
            case code:add_patha(EbinDir) of
                true ->
                    ?DEBUG("Added to code path: ~s", [EbinDir]);
                {error, bad_directory} ->
                    ?WARN("Bad directory for ~s: ~s", [AppName, EbinDir])
            end,
            
            %% Try to load the application if .app file exists
            %% This is optional - code:lib_dir/1 works with just the code path
            AppNameAtom = case is_binary(AppName) of
                true -> binary_to_atom(AppName, utf8);
                false -> AppName
            end,
            
            case application:load(AppNameAtom) of
                ok -> 
                    ?DEBUG("Loaded application: ~s", [AppName]);
                {error, {already_loaded, _}} -> 
                    ?DEBUG("Application already loaded: ~s", [AppName]);
                {error, _Reason} ->
                    %% This is OK - as long as the path is set, include-lib will work
                    ?DEBUG("Could not load ~s (path is set, include-lib should still work)", 
                           [AppName])
            end
        end,
        AllDeps
    ),
    ok.

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
        "in rebar.config.~n"
        "~n"
        "Source directories can be configured via:~n"
        "  {erl_opts, [{src_dirs, [\"src\", \"test\"]}]}.~n",
        [Description]
    ).
