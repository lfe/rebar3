-module(rebar3_lfe_prv_compile).

-export([init/1, do/1, format_error/1]).
-export([compile/1, compile_app/1, compile_file/3]).

-define(PROVIDER, compile).
-define(NAMESPACE, lfe).
-define(NAMESPACE_PROVIDER, {?NAMESPACE, ?PROVIDER}).
-define(DEPS, [{default, lock}]).
-define(LFE, <<lfe>>).

%% =============================================================================
%% Public API
%% =============================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
  Provider = providers:create([{namespace,  ?NAMESPACE},
                               {name,       ?PROVIDER},
                               {module,     ?MODULE},
                               {bare,       true},
                               {deps,       ?DEPS},
                               {example,    "rebar3 lfe compile"},
                               {opts,       []},
                               {short_desc, "Compile LFE project"},
                               {desc,       info("Compile LFE project")}
                            ]),
  {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    rebar_api:debug("Starting do/1 for {lfe, compile} ...", []),
    rebar_api:console(" ~~~~> \tFinding .lfe files ...",[]),
    rebar_api:debug("\trebar_state module: ~p",
                    [code:where_is_file("rebar_state.beam")]),
    compile(State).

-spec format_error(any()) -> iolist().
format_error({missing_artifact, File}) ->
    io_lib:format("Missing artifact ~s", [File]);
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% =============================================================================
%% Internal functions
%% =============================================================================

-spec info(string()) -> iolist().
info(Description) ->
    io_lib:format(
        "~n~s~n"
        "~n"
        "No additional configuration options are required to compile~n"
        "LFE (*.lfe) files. The rebar 'erl_opts' setting is reused by~n"
        "LFE. For more information, see the rebar documentation for~n"
        "'erl_opts'.~n",
        [Description]).

compile_file(Source, Target, Config) ->
    rebar_api:console(" ~~~~> \tCompiling ~s ...",
                      [rebar3_lfe_utils:relative(Source)]),
    rebar_api:debug("\t\tOutput file: ~s",[Target]),
    LfeOpts = dict:fetch(lfe_opts, Config),
    rebar_api:debug("\t\tConfig: ~p", [LfeOpts]),
    CompileResults = lfe_comp:file(Source, LfeOpts),
    rebar_api:debug("\tCompile results: ~p", [CompileResults]),
    case CompileResults of
        {ok, _Mod} ->
            ok;
        {ok, _Mod, Ws} ->
            rebar_base_compiler:ok_tuple(Source, Ws);
        {error, [], Es, Ws} ->
            rebar_base_compiler:error_tuple(Source, Es, Ws, Config);
        {error, [{error, Es, Ws}|_], _Es, _Ws} ->
            rebar_base_compiler:error_tuple(Source, Es, Ws, Config)
    end.

compile_dir(Config, FirstFiles, SourceDir, TargetDir) ->
    rebar3_lfe_utils:ensure_dir(TargetDir),
    rebar_base_compiler:run(Config, FirstFiles,
                            SourceDir, ".lfe", TargetDir, ".beam",
                            fun compile_file/3).

compile(State) ->
    rebar_api:debug("\tCompiling LFE apps ...", []),
    Apps = rebar3_lfe_utils:get_apps(State),
    [compile_app(AppInfo) || AppInfo <- Apps],
    {ok, State}.

compile_app(AppInfo) ->
    rebar3_lfe_utils:copy_app_src(AppInfo),
    Opts = rebar_app_info:opts(AppInfo),
    AppDir = rebar_app_info:dir(AppInfo),
    OtherSrcDirs = rebar_dir:src_dirs(Opts),
    SourceDirs = rebar3_lfe_utils:get_src_dirs(AppDir, ["src"] ++ OtherSrcDirs),
    OutDir = rebar3_lfe_utils:relative_out_dir(AppInfo),
    FirstFiles = rebar3_lfe_utils:get_first_files(Opts, AppDir),
    Config = rebar3_lfe_utils:config(OutDir, Opts),
    rebar_api:debug("\tOtherSrcDirs: ~p", [OtherSrcDirs]),
    rebar_api:debug("\tAppInfoDir: ~p", [AppDir]),
    rebar_api:debug("\tSourceDirs: ~p", [SourceDirs]),
    rebar_api:debug("\tOutDir: ~p", [OutDir]),
    rebar_api:debug("\tFirstFiles: ~p", [FirstFiles]),
    rebar_api:debug("\tConfig: ~p", [dict:fetch(lfe_opts, Config)]),
    [compile_dir(Config, FirstFiles, Dir, OutDir) || Dir <- SourceDirs],
    rebar_api:debug("\tFinished compile.", []),
    code:add_patha(rebar3_lfe_utils:out_dir(rebar_app_info:dir(AppInfo))).
