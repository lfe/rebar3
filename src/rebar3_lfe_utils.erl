-module(rebar3_lfe_utils).

-export([config/2,
         copy_app_src/1,
         out_dir/0, out_dir/1,
         include_dir/0, include_dir/1,
         ensure_dir/1,
         get_apps/1,
         get_first_files/2,
         get_src_dirs/2,
         relative_out_dir/1,
         relative/1,
         lfe_config/1,
         first_value/2]).

%% The following spec was commented out in this commit:
%% * https://github.com/lfe-rebar3/compile/commit/cde13fdceec399c36635fe259074b7a01fcf3430
%% See the commit message for more information.
%% -spec config(file:dirname(), dict:dict()) -> dict:dict().
config(OutDir, Config) ->
    Key = lfe_opts,
    Defaults = [{outdir, OutDir}] ++ rebar_opts:erl_opts(Config) ++
        [{i, include_dir()}, return, verbose],
    case dict:is_key(Key, Config) of
        true  -> dict:append_list(Key, Defaults, Config);
        false -> dict:store(Key, Defaults, Config)
    end.

copy_app_src(AppInfo) ->
    rebar_api:debug("\t\tEntered copy_app_src/1 ...", []),
    AppOutDir = rebar_app_info:out_dir(AppInfo),
    AppSrcFile = rebar_app_info:app_file_src(AppInfo),
    AppFile = rebar_app_utils:app_src_to_app(AppOutDir, AppSrcFile),
    rebar_api:debug("\t\tAppOutDir: ~p", [AppOutDir]),
    rebar_api:debug("\t\tAppSrcFile: ~p", [AppSrcFile]),
    rebar_api:debug("\t\tAppFile: ~p", [AppFile]),
    rebar_api:debug("\t\tCopying ~p to ~p ...", [AppSrcFile, AppFile]),
    copy_file(AppSrcFile, AppFile).

copy_file(Src, Dst) ->
    case file:copy(Src, Dst) of
        {ok, BytesCopied} ->
            rebar_api:debug("\t\tCopied ~p bytes.", [BytesCopied]);
        {error, Reason} ->
            rebar_api:error("\t\tFailed to copy ~p: ~p", [Src, Reason])
    end.

out_dir() ->
    "ebin".

out_dir(AppDir) ->
    filename:join(AppDir, "ebin").

relative_out_dir(AppInfo) ->
    filename:join(rebar_app_info:out_dir(AppInfo), "ebin").

include_dir() ->
    "include".

include_dir(AppDir) ->
    filename:join(AppDir, "include").

-spec ensure_dir(file:dirname()) -> ok.
ensure_dir(OutDir) ->
    %% Make sure that ebin/ exists and is on the path
    ok = filelib:ensure_dir(filename:join(OutDir, "dummy.beam")),
    AbsOutDir = filename:absname(OutDir),
    rebar_api:debug("\t\tAdding ~p to path ...", [AbsOutDir]),
    true = code:add_patha(AbsOutDir),
    ok.

get_apps(State) ->
    case rebar_state:current_app(State) of
           undefined ->
             rebar_api:debug("\tCurrent app state is undefined ...", []),
             rebar_state:project_apps(State);
           AppInfo ->
             rebar_api:debug("\tConverting current app state to list ...", []),
             [AppInfo]
    end.

get_first_files(Opts, AppDir) ->
    Dirs = rebar_opts:get(Opts, lfe_first_files, []),
    [filename:join(AppDir, Dir) || Dir <- Dirs].

get_src_dirs(AppDir, Dirs) ->
    rebar_api:debug("\tDirs: ~p", [Dirs]),
    lists:usort([filename:join(AppDir, DirName) || DirName <- Dirs]).

relative(Filename) ->
    {ok, Cwd} = file:get_cwd(),
    re:replace(Filename, Cwd, ".", [{return,list}]).

lfe_config(State) ->
  rebar_state:get(State, lfe).

first_value([], _) -> no_value;
first_value([Fun | Rest], State) ->
    case Fun(State) of
        no_value ->
            first_value(Rest, State);
        Value ->
            Value
    end.
