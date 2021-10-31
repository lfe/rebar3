-module(rebar3_lfe_utils).

-export([app_name/1,
         app_name_str/1,
         config/2,
         %%copy_app_src/1, copy_app_src/2,
         debug_get_value/4,
         ensure_dir/1,
         first_value/2,
         get_apps/1,
         get_first_files/2,
         get_src_dirs/2,
         include_dir/0, include_dir/1,
         lfe_config/1,
         out_dir/0, out_dir/1,
         relative/1,
         relative_out_dir/1,
         run_prehooks/2,
         set_diff/2,
         update_app_file/1]).

config(OutDir, Config) ->
    Key = lfe_opts,
    Defaults = [{outdir, OutDir}] ++ rebar_opts:erl_opts(Config) ++
        [{i, include_dir()}, return, verbose],
    case dict:is_key(Key, Config) of
        true  -> dict:append_list(Key, Defaults, Config);
        false -> dict:store(Key, Defaults, Config)
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
  rebar_state:get(State, lfe, []).

first_value([], _) -> no_value;
first_value([Fun | Rest], State) ->
    case Fun(State) of
        no_value ->
            first_value(Rest, State);
        Value ->
            Value
    end.

%% @doc Updates the list of modules in the .app file for the specified
%% directory.
%%
%% The .app file will be update to include all modules in its
%% `modules' entry. The modules listed are resolved by looking for all
%% files with the extension `.beam' in `Dir'.
-spec update_app_file(file:name()) -> ok.
update_app_file(Dir) ->
  case rebar_utils:find_files(Dir, ".app$", false) of
    [AppFile] ->
      {ok, [{application, AppName, AppDetail0}]} = file:consult(AppFile),

      BeamPaths = rebar_utils:find_files(Dir, ".beam$", false),
      Modules   = [ list_to_atom(filename:basename(Path, ".beam"))
                    || Path <- BeamPaths
                  ],
      AppDetail1  = lists:keyreplace( modules
                                    , 1
                                    , AppDetail0
                                    , {modules, Modules}
                                    ),
      SpecBefore = io_lib:format("~p.\n", [{application, AppName, AppDetail0}]),
      SpecAfter = io_lib:format("~p.\n", [{application, AppName, AppDetail1}]),

      rebar_api:debug("Updating app file for ~p", [AppName]),
      rebar_api:debug("~p.app (BEFORE):~n~s", [AppName, SpecBefore]),
      rebar_api:debug("~p.app (AFTER):~n~s", [AppName, SpecAfter]),

      ok = rebar_file_utils:write_file_if_contents_differ(AppFile, SpecAfter, utf8),
      ok;
    [] -> ok
  end.

app_name(AppInfo) ->
  rebar_api:debug("Getting app name (atom) ...", []),
  AppName = erlang:list_to_atom(app_name_str(AppInfo)),
  rebar_api:debug("AppName: ~p", [AppName]),
  AppName.

app_name_str(AppInfo) ->
  rebar_api:debug("Getting app name (string) ...", []),
  AppName = rebar_app_info:name(AppInfo),
  rebar_api:debug("AppName: ~p", [AppName]),
  case erlang:is_binary(AppName) of
      true -> erlang:binary_to_list(AppName);
      false ->
          rebar_api:error("Unexpected type for app name: ~p", [AppName]),
          "unknown"
  end.

set_diff(BigList, SmallList) ->
    sets:to_list(sets:subtract(sets:from_list(BigList),
    sets:from_list(SmallList))).

debug_get_value(Key, List, Default, Description) ->
    case proplists:get_value(Key, List, Default) of
        Default -> Default;
        Value ->
            rebar_api:debug(Description, []),
            Value
    end.

run_prehooks(State, Provider) ->
    Providers = rebar_state:providers(State),
    Cwd = rebar_dir:get_cwd(),
    rebar_hooks:run_project_and_app_hooks(Cwd, pre, Provider, Providers, State).
