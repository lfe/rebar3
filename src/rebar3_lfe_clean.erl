-module(rebar3_lfe_clean).

-export([delete_files/1, 
         delete_file/1,
         beam_files/1,
         apps_beam_files/1,
         build/1,
         apps_build/1,
         cache/2,
         apps_cache/1,
         erl_rebar_files/0,
         all/1]).

-define(LIB, "lib").
-define(PLUGINS, "plugins").
-define(PLUGIN, "rebar3_lfe").
-define(ERL_REBAR_FILES, ["erl_crash.dump", 
                          "rebar3.crashdump", 
                          "rebar.lock"]).

%% For whatever reason, rebar_file_utils:delete_each doesn't seem to work?
%% Maybe I just used it wrong; anyway, reverted use of that to the custom
%% functions below.

delete_files(Path) ->
    Filenames = case file:list_dir(Path) of
      {ok, Fs} -> Fs;
      {error, Reason} ->
          rebar_api:warning("Could not get files in ~p: ~p", [Path, Reason]),
          []
      end,
    [delete_file(filename:join([Path, Filename])) || Filename <- Filenames].

delete_file(Filename) ->
    case filelib:is_file(Filename) of
      true -> case file:delete(Filename) of
                  ok -> rebar_api:info("Deleted ~s", [Filename]);
                  {error, Reason} -> rebar_api:warning("Problem deleting ~s: ~p", [Filename, Reason])
              end;
      false -> rebar_api:debug("File doesn't exist; skipping", [])
    end.

beam_files(AppInfo) ->
    AppEbin = rebar_app_info:ebin_dir(AppInfo),
    rebar_api:debug("AppEbin: ~p", [AppEbin]),
    delete_files(AppEbin).

apps_beam_files(State) ->
    Apps = rebar_state:project_apps(State),
    [beam_files(AppInfo) || AppInfo <- Apps].

build(AppInfo) ->
    rebar_api:debug("AppInfo: ~p", [AppInfo]),
    BuildDir = rebar_app_info:get(AppInfo, base_dir, "_build"),
    rebar_api:debug("BuildDir: ~p~n", [BuildDir]),
    rebar_file_utils:rm_rf(BuildDir).

apps_build(State) ->
    Apps = rebar_state:project_apps(State),
    [build(AppInfo) || AppInfo <- Apps].

cache(Opts, AppInfo) ->
    Dirs = cache_dirs(Opts, AppInfo),
    rebar_api:debug("~p~n", [Dirs]),
    [rebar_file_utils:rm_rf(Dir) || Dir <- Dirs].

apps_cache(State) ->
    Opts = rebar_state:opts(State),
    Apps = rebar_state:project_apps(State),
    [cache(Opts, AppInfo) || AppInfo <- Apps].

cache_dirs(Opts, AppInfo) ->
    Global = rebar_dir:global_cache_dir(Opts),
    AppName = rebar3_lfe_utils:app_name_str(AppInfo),
    rebar_api:debug("Global cache dir: ~p~n", [Global]),
    [filename:join([Global, ?LIB, AppName]),
     filename:join([Global, ?PLUGINS, AppName]),
     filename:join([Global, ?LIB, ?PLUGIN]),
     filename:join([Global, ?PLUGINS, ?PLUGIN])].

erl_rebar_files() ->
    [rebar3_lfe_clean:delete_file(File) || File <- ?ERL_REBAR_FILES].

all(State) ->
    erl_rebar_files(),
    apps_build(State),
    apps_cache(State).
