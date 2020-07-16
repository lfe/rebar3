-module(rebar3_lfe_clean).

-export([delete_files/1, 
         delete_file/1]).

%% For whatever reason, rebar_file_utils:delete_each doesn't seem to work?
%% Maybe I just used it wrong; anway, reverted use of that to the custom
%% functions below.

delete_files(Path) ->
    Filenames = case file:list_dir(Path) of
      {ok, Fs} -> Fs;
      {error, Reason} ->
          rebar_api:error("Could not get files in ~p: ~p", [Path, Reason]),
          []
      end,
    [delete_file(filename:join([Path, Filename])) || Filename <- Filenames].

delete_file(Filename) ->
    case file:delete(Filename) of
        ok -> rebar_api:info("Deleted ~s", [Filename]);
        {error, Reason} -> rebar_api:error("Problem deleting ~s: ~p", [Filename, Reason])
    end.
