-module(rebar3_lfe_clean).

-export([delete_ebins/1, 
         delete_ebin/1]).

delete_ebins(AppEbin) ->
    Dirs = case file:list_dir(AppEbin) of
              {ok, Filenames} -> Filenames;
              {error, Reason} ->
                  rebar_api:error("Could not get files in ~p: ~p", [AppEbin, Reason]),
                  []
           end,
    [delete_ebin(filename:join([AppEbin, Filename])) || Filename <- Dirs].

delete_ebin(Filename) ->
    case file:delete(Filename) of
        ok -> rebar_api:info("Deleted ~s", [Filename]);
        {error, Reason} -> rebar_api:error("Problem deleting ~s: ~p", [Filename, Reason])
    end.
