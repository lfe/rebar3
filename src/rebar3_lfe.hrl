-define(NAMESPACE, lfe).

-define(LFE, <<lfe>>).
-define(LIB, "lib").
-define(PLUGINS, "plugins").
-define(PLUGIN, "rebar3_lfe").
-define(ERL_REBAR_FILES, ["erl_crash.dump",
                          "rebar3.crashdump",
                          "rebar.lock"]).
-define(FILE_REGEX, ".*[le][fr][el]$").

-define(PRV_ERROR(Reason),
        {error, {?MODULE, Reason}}).
