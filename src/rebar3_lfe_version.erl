-module(rebar3_lfe_version).

-export([app_version/1,
         app_and_version/1,
         rebar_versions/0,
         language_versions/0,
         versions/1,
         versions_with_tooling/1]).

app_version(AppName) ->
  application:load(AppName),
  case application:get_key(AppName, vsn) of
    {ok, Vsn} -> Vsn;
    Default -> Default
  end.

app_and_version(AppName) ->
    {AppName, app_version(AppName)}.

rebar_versions() ->
    [{rebar, app_version(rebar)},
     {rebar3_lfe, app_version(rebar3_lfe)}
    ].

language_versions() ->
    [{lfe, app_version(lfe)},
     {erlang, erlang:system_info(otp_release)},
     {emulator, erlang:system_info(version)},
     {driver_version, erlang:system_info(driver_version)}
    ].

versions(AppNames) ->
    [{apps, [app_and_version(AppName) || AppName <- AppNames]},
     {languages, language_versions()}].

versions_with_tooling(AppNames) ->
    lists:append(versions(AppNames), [{tooling, rebar_versions()}]).
