{erl_opts, [no_debug_info]}.

{deps, [
  {lfe, {git, "http://github.com/rvirding/lfe", {branch, "develop"}}}
]}.

{plugins, [
  {rebar3_lfe, {git, "http://github.com/lfe-rebar3/rebar3_lfe", {branch, "master"}}}
]}.

{escript_incl_apps, [lfe, '{{name}}']}.
{escript_main_app, '{{name}}'}.
{escript_name, '{{name}}'}.
{escript_emu_args, "%%! +sbtu +A1\n"}.

%% Profiles
{profiles, [{test,
             [{erl_opts, [debug_info]}
            ]}]}.
