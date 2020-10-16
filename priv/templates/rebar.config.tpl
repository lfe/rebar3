{erl_opts, [debug_info]}.

{deps, [
  {lfe, {git, "https://github.com/rvirding/lfe", {branch, "develop"}}}
]}.

{plugins, [
  {rebar3_lfe, {git, "https://github.com/lfe-rebar3/rebar3_lfe", {branch, "release/0.3.x"}}}
]}.

{provider_hooks, [
  {pre, [{compile, {lfe, compile}}]}
]}.
    
{profiles, [
  {test, [
    {deps, [
      {ltest, {git, "https://github.com/lfex/ltest", {branch, "master"}}}
    ]},
    {eunit_opts, [verbose]},
    {erl_opts, [{src_dirs, ["src", "test"]}]}
  ]}
]}.
