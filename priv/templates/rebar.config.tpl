{erl_opts, [debug_info]}.

{deps, [
  {lfe, "2.0.0"}
]}.

{plugins, [
  {rebar3_lfe, "0.3.0"}
]}.

{provider_hooks, [
  {pre, [{compile, {lfe, compile}}]}
]}.
    
{profiles, [
  {test, [
    {deps, [
      {ltest, "0.13.0"}
    ]},
    {eunit_opts, [verbose]},
    {erl_opts, [{src_dirs, ["src", "test"]}]}
  ]}
]}.