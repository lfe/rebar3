{erl_opts, [debug_info]}.

{deps, [
  {lfe, {git, "http://github.com/rvirding/lfe", {branch, "develop"}}}
]}.

{plugins, [
  {rebar3_lfe, {git, "http://github.com/lfe-rebar3/rebar3_lfe", {branch, "master"}}}
]}.

{relx, [{release, {'{{name}}', "0.1.0"},
         ['{{name}}',
          lfe,
          sasl]},

        {sys_config, "./config/sys.config"},
        {vm_args, "./config/vm.args"},

        {dev_mode, true},
        {include_erts, false},

        {extended_start_script, true}]
}.

{profiles, [{prod, [{relx, [{dev_mode, false},
                            {include_erts, true}]}]
            }]
}.
