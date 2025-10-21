repl:
	ERL_AFLAGS=$$'-prompt \'\033[1;32mlfe\033[0m\033[33m>\033[0m \'' rlwrap --always-readline -H ~/.lfe_rlwrap_history rebar3 lfe repl
