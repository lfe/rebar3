PROJECT = {{name}}

default: build

build:
	@rebar3 compile

repl: build
	@ERL_AFLAGS=$$'-prompt \'\033[1;32mlfe\033[0m\033[33m>\033[0m \'' rlwrap --always-readline -H ~/.lfe_rlwrap_history rebar3 lfe repl

clean:
	@rm -f ebin/*.beam  _build/default/lib/$(PROJECT)/ebin/*.beam *.beam test/*.beam

clean-all: clean
	@rm -rf _build rebar.lock erl_crash.dump rebar3.crashdump

build-tests:
	@rebar3 as test compile

coverage:
	@rebar3 as test coverge

test:
	@rebar3 as test ltest

check:
	@rebar3 as test check

publish: clean
	rebar3 as hexpm hex publish
