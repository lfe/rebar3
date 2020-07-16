# rebar3_lfe

[![Build Status][gh-actions-badge]][gh-actions] [![LFE Versions][lfe badge]][lfe] [![Erlang Versions][erlang badge]][versions] [![Tag][github tag badge]][github tag]

*A comprehensive LFE rebar3 plugin for all your LFE tooling needs*

[![Project Logo][logo]][logo-large]

#### Contents

* [About](#about-)
* [Features](#features-)
* [Setup](#setup-)
* [Use](#use-)
  * [Compiling](#compiling-)
  * [Running the LFE REPL](#running-the-lfe-repl-)
  * Creating Projects
    * [Creating and Using a Library](#creating-and-using-a-library-)
    * [Creating and Running a `main` Script](#creating-and-running-a-main-script-)
    * [Creating and Running an LFE escript Application](#creating-and-running-an-lfe-escript-application-)
    * [Creating and Running an LFE/OTP Application](#creating-and-running-an-lfeotp-application-)
    * [Creating and Running an LFE/OTP Release](#creating-and-running-an-lfeotp-release-)
  * [Cleanup](#cleanup-)
  * Metadata
    * [Get Versions](#get-versions-)

## About [&#x219F;](#contents)

This plugin originally started life as a shell script (`lfetool` -- there's
even a T-shirt for it!), then it toyed with integrating with `rebar` (the
original). Around that time, though, `rebar3` was under initial development,
and LFE took a chance on it as an early adopter. This lead to a whole series of
LFE plugins, but after a few years momentum was lost. 

Those early `rebar3` efforts have been combined into a single plugin in this
project, with many updates and using all the latest approaches developed in
`rebar3`'s now mature ecosystem.

## Features [&#x219F;](#contents)

* Create new LFE projects:
  * `rebar3 new lfe-lib`
  * `rebar3 new lfe-main`
  * `rebar3 new lfe-escript`
  * `rebar3 new lfe-app`
  * `rebar3 new lfe-release`
* Start up an LFE REPL:
  * `rebar3 lfe repl`
* Compile LFE source code:
  * `rebar3 lfe compile`
* Run an LFE project's `main/1` function as an lfescript (run `rebar3 new lfe-main` to see an example):
  * `rebar3 lfe run`
  * `rebar3 lfe run -- 1 2 5`
  * `rebar3 lfe run -main some/path/main.lfe`
* Escriptize an LFE escript project:
  * `rebar3 ecsriptize`
* Run an escriptized LFE escript project:
  * `rebar3 lfe run-ecsript`
* Generate an LFE/OTP release
  * `rebar3 release`
* Cleanup
  * `rebar3 lfe clean`
  * `rebar3 lfe clean-build`
  * `rebar3 lfe clean-cache`
  * `rebar3 lfe clean-all`
* Metadata
  * `rebar3 lfe versions`

[More coming soon!](https://github.com/lfe-rebar3/rebar3_lfe/issues?q=is%3Aissue+is%3Aopen+label%3Afeature)

## Setup [&#x219F;](#contents)

Add the plugin to your ``rebar.config``:

```erlang
{plugins, [
  {rebar3_lfe,
    {git, "https://github.com/lfe-rebar3/rebar2_lfe.git", {tag, "0.2.0"}}}
]}.
```

## Use [&#x219F;](#contents)

### Compiling [&#x219F;](#contents)

Change directory to your LFE project, and then:

```shell
$ rebar3 lfe compile
```

### Running the LFE REPL [&#x219F;](#contents)

Change directory to your LFE project, and then:

```shell
$ rebar3 lfe repl
```

### Creating Projects

#### Creating and Using a Library [&#x219F;](#contents)

```shell
$ rebar3 new lfe-lib mything
```

```text
===> Compiling rebar3_lfe
===> Writing mything/README.md
===> Writing mything/LICENSE
===> Writing mything/rebar.config
===> Writing mything/.gitignore
===> Writing mything/src/mything.lfe
===> Writing mything/src/mything.app.src
```

```shell
$ cd mything
$ rebar3 lfe repl
```

```lisp
lfe> (mything:my-fun)
hello-world
```

#### Creating and Running a `main` Script [&#x219F;](#contents)

```shell
$ rebar3 new lfe-main mymain
```

```text
===> Writing mymain/README.md
===> Writing mymain/LICENSE
===> Writing mymain/rebar.config
===> Writing mymain/.gitignore
===> Writing mymain/src/mymain.lfe
===> Writing mymain/scripts/main.lfe
===> Writing mymain/src/mymain.app.src
```

```shell
$ cd mymain
$ rebar3 lfe run -- 42
```

```text
Running script '/usr/local/bin/rebar3' with args [<<"42">>] ...
'hello-world'
```

#### Creating and Running an LFE escript Application [&#x219F;](#contents)

```shell
$ rebar3 new lfe-escript myapp
```

```text
===> Writing myapp/README.md
===> Writing myapp/LICENSE
===> Writing myapp/rebar.config
===> Writing myapp/.gitignore
===> Writing myapp/src/myapp.lfe
===> Writing myapp/src/myapp.app.src
```

```shell
$ cd myapp
$ rebar3 lfe compile
$ rebar3 escriptize
```

```lisp
$ rebar3 lfe run-escript 1 2 5 no '3!'
Got args: ("1" "2" "5" "no" "3!")
Answer: 42
```

#### Creating and Running an LFE/OTP Application [&#x219F;](#contents)

```shell
$ rebar3 new lfe-app myapp
```

```text
===> Writing myapp/README.md
===> Writing myapp/LICENSE
===> Writing myapp/rebar.config
===> Writing myapp/.gitignore
===> Writing myapp/src/myapp.lfe
===> Writing myapp/src/myapp-app.lfe
===> Writing myapp/src/myapp-sup.lfe
===> Writing myapp/src/myapp.app.src
```

```shell
$ cd myapp
$ rebar3 lfe repl
```

```lisp
lfe> (application:ensure_all_started 'myapp)
#(ok (myapp))
lfe> (erlang:whereis 'myapp-sup)
#Pid<0.205.0>
lfe> (myapp:echo "testing the supervised gen_server ...")
"testing the supervised gen_server ..."
```

#### Creating and Running an LFE/OTP Release [&#x219F;](#contents)

```shell
$ rebar3 new lfe-release prod-app
```

```text
===> Writing prod-app/README.md
===> Writing prod-app/LICENSE
===> Writing prod-app/rebar.config
===> Writing prod-app/.gitignore
===> Writing prod-app/apps/prod-app/src/prod-app.lfe
===> Writing prod-app/apps/prod-app/src/prod-app-app.lfe
===> Writing prod-app/apps/prod-app/src/prod-app-sup.lfe
===> Writing prod-app/apps/prod-app/src/prod-app.app.src
===> Writing prod-app/config/sys.config
===> Writing prod-app/config/vm.args
```

```shell
$ rebar3 release
$ ./_build/default/rel/prod-app/bin/prod-app console
```

```erlang
Eshell V11.0  (abort with ^G)
(prod-app@spacemac)1> lfe_shell:start().
```

```lisp
(prod-app@spacemac)lfe> (erlang:whereis 'prod-app-sup)
#Pid<0.351.0>
(prod-app@spacemac)lfe> (prod-app:echo "testing from production!")
"testing from production!"
```

Additionally, you can run the release script commands (the non-interactive,
non-shell commands) via the LFE rebar3 plugin:

```shell
$ rebar3 lfe run-release ping
pong
```

### Cleanup

Remove the apps' `ebin/*` files:

```shell
$ rebar3 lfe clean
```

Remove the `_build` directory:

```shell
$ rebar3 lfe clean-build
```

Remove the the cached directories for the app and the rebar3_lfe plugin, both
global and local:

```shell
$ rebar3 lfe clean-cache
```

Perform all clean tasks:

```shell
$ rebar3 lfe clean-all
```

### Metadata

#### Get Versions

```shell
$ rebar3 lfe versions
```

```lisp
(#(apps
   (#(prod-app git)))
 #(languages
   (#(lfe "1.3-dev")
    #(erlang "23")
    #(emulator "11.0")
    #(driver_version "3.3")))
 #(tooling
   (#(rebar "3.10.0")
    #(rebar3_lfe "0.1.0"))))
```


<!-- Named page links below: /-->

[logo]: https://avatars2.githubusercontent.com/u/15242004?s=250
[logo-large]: https://avatars2.githubusercontent.com/u/15242004
[github]: https://github.com/lfe-rebar3/rebar3_lfe
[gitlab]: https://gitlab.com/lfe-rebar3/rebar3_lfe
[gh-actions-badge]: https://github.com/lfe-rebar3/rebar3_lfe/workflows/ci%2Fcd/badge.svg
[gh-actions]: https://github.com/lfe-rebar3/rebar3_lfe/actions
[lfe]: https://github.com/rvirding/lfe
[lfe]: https://github.com/rvirding/lfe
[lfe badge]: https://img.shields.io/badge/lfe-1.3.0%E2%88%92dev-blue.svg
[erlang badge]: https://img.shields.io/badge/erlang-19%E2%88%9223-blue.svg
[versions]: https://github.com/lfe-rebar3/rebar3_lfe/blob/master/.travis.yml
[github tag]: https://github.com/lfe-rebar3/rebar3_lfe/tags
[github tag badge]: https://img.shields.io/github/tag/lfe-rebar3/rebar3_lfe.svg
[github downloads]: https://img.shields.io/github/downloads/atom/atom/total.svg
[hex badge]: https://img.shields.io/hexpm/v/rebar3_lfe.svg?maxAge=2592000
[hex package]: https://hex.pm/packages/rebar3_lfe
[hex downloads]: https://img.shields.io/hexpm/dt/rebar3_lfe.svg

<!-- Unused badges:
[![Downloads][hex downloads]][hex package]
 /-->
