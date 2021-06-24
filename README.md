# rebar3_lfe

[![Build Status][gh-actions-badge]][gh-actions] [![LFE Versions][lfe badge]][lfe] [![Erlang Versions][erlang badge]][versions] [![Tag][github tag badge]][github tag]

*A comprehensive LFE rebar3 plugin for all your LFE tooling needs*

[![Project Logo][logo]][logo-large]

#### Contents

* [About](#about-)
* [Features](#features-)
* [Setup](#setup-)
* [Documentation](#documentation-)


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
* Run tests using the LFE testing library (wrapper for eunit)
  * `rebar3 lfe ltest`
  * `rebar3 lfe ltest -tunit`
  * `rebar3 lfe ltest -tsystem`
  * `rebar3 lfe ltest -tintegration`
  * `rebar3 lfe ltest -tall`
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
* Run an LFE/OTP release project's release script (`COMMAND` can be `start` , `stop` , `status`, `ping`, etc.):
  * `rebar3 lfe run-release COMMAND`
* Convert LFE data files to Erlang data file
  * `rebar3 lfe confabulate lfe-data.lfe erl_data.erl`
* Cleanup
  * `rebar3 lfe clean`
  * `rebar3 lfe clean-build`
  * `rebar3 lfe clean-cache`
  * `rebar3 lfe clean-all`
* Metadata
  * `rebar3 lfe versions`

[More coming soon!](https://github.com/lfe-rebar3/rebar3_lfe/issues?q=is%3Aissue+is%3Aopen+label%3Afeature)

## Setup [&#x219F;](#contents)

Add the plugin to your ``rebar.config`` (stable):

```erlang
{plugins, [
    {rebar3_lfe, "0.3.0"}
]}.
```

Or, if you want to use the current development branch (unstable):

```erlang
{plugins, [
    {rebar3_lfe,
        {git, "https://github.com/lfe-rebar3/rebar3_lfe.git", {branch, "release/0.4.x"}}}
]}.
```

## Documentation [&#x219F;](#contents)

Detailed usage is provided in the [project documentation](https://lfe-rebar3.github.io/).

[//]: ---Named-Links---

[logo]: https://avatars2.githubusercontent.com/u/15242004?s=250
[logo-large]: https://avatars2.githubusercontent.com/u/15242004
[github]: https://github.com/lfe-rebar3/rebar3_lfe
[gitlab]: https://gitlab.com/lfe-rebar3/rebar3_lfe
[gh-actions-badge]: https://github.com/lfe-rebar3/rebar3_lfe/workflows/ci%2Fcd/badge.svg
[gh-actions]: https://github.com/lfe-rebar3/rebar3_lfe/actions
[lfe]: https://github.com/rvirding/lfe
[lfe]: https://github.com/rvirding/lfe
[lfe badge]: https://img.shields.io/badge/lfe-2.0-blue.svg
[erlang badge]: https://img.shields.io/badge/erlang-19%E2%88%9224-blue.svg
[versions]: https://github.com/lfe-rebar3/rebar3_lfe/blob/master/.github/workflows/cicd.yml
[github tag]: https://github.com/lfe-rebar3/rebar3_lfe/tags
[github tag badge]: https://img.shields.io/github/tag/lfe-rebar3/rebar3_lfe.svg
[github downloads]: https://img.shields.io/github/downloads/atom/atom/total.svg
[hex badge]: https://img.shields.io/hexpm/v/rebar3_lfe.svg?maxAge=2592000
[hex package]: https://hex.pm/packages/rebar3_lfe
[hex downloads]: https://img.shields.io/hexpm/dt/rebar3_lfe.svg

<!-- Unused badges:
[![Downloads][hex downloads]][hex package]
 /-->
