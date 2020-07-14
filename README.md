# rebar3_lfe

[![Build Status][gh-actions-badge]][gh-actions] [![LFE Versions][lfe badge]][lfe] [![Erlang Versions][erlang badge]][versions] [![Tag][github tag badge]][github tag]

[![Project Logo][logo]][logo-large]

*A comprehensive LFE rebar3 plugin for all your LFE tooling needs*

#### Contents

* [Features](#build-)
* [Setup](#setup-)
* [Use](#use-)

## Features [&#x219F;](#contents)

* Create new LFE projects:
  * `rebar3 new lfe-lib`
  * `rebar3 new lfe-main`
* Start up an LFE REPL:
  * `rebar3 lfe repl`
* Compile LFE source code:
  * `rebar3 lfe compile`
* Run an LFE project's `main/1` function as an lfescript (run `rebar3 new lfe-main` to see an example):
  * `rebar3 lfe run`
  * `rebar3 lfe run -- 1 2 5`
  * `rebar3 lfe run -main some/path/main.lfe`

[More coming soon!](https://github.com/lfe-rebar3/rebar3_lfe/issues?q=is%3Aissue+is%3Aopen+label%3Afeature)

## Setup [&#x219F;](#contents)

Add the required plugins and provider hooks to your ``rebar.config``:

```erlang
{plugins, [
  {rebar3_lfe,
    {git, "https://github.com/lfe-rebar3/rebar2_lfe.git", {branch, "master"}}}
]}.
```

## Use [&#x219F;](#contents)

TBD

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
