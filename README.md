# rebar3_lfe

[![Project Logo][logo]][logo-large]

*A comprehensive LFE rebar3 plugin for all your LFE tooling needs*

#### Contents

* [Features](#build-)
* [Use](#use-)

## Features [&#x219F;](#contents)

Completed:

* Create new LFE projects:
  * `rebar3 new lfe-lib`
* Start up an LFE REPL: `rebar3 lfe repl`
* Compile LFE source code: `rebar3 lfe compile`

In progress:

* Create new LFE projects:
  * `rebar3 new lfe-main`
* Run an LFE project's `(main)` function as a script `rebar3 lfe run`

More coming soon!

## Use [&#x219F;](#contents)

Add the required plugins and provider hooks to your ``rebar.config``:

```erlang
{plugins, [
  {rebar3_lfe,
    {git, "https://github.com/lfe-rebar3/rebar2_lfe.git", {branch, "master"}}}
]}.
```

<!-- Named page links below: /-->

[logo]: https://avatars2.githubusercontent.com/u/15242004?s=250
[logo-large]: https://avatars2.githubusercontent.com/u/15242004
