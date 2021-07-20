# Loise

[![Build Status][gh-actions-badge]][gh-actions]
[![LFE Versions][lfe badge]][lfe]
[![Erlang Versions][erlang badge]][versions]
[![Tags][github tags badge]][github tags]

*A noise library for LFE/Erlang*

[![Loise project logo][logo]][logo]


#### Contents

* [Introduction](#introduction-)
* [Dependencies](#dependencies-)
* [Building and Starting](#building-and-starting-)
* [Usage Example](#usage-example)
* [Documentation](#documentation-)
* [License](#license-)


## Introduction [&#x219F;](#contents)

This is a library, written in [LFE](http://lfe.io/), useful for generating
Perlin and Simplex noise. There's more info in the docs on
[the background of both](docs/BACKGROUND.md). This LFE noise library may be
used as simply that (a library) but it also supports more: stateful management
of multiple "matrices" (renderable as images) and the navigation / examination
of that generated noise data.

## Dependencies [&#x219F;](#contents)

This project requires that you have Erlang installed (tested with R15B03, R16B03, 17.5, 18.0, and 18.3). It also assumes that you have [rebar3](https://github.com/erlang/rebar3)
installed somwhere in your ``$PATH``.

## Building and Starting [&#x219F;](#contents)

Preliminary steps:

```bash
$ rebar3 compile
$ rebar3 lfe repl
```

The loise library maintains state and as such must be run in order to use, even
when simply used as a library. From the REPL:

``` cl
lfe> (loise:start)
#(ok (loise))
```

For non-REPL use, in your project, simply add `loise` to the `applications` 
list:

``` erlang
{applications, [
    kernel,
    stdlib,
    loise
]},
```

## Usage [&#x219F;](#contents)

The following usage example is just the merest fraction of what you can do with
loise. Be sure to see the [Documentation](#documentation-) section below for
links to usage examples for specific features.

Simplex smooth:

``` cl
lfe> (loise:image "simplex-4.png" (mupd opts 'multiplier 4))
ok
```
<img src="priv/images/simplex-4.png" />

Simplex graded:

```cl
lfe> (set opts (mset opts 'graded? 'true 'grades-count 5 'multiplier 4))
lfe> (loise:image "simplex-5-shades.png" opts)
ok
```
<img src="priv/images/simplex-5-shades.png" />

Simplex as coloured ASCII:

```cl
lfe> (loise:format-ascii #m(noise simplex color? true))
```
<img src="priv/images/simplex-ascii.png" />


## Documentation [&#x219F;](#contents)

* [Background Information](docs/BACKGROUND.md)
* Usage
  * [In LFE Modules](docs/USAGE-MODULE.md)
  * In the REPL:
    * [Basic Usage](docs/USAGE-REPL-BASIC.md)
    * [Perlin Noise](docs/USAGE-REPL-PERLIN.md)
    * [Simplex Noise](docs/USAGE-REPL-SIMPLEX.md)
    * [Layer Management](docs/USAGE-REPL-ASCII.md)
      * [Creating Layers](docs/USAGE-LAYERS.md#create-layers)
      * [Random Walks](docs/USAGE-REPL-ASCII.md#)

## License [&#x219F;](#contents)

```
Copyright © 2013-2021 Duncan McGreggor

Distributed under the Apache License, Version 2.0.
```

[//]: ---Named-Links---

[org]: https://github.com/lfex
[github]: https://github.com/lfex/loise
[gitlab]: https://gitlab.com/lfex/loise
[gh-actions-badge]: https://github.com/lfex/loise/workflows/ci%2Fcd/badge.svg
[gh-actions]: https://github.com/lfex/loise/actions
[logo]: priv/images/loise.jpg
[lfe]: https://github.com/lfe/lfe
[lfe badge]: https://img.shields.io/badge/lfe-2.0-blue.svg
[erlang badge]: https://img.shields.io/badge/erlang-21%20to%2024-blue.svg
[versions]: https://github.com/lfex/loise/blob/master/.github/workflows/cicd.yml
[github tags]: https://github.com/lfex/loise/tags
[github tags badge]: https://img.shields.io/github/tag/lfex/loise.svg
[github downloads]: https://img.shields.io/github/downloads/lfex/loise/total.svg
[hex badge]: https://img.shields.io/hexpm/v/loise.svg?maxAge=2592000
[hex package]: https://hex.pm/packages/loise
[hex downloads]: https://img.shields.io/hexpm/dt/loise.svg
