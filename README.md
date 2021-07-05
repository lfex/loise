# Loise

[![Build Status][gh-actions-badge]][gh-actions]
[![LFE Versions][lfe badge]][lfe]
[![Erlang Versions][erlang badge]][versions]
[![Tags][github tags badge]][github tags]

*A noise library for LFE/Erlang*

[![Loise project logo][logo]][logo]


#### Contents

* [Introduction](#introduction-)
* [Background](#background-)
* [Dependencies](#dependencies-)
* [Usage](#usage-)
  * [Starting](#starting-)
  * [Perlin](#perlin-)
  * [Simplex](#simplex-)
  * [ASCII](#ascii-)
  * [Basics: From the REPL](#basics--from-the-repl-)
  * [Basics: In a Module](#basics--in-a-module-)
* [Traversing Noise Planes](#traversing-noise-planes-)
* [License](#license-)


## Introduction [&#x219F;](#contents)

This is a library, written in [LFE](http://lfe.io/), useful for generating
Perlin and Simplex noise. Perlin noise is a computer-generated visual effect
developed by Ken Perlin, who won an Academy Award for Technical Achievement for
inventing it. It can be used to simulate elements from nature, and is especially
useful in circumstances where computer memory is limited. (See the complete
[Perlin Wikipedia article](http://en.wikipedia.org/wiki/Perlin_noise).)

Simplex noise, on the other hand, is a method for constructing an n-dimensional
noise function comparable to Perlin noise ("classic" noise) but with a lower
computational overhead, especially in larger dimensions. Ken Perlin designed
the algorithm in 2001 to address the limitations of his classic noise
function, especially in higher dimensions. (See the complete
[Simplex Wikipedia article](http://en.wikipedia.org/wiki/Simplex_noise) for
more.)


## Background [&#x219F;](#contents)

The loise project stated life as a port of the
[Racket noise-generator](https://github.com/jpverkamp/noise) by
[jpverkamp](https://github.com/jpverkamp) to LFE. However, it has undergone
some seriosu refactoring since then, as well as the inclusion of many new
features.


## Dependencies [&#x219F;](#contents)

This project requires that you have Erlang installed (tested with R15B03, R16B03, 17.5, 18.0, and 18.3). It also assumes that you have [rebar3](https://github.com/erlang/rebar3)
installed somwhere in your ``$PATH``.


## Usage [&#x219F;](#contents)

The data generated with the ``perlin`` and ``simplex`` functions can be used to
create images. Erlang is not a good language for image generation, however this
library does provide some convenience functions for generating images.

Preliminary steps:

```bash
$ rebar3 compile
$ rebar3 lfe repl
```

### Starting [&#x219F;](#contents)

The loise library maintains state and as such must be run in order to use:

``` cl
lfe> (loise:start)
#(ok (loise))
```

Once this is done, the examples below will be ready to run.

### Perlin [&#x219F;](#contents)

Below are 5 perlin noise images generated at 1x, 2x, 4x, 8x, and 16x respectively.

<img src="priv/images/perlin-1.png" />

<img src="priv/images/perlin-2.png" />

<img src="priv/images/perlin-4.png" />

<img src="priv/images/perlin-8.png" />

<img src="priv/images/perlin-16.png" />

These were generated with the following from the REPL:

```cl
lfe> (set opts #m(noise perlin multiplier 1))
#M(multiplier 1 noise perlin)
lfe> (loise:image "perlin-1.png")
ok
lfe> (loise:image "perlin-2.png" (mupd opts 'multiplier 2))
ok
lfe> (loise:image "perlin-4.png" (mupd opts 'multiplier 4))
ok
lfe> (loise:image "perlin-8.png" (mupd opts 'multiplier 8))
ok
lfe> (loise:image "perlin-16.png" (mupd opts 'multiplier 16))
ok
```

You can also limit the number of gradations for the shades of grey, giving
the images a more "layered" or "topographical" look:

```cl
lfe> (set opts `#m(noise perlin
                   multiplier 4
                   graded? true
                   grades-count 8))
lfe> (loise:image "perlin-8-shades.png" opts)
ok
```

Which will create the following:

<img src="priv/images/perlin-8-shades.png" />

You may also change the permutation table from the default, to one generated
with a random seed:

```cl
lfe> (set opts (maps:merge opts #m(random? true
                                   graded? false
                                   seed 4)))
lfe> (loise:image "perlin-rand-1.png" opts)
ok
lfe> (loise:image "perlin-rand-2.png" (mupd opts 'seed '(4 2)))
ok
lfe> (loise:image "perlin-rand-3.png" (mupd opts 'seed (4 2 42)))
ok
```

### Simplex [&#x219F;](#contents)

Below are 5 simplex noise images generated at 1x, 2x, 4x, 8x, and 16x respectively.

<img src="priv/images/simplex-1.png" />

<img src="priv/images/simplex-2.png" />

<img src="priv/images/simplex-4.png" />

<img src="priv/images/simplex-8.png" />

<img src="priv/images/simplex-16.png" />

These were generated with the following from the REPL:

```cl
lfe> (set opts #m(noise simplex multiplier 1))
#M(multiplier 1 noise simplex)
lfe> (loise:image "simplex-1.png")
ok
lfe> (loise:image "simplex-2.png" (mupd opts 'multiplier 2))
ok
lfe> (loise:image "simplex-2.png" (mupd opts 'multiplier 2))
ok
lfe> (loise:image "simplex-8.png" (mupd opts 'multiplier 8))
ok
lfe> (loise:image "simplex-16.png" (mupd opts 'multiplier 16))
ok
```

Just as with perlin, simplex allows you to limit the number of gradations for
the shades of grey:

```cl
lfe> (set opts (mset opts 'graded? 'true 'grades-count 5 'multiplier 4))
lfe> (loise:image "simplex-5-shades.png" opts)
ok
```

Which will create the following:

<img src="priv/images/simplex-5-shades.png" />

You may also change the permutation table from the default, to one generated
with a random seed:

```cl
lfe> (set opts (mset opts 'random? 'true 'graded? 'false 'seed 4))
lfe> (loise:image "simplex-rand-1.png")
ok
lfe> (loise:image "simplex-rand-2.png" (mupd opts 'seed '(4 2)))
ok
lfe> (loise:image "simplex-rand-3.png" (mupd opts 'seed '(4 2 42)))
ok
```

You may either pass an integer or a list of 1, 2 or 3 integers as values
for the `seed` option key. Note that without different seeds, the same image
would be generated for multiple calls to `loise:image`.

### ASCII [&#x219F;](#contents)

You can also generate ASCII "images" with loise:

```cl
lfe> (loise:format-ascii #m(noise perlin color? true))
```
<img src="priv/images/perlin-ascii.png" />

And this:

```cl
lfe> (loise:format-ascii #m(noise simplex color? true))
```
<img src="priv/images/simplex-ascii.png" />

The ASCII annlog to the greyscale PNG noise images is simply a
gride without color:

``` cl
lfe> (loise:format-ascii #m(noise simplex color? false))
```

<img src="priv/images/simplex-ascii-no-color.png" />

You are not bound to the default ASCII represnetation nor default
color scheme. In the following example, we can generate a landscape
that includes alpine forests and grasslands and greatly increase the
map area in the terminal:

```cl
lfe> (set opts #m(color? true
                  width 282
                  height 94
                  multiplier 2.5
                  graded? true
                  grades-count 9
                  ascii-map ("A" "^" "!" "n" "*" "-" "~" "~" "~")
                  colors (whiteb yellow green green greenb green
                          blue blue blue)))

lfe> (loise:format-ascii opts)
```
<a href="https://raw.githubusercontent.com/lfex/loise/master/priv/images/simplex-ascii-2.png"><img src="priv/images/simplex-ascii-2-small.png" /></a>

As with the PNG images, ASCII output may be randomized by setting different seeds:


By default, loise uses a pre-generated "permutation table" to generate patterns.
You can view this table in `src/loise-defaults.lfe`. If you would like to
generate your own for more random results, you will need to enable the `random`
option and then generate a new table:

```cl
lfe> (set opts #m(random? true graded? true seed 4))
lfe> (loise:format-ascii opts)
lfe> (loise:format-ascii (mupd opts 'seed '(4 2)))
lfe> (loise:format-ascii (mupd opts 'seed '(4 2 42)))
```

### From the REPL [&#x219F;](#contents)

You can now use the loise noise functions by themseves, if you so desire
(without having to render output, etc.). Here is some example usage:

```cl
lfe> (loise:perlin 3.14 1.59 2.65)
-0.3772216257243449
lfe> (loise:simplex 0.1)
0.4410072765
lfe> (loise:simplex 0.1 0.2)
0.9410934374999996
lfe> (loise:simplex 0.1 0.2 0.9)
-0.07602014100000003
```

Note that, depending upon your random and seed settings, you may get different values
than those shown above.

Or, iterating over some values in one dimension:

```cl
lfe> (set input (list-comp ((<- x (lists:seq 0 9))) (/ x 10)))
(0.0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9)
lfe> (list-comp ((<- x input))
       (loise:round
        (loise:perlin x)
        2))
(0.0 0.11 0.23 0.37 0.46 0.5 0.46 0.37 0.23 0.11)
```


``` cl
lfe> (list-comp ((<- x input))
       (list-comp ((<- y input))
         (loise:round
          (loise:perlin x y)
          2)))
```

### In a Module [&#x219F;](#contents)

```cl
(defmodule mymodule
  (export
   (get-perlin-pie 0)
   (get-simplex-pie 0)))

(def get-perlin-pie ()
  (loise:perlin 3.14 1.59 2.65))

(def get-simplex-pie ()
  (loise:simplex 3.14 1.59 2.65))
```

## Traversing Noise Planes [&#x219F;](#contents)

### Brownian Motion / Random Walk

``` cl
lfe> (loise:start)
#(ok (loise))
lfe> (set opts `#m(scale-func ,#'lutil-math:midi-scale/2))
lfe> (loise:add-layer 'pitch opts)
ok
lfe> (loise:add-layer 'velocity opts)
ok
lfe> (set pitch-path (loise-traverse:brownian 'pitch #(0 0) opts))
lfe> (set velocity-path (loise-traverse:brownian 'velocity #(0 0) opts))
lfe> (lists:sublist pitch-path 1 10)
lfe> (lists:sublist velocity-path 1 10)
```

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
