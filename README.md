# Loise [![Build Status][travis-badge]][travis]

**A Noise-Generator for LFE**

[![Loise project logo][logo]][logo]

#### Table of Contents

* [Introduction](#introduction-)
* [Background](#background-)
* [Dependencies](#dependencies-)
* [Eye Candy](#eye-candy-)
  * [Perlin](#perlin-)
  * [Simplex](#simplex-)
  * [ASCII](#ascii-)
* [Usage](#usge-)
  * [From the REPL](#from-the-repl-)
  * [In a Module](#in-a-module-)


## Introduction [&#x219F;](#table-of-contents)

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


## Background [&#x219F;](#table-of-contents)

The loise project stated life as a port of the
[Racket noise-generator](https://github.com/jpverkamp/noise) by
[jpverkamp](https://github.com/jpverkamp) to LFE. However, it has undergone
some seriosu refactoring since then, as well as the inclusion of many new
features.


## Dependencies [&#x219F;](#table-of-contents)

This project requires that you have Erlang installed (tested with R15B03, R16B03, 17.5, 18.0, and 18.3). It also assumes that you have [rebar3](https://github.com/erlang/rebar3)
installed somwhere in your ``$PATH``.


## Eye Candy [&#x219F;](#table-of-contents)

The data generated with the ``perlin`` and ``simplex`` functions can be used to
create images. Erlang is not a good language for image generation, however this
library does provide some convenience functions for generating images.


### Perlin [&#x219F;](#table-of-contents)

Below are 4 perlin noise images generated at 1x, 2x, 4x, and 8x, respectively.

<img src="priv/images/perlin-1.png" />

<img src="priv/images/perlin-2.png" />

<img src="priv/images/perlin-4.png" />

<img src="priv/images/perlin-8.png" />

These were generated with the following from the REPL:

```cl
  > (loise-egd:create-perlin "perlin-1.png" `(#(multiplier 1)))
  ok
  > (loise-egd:create-perlin "perlin-2.png" `(#(multiplier 2)))
  ok
  > (loise-egd:create-perlin "perlin-4.png" `(#(multiplier 4)))
  ok
  > (loise-egd:create-perlin "perlin-8.png" `(#(multiplier 8)))
  ok
```

You can also limit the number of gradations for the shades of grey, giving
the images a more "layered" or "topographical" look:

```cl
> (set grades (loise-util:get-gradations 7))
(0 42.5 85.0 127.5 170.0 212.5 255.0)
> (loise-egd:create-perlin
    "perlin-7-shades.png" 'png 256 128 8 grades)
ok
```

Which will create the following:

<img src="priv/images/perlin-7-shades.png" />


### Simplex [&#x219F;](#table-of-contents)

Below are 4 simplex noise images generated at 1x, 2x, 4x, and 8x, respectively.

<img src="priv/images/simplex-1.png" />

<img src="priv/images/simplex-2.png" />

<img src="priv/images/simplex-4.png" />

<img src="priv/images/simplex-8.png" />

These were generated with the following from the REPL:

```cl
  > (loise-egd:create-simplex "simplex-1.png" `(#(multiplier 1)))
  ok
  > (loise-egd:create-simplex "simplex-2.png" `(#(multiplier 2)))
  ok
  > (loise-egd:create-simplex "simplex-4.png" `(#(multiplier 4)))
  ok
  > (loise-egd:create-simplex "simplex-8.png" `(#(multiplier 8)))
  ok
```

Just as with perlin, simplex allows you to limit the number of gradations for
the shades of grey:

```cl
  > (set grades (loise-util:get-gradations 5))
  (0 63.75 127.5 191.25 255.0)
  > (set opts `(#(multiplier 8)
                #(grades ,grades)))
  > (loise-egd:create-simplex "simplex-5-shades.png" opts)
  ok
```

Which will create the following:

<img src="priv/images/simplex-5-shades.png" />

You may also change the permutation table from the default, to one generated
with a random seed:

```cl
> (set opts (++ `(#(random true))))
> (loise-egd:create-perlin
    "perlin-rand-1.png" (++ `(#(seed (1))) opts))
> (loise-egd:create-simplex
    "simplex-rand-1.png" (++ `(#(seed (1 2))) opts))
> (loise-egd:create-simplex
    "simplex-rand-2.png" (++ `(#(seed (1 2 3))) opts))
```

You may either pass an integer or a list of 1, 2 or 3 integers as values
for the ``seed`` option key.

To see the full list of options available be sure to look at both
``loise-const:base-options/0`` and ``loise-egd:default-options``.


### ASCII [&#x219F;](#table-of-contents)

You can also generate ASCII "images" with loise. As an example of this, we can
map the default values represented by this range:

```cl
  > (loise-util:get-gradations 6)
  (0 51.0 102.0 153.0 204.0 255.0)
```

And by this set of ASCII characters:

* Level 6 - ``A``
* Level 5 - ``^``
* Level 4 - ``n``
* Level 3 - ``*``
* Level 2 - ``~``
* Level 1 - ``~``

By making calls like this:

```cl
> (loise-ascii:create-perlin)
```
<img src="priv/images/perlin-ascii.png" />

And this:

```cl
> (loise-ascii:create-simplex)
```
<img src="priv/images/simplex-ascii.png" />

We can, of course, pass new options to the function. The following shows the
addition of alpine forests and grasslands and greatly increasing the
map area in the terminal:

```cl
> (set opts
    `(#(width 100)
      #(height 42)
      #(multiplier 3.0)
      #(grades ,(loise-util:get-gradations 9))
      #(ascii-map ("A" "^" "!" "n" "*" "-" "~" "~" "~"))
      #(colors (,#'color:whiteb/1 ,#'color:yellow/1 ,#'color:green/1
                ,#'color:green/1 ,#'color:greenb/1 ,#'color:green/1
                ,#'color:blue/1 ,#'color:blue/1 ,#'color:blue/1))))

> (loise-ascii:create-simplex opts)
```
<a href="https://raw.githubusercontent.com/lfex/loise/master/priv/images/simplex-ascii-2.png"><img src="priv/images/simplex-ascii-2-small.png" /></a>

By default, loise uses a "permutation table" to generate patterns. You can view
this table in the source ``loise-const`` module. If you would like to generate
random results, you will need to enable the ``random`` option:

```cl
> (set opts (++ `(#(random true)) opts))
```

If you do not
provide a seed, the same "random" result will be given for every call. If you
would like a different result each time, you will need to pass a new seed.
For instance:

```cl
> (loise-ascii:create-perlin (++ `(#(seed 1)) opts))
> (loise-ascii:create-simplex (++ `(#(seed (1 2))) opts))
> (loise-ascii:create-simplex (++ `(#(seed (1 2 3))) opts))
```

To see the full list of options available be sure to look at both
``loise-const:base-options/0`` and ``loise-ascii:default-options``.


## Usage [&#x219F;](#table-of-contents)

The first place to start is ensuring that the code you obtained works as
expected. To find out, run the unit tests:

```bash
  $ cd loise
  $ make check
```


### From the REPL [&#x219F;](#table-of-contents)

Once everything is working, start up an LFE REPL:

```bash
  $ make repl
```

You can now use loise by itself, if you so desire. Here is some example usage:

```cl
  > (loise:perlin 3.14 1.59 2.65)
  -0.3772216257243449
  > (loise:simplex 0.1)
  0.4410072765
  > (loise:simplex 0.1 0.2)
  0.9410934374999996
  > (loise:simplex 0.1 0.2 0.9)
  -0.07602014100000003
```

Or, iterating over some values:

```cl
  > (set input
      (lists:map
        (lambda (x)
          (/ x 10))
        (lists:seq 0 9))))
  (0.0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9)
  > (lists:map
      (lambda (x)
        (loise:round
          (loise:perlin x)
        2))
      input)
  (0.0 0.11 0.23 0.37 0.46 0.5 0.46 0.37 0.23 0.11)
```


### In a Module [&#x219F;](#table-of-contents)

```cl
  (defmodule mymodule
    (export all)
    (import
      (from loise
        (perlin 3)
        (simplex 3))))

  (def get-perlin-pie ()
    (perlin 3.14 1.59 2.65))

  (def get-simplex-pie ()
    (simplex 3.14 1.59 2.65))
```

<!-- Named page links below: /-->

[travis]: https://travis-ci.org/lfex/loise
[travis-badge]: https://travis-ci.org/lfex/loise.png?branch=master
[logo]: priv/images/loise.jpg
