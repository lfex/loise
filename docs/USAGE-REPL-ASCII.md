# Usage

## ASCII

``` cl
lfe> (loise:start)
#(ok (loise)
```

You can also generate ASCII "images" with loise:

```cl
lfe> (loise:format-ascii #m(noise perlin color? true))
```
<img src="../priv/images/perlin-ascii.png" />

And this:

```cl
lfe> (loise:format-ascii #m(noise simplex color? true))
```
<img src="../priv/images/simplex-ascii.png" />

The ASCII annlog to the greyscale PNG noise images is simply a
gride without color:

``` cl
lfe> (loise:format-ascii #m(noise simplex color? false))
```

<img src="../priv/images/simplex-ascii-no-color.png" />

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
<a href="https://raw.githubusercontent.com/lfex/loise/master/priv/images/simplex-ascii-2.png"><img src="../priv/images/simplex-ascii-2-small.png" /></a>

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
