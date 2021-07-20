# Usage

## Simplex

``` cl
lfe> (loise:start)
#(ok (loise)
```

Below are 5 simplex noise images generated at 1x, 2x, 4x, 8x, and 16x respectively.

<img src="../priv/images/simplex-1.png" />

<img src="../priv/images/simplex-2.png" />

<img src="../priv/images/simplex-4.png" />

<img src="../priv/images/simplex-8.png" />

<img src="../priv/images/simplex-16.png" />

These were generated with the following from the REPL:

```cl
lfe> (set opts #m(noise simplex multiplier 1))
#M(multiplier 1 noise simplex)
lfe> (loise:image "simplex-1.png")
ok
lfe> (loise:image "simplex-2.png" (mupd opts 'multiplier 2))
ok
lfe> (loise:image "simplex-4.png" (mupd opts 'multiplier 4))
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

<img src="../priv/images/simplex-5-shades.png" />

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
