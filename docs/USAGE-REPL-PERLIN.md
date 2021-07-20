# Usage

## Perlin

``` cl
lfe> (loise:start)
#(ok (loise)
```

Below are 5 perlin noise images generated at 1x, 2x, 4x, 8x, and 16x respectively.

<img src="../priv/images/perlin-1.png" />

<img src="../priv/images/perlin-2.png" />

<img src="../priv/images/perlin-4.png" />

<img src="../priv/images/perlin-8.png" />

<img src="../priv/images/perlin-16.png" />

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

<img src="../priv/images/perlin-8-shades.png" />

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
