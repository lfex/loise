# Usage

## Managing Layers

### Startup

``` cl
lfe> (loise:start)
#(ok (loise))
```

Might be good to create some options now, too:

``` cl
lfe> (set opts `#m(scale-func ,#'lutil-math:midi-scale/2))
```

A scaling function is a 2-arity one that takes a given value and a tuple of the
valid range for any value of this type and then performs a transformation to a
different scale.

### Create Layers


``` cl
lfe> (loise:add-layer 'pitch opts)
ok
lfe> (loise:add-layer 'velocity opts)
ok
```

### Finite Traversals

#### Brownian Motion / Random Walk

You can perform a random walk on a noise plane, returning values with the
following:

``` cl
lfe> (set pitches (loise:traverse 'pitch 'brownian))
(64 84 100 108 100 84 122 108 116 107 100 107 124 116 121 92 73 63 78
 73 59 54 59 63 78 106 120 124 124 121 ...)

```

You can also perform this with an explicit path:

``` cl
lfe> (set velocity-path (loise:get-path 'velocity 'brownian #(0 0)))
lfe> (lists:sublist velocity-path 1 10)
(#(0 0) #(0 1) #(0 0) #(0 1) #(0 2) #(0 1) #(0 0) #(0 1) #(0 2) #(0 1))
lfe> (lists:sublist (loise:traverse 'velocity velocity-path) 1 10)
(64 94 64 94 108 94 64 94 108 94)

```

### Infinite Traversals

TBD
