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

### Brownian Motion / Random Walk

Create finite paths (default length is 100 points):

``` cl
lfe> (set pitch-path (loise:get-path 'brownian 'pitch #(0 0)))
lfe> (set velocity-path (loise:get-path 'brownian 'velocity #(0 0)))
```

Walk those paths to get the value at each step's coorindates:

``` cl
lfe> (lists:sublist pitch-path 1 10)
lfe> (lists:sublist velocity-path 1 10)
```
