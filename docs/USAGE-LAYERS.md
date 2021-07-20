# Usage

## Managing Layers

### Startup

``` cl
lfe> (loise:start)
#(ok (loise)
```

Might be good to create some options now, too:

``` cl
lfe> (set opts `#m(scale-func ,#'lutil-math:midi-scale/2))
```

### Create Layers


``` cl
lfe> (loise:add-layer 'pitch opts)
ok
lfe> (loise:add-layer 'velocity opts)
ok
```

### Brownian Motion / Random Walk

``` cl
lfe> (set pitch-path (loise-traverse:brownian 'pitch #(0 0) opts))
lfe> (set velocity-path (loise-traverse:brownian 'velocity #(0 0) opts))
lfe> (lists:sublist pitch-path 1 10)
lfe> (lists:sublist velocity-path 1 10)
```
