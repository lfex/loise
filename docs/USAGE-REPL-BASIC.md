# Usage

## Basic

Simple usage of the noise functions in losie is demonstrated below.

``` cl
lfe> (loise:start)
#(ok (loise)
```

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
