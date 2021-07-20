# Usage

## In a Module

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
