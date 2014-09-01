(defmodule loise-util-tests
  (behaviour ltest-unit)
  (export all)
  (import
    (from lutil-math
      (round 2))
    (from loise-util
      (dot 4)
      (get-perlin-for-point 3)
      (index 2)
      (rem 2))))

(include-lib "ltest/include/ltest-macros.lfe")

(deftest index
  (is-equal 42 (index '(99 4 7 42 13) 3))
  (is-equal '(-1.0 -1.0 0.0) (index (loise:grad3) 3)))

(deftest dot
  (is-equal 3.0 (dot (index (loise:grad3) 0) 1 2 3))
  (is-equal 1.0 (dot (index (loise:grad3) 1) 1 2 3))
  (is-equal -1.0 (dot (index (loise:grad3) 2) 1 2 3))
  (is-equal 4.0 (dot (index (loise:grad3) 4) 1 2 3)))

(deftest get-perlin-for-point
  (is-equal 0.0 (round (get-perlin-for-point #(0 0) #(256 256) 1) 2))
  (is-equal 0.5 (round (get-perlin-for-point #(127) #(256) 1) 2))
  (is-equal 0.56 (round (get-perlin-for-point #(127 64) #(256 256) 1) 2))
  (is-equal 0.49 (round (get-perlin-for-point #(127 64 32) #(256 256 256) 1) 2)))