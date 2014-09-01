(defmodule loise-util-tests
  (behaviour ltest-unit)
  (export all)
  (import
    (from lutil-math
      (round 2))
    (from loise-util
      (get-perlin-for-point 3))))

(include-lib "ltest/include/ltest-macros.lfe")

(deftest get-perlin-for-point
  (is-equal 0.0 (round (get-perlin-for-point #(0 0) #(256 256) 1) 2))
  (is-equal 0.5 (round (get-perlin-for-point #(127) #(256) 1) 2))
  (is-equal 0.56 (round (get-perlin-for-point #(127 64) #(256 256) 1) 2))
  (is-equal 0.49 (round (get-perlin-for-point #(127 64 32) #(256 256 256) 1) 2)))