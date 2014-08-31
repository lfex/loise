(defmodule loise-util_tests
  (export all)
  (import
    (from lutil
      (round 2))
    (from lfeunit
      (assert 1)
      (assert-not 1)
      (assert-equal 2)
      (assert-not-equal 2))
    (from loise-util
      (get-perlin-for-point 3))))

(defun get-perlin-for-point_test ()
  (assert-equal 0.0 (round (get-perlin-for-point #(0 0) #(256 256) 1) 2))
  (assert-equal 0.5 (round (get-perlin-for-point #(127) #(256) 1) 2))
  (assert-equal 0.56 (round (get-perlin-for-point #(127 64) #(256 256) 1) 2))
  (assert-equal 0.49 (round (get-perlin-for-point #(127 64 32) #(256 256 256) 1) 2)))