(defmodule loise-util_tests
  (export all)
  (import
    (from lfeunit
      (assert 1)
      (assert-not 1)
      (assert-equal 2)
      (assert-not-equal 2))
    (from loise
      (round 2))
    (from loise-util
      (color-scale 2)
      (get-perlin-for-point 3)
      (scale 3)
      (unit-scale 2))))

(defun scale_test ()
  (assert-equal 0.25 (scale -0.5 #(-1.0 1.0) #(0.0 1.0)))
  (assert-equal 0.0 (scale -0.5 #(-0.5 1.0) #(0.0 1.0)))
  (assert-equal 0.5 (scale -0.5 #(-1.5 0.5) #(0.0 1.0)))
  (assert-equal 1.0 (scale 1 #(-1.0 1.0) #(0.0 1.0)))
  (assert-equal 0.5 (scale 0 #(-1.0 1.0) #(0.0 1.0)))
  (assert-equal 0.0 (scale -1 #(-1.0 1.0) #(0.0 1.0)))
  (assert-equal 0.0 (scale 0 #(0.0 1.0) #(0.0 255.0)))
  (assert-equal 127.5 (scale 0.5 #(0.0 1.0) #(0.0 255.0)))
  (assert-equal 255.0 (scale 1.0 #(0.0 1.0) #(0.0 255.0))))

(defun unit-scale_test ()
  (assert-equal 0.25 (unit-scale -0.5 #(-1.0 1.0)))
  (assert-equal 0.0 (unit-scale -0.5 #(-0.5 1.0)))
  (assert-equal 0.5 (unit-scale -0.5 #(-1.5 0.5)))
  (assert-equal 1.0 (unit-scale 1 #(-1.0 1.0)))
  (assert-equal 0.5 (unit-scale 0 #(-1.0 1.0)))
  (assert-equal 0.0 (unit-scale -1 #(-1.0 1.0))))

(defun color-scale_test ()
  (assert-equal 0 (color-scale 0 #(0.0 1.0)))
  (assert-equal 64 (color-scale -0.5 #(-1.0 1.0)))
  (assert-equal 128 (color-scale 0.5 #(0.0 1.0)))
  (assert-equal 255 (color-scale 1.0 #(0.0 1.0))))

(defun get-perlin-for-point_test ()
  (assert-equal 0.0 (round (get-perlin-for-point #(0 0) #(256 256) 1) 2))
  (assert-equal 0.5 (round (get-perlin-for-point #(127) #(256) 1) 2))
  (assert-equal 0.56 (round (get-perlin-for-point #(127 64) #(256 256) 1) 2))
  (assert-equal 0.49 (round (get-perlin-for-point #(127 64 32) #(256 256 256) 1) 2)))