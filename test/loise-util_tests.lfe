(defmodule loise-util_tests
  (export all)
  (import
    (from lfeunit
      (assert 1)
      (assert-not 1)
      (assert-equal 2)
      (assert-not-equal 2))
    (from loise-util
      (color-scale 2)
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

(defun color_scale_test ()
  (assert-equal 0 (color-scale 0 #(0.0 1.0)))
  (assert-equal 64 (color-scale -0.5 #(-1.0 1.0)))
  (assert-equal 128 (color-scale 0.5 #(0.0 1.0)))
  (assert-equal 255 (color-scale 1.0 #(0.0 1.0))))