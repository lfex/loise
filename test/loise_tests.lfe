(defmodule loise_tests
  (export all)
  (import
    (from lfeunit-util
      (check-failed-assert 2)
      (check-wrong-assert-exception 2))
    (from lfeunit
      (assert 1)
      (assert-not 1)
      (assert-equal 2)
      (assert-not-equal 2)
      (assert-exception 3)
      (assert-error 2)
      (assert-throw 2)
      (assert-exit 2))
    (from loise
      (add-tuples 1)
      (fast-floor 1)
      (vector-ref 2)
      (dot-product 2)
      (mix 3)
      (fade 1)
      (dot 2)
      (dot 3))))

(defun add-tuples_test ()
  (let ((data (list (tuple 1 2 3) (tuple 2 3 4))))
    (assert-equal #(1 2 3 2 3 4) (add-tuples data))))

(defun fast-floor_test ()
  (assert-equal 0 (fast-floor 0.0))
  (assert-equal 1 (fast-floor 1.0))
  (assert-equal -5 (fast-floor -4.3))
  (assert-equal 3 (fast-floor 3.1))
  (assert-equal 3 (fast-floor 3.4))
  (assert-equal 3 (fast-floor 3.5))
  (assert-equal 3 (fast-floor 3.9)))

(defun vector-ref_test ()
  (assert-equal 42 (vector-ref #(99 4 7 42 13) 3)))

(defun dot-product_test ()
  (assert-equal 32 (dot-product '(1 2 3) '(4 5 6)))
  (assert-equal 122 (dot-product '(9 2 7) '(4 8 10))))

(defun dot_test ()
  (assert `'true))

(defun mix_test ()
  (assert-equal 4.0 (mix 1 2 3))
  (assert-equal 90010.0 (mix 10 100 1000))
  (assert-equal 11.0 (mix 1 2 10))
  (assert-equal 31.0 (mix 1 2 30))
  (assert-equal 71.0 (mix 1 2 70))
  (assert-equal 1.23 (mix 1.1 1.2 1.3)))

(defun fade_test ()
  (assert-equal 0.103515625 (fade 0.25))
  (assert-equal 0.5 (fade 0.5))
  (assert-equal 0.896484375 (fade 0.75))
  (assert-equal 0.0 (fade 0.0))
  (assert-equal 1.0 (fade 1.0))
  (assert-equal 3.375 (fade 1.5))
  (assert-equal 32.0 (fade 2.0))
  (assert-equal 156.25 (fade 2.5))
  (assert-equal 10625.0 (fade 5.0)))

(defun perlin_test ()
  (assert `'true))

(defun simplex_test ()
  (assert `'true))