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
      (dot-product 2)
      (mix 3)
      (dot 2)
      (dot 3))))

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
  (assert `'true))

(defun perlin_test ()
  (assert `'true))

(defun simplex_test ()
  (assert `'true))