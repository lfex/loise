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
      (dot 2)
      (dot 3))))

(defun dot-product_test ()
  (assert `'true))

(defun dot_test ()
  (assert `'true))

(defun mix_test ()
  (assert `'true))

(defun fade_test ()
  (assert `'true))

(defun perlin_test ()
  (assert `'true))

(defun simplex_test ()
  (assert `'true))