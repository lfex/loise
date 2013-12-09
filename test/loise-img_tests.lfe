(defmodule loise-img_tests
  (export all)
  (import
    (from lfe-utils
      (round 2))
    (from lfeunit
      (assert 1)
      (assert-not 1)
      (assert-equal 2)
      (assert-not-equal 2))
    (from loise-img
      (span 1))))

(defun span_test ()
  (let ((points (list (tuple 1 2) (tuple 1 4) (tuple 4 5) (tuple 20 -10))))
    (assert-equal (tuple 1 -10 20 5) (span points))))