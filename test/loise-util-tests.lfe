(defmodule loise-util-tests
  (behaviour ltest-unit)
  (export all)
  (import
    (from lutil-math
      (round 2))
    (from loise-util
      (dot 4)
      (index 2)
      (rem 2))))

(include-lib "ltest/include/ltest-macros.lfe")

(deftest get-seed
  (is-equal '#(1 0 0) (loise-util:get-seed 1))
  (is-equal '#(1 0 0) (loise-util:get-seed '(1)))
  (is-equal '#(1 2 0) (loise-util:get-seed '(1 2)))
  (is-equal '#(1 2 3) (loise-util:get-seed '(1 2 3))))

(deftest index
  (is-equal 42 (index '(99 4 7 42 13) 3))
  (is-equal '(-1.0 -1.0 0.0) (index (loise:gradient-matrix) 3)))

(deftest dot
  (is-equal 3.0 (dot (index (loise:gradient-matrix) 0) 1 2 3))
  (is-equal 1.0 (dot (index (loise:gradient-matrix) 1) 1 2 3))
  (is-equal -1.0 (dot (index (loise:gradient-matrix) 2) 1 2 3))
  (is-equal 4.0 (dot (index (loise:gradient-matrix) 4) 1 2 3)))
