(defmodule loise-util-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")

(defun get-grad (index)
  (loise-util:index (loise:gradient-matrix) index))

(deftest get-seed
  (is-equal '#(1 0 0) (loise-util:get-seed 1))
  (is-equal '#(1 0 0) (loise-util:get-seed '(1)))
  (is-equal '#(1 2 0) (loise-util:get-seed '(1 2)))
  (is-equal '#(1 2 3) (loise-util:get-seed '(1 2 3))))

(deftest index
  (is-equal 42 (loise-util:index '(99 4 7 42 13) 3))
  (is-equal '(-1.0 -1.0 0.0) (get-grad 3)))

(deftest dot
  (is-equal 3.0 (loise-util:dot (get-grad 0) 1 2 3))
  (is-equal 1.0 (loise-util:dot (get-grad 1) 1 2 3))
  (is-equal -1.0 (loise-util:dot (get-grad 2) 1 2 3))
  (is-equal 4.0 (loise-util:dot (get-grad 4) 1 2 3)))
