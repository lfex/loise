(defmodule loise-ascii-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")

(defun def-opts ()
  (loise-ascii:default-options))

(defun tiny-opts ()
  (++ `(#(width 2) #(height 2))
      (def-opts)))

(deftest get-perlin-point
  (is-equal '#((0 0) "*") (loise-ascii:get-perlin-point 0 0 (tiny-opts)))
  (is-equal '#((0 1) "*") (loise-ascii:get-perlin-point 0 1 (tiny-opts)))
  (is-equal '#((1 0) "*") (loise-ascii:get-perlin-point 1 0 (tiny-opts))))

(deftest get-simplex-point
  (is-equal '#((0 0) "*") (loise-ascii:get-simplex-point 0 0 (tiny-opts)))
  (is-equal '#((0 1) "~") (loise-ascii:get-simplex-point 0 1 (tiny-opts)))
  (is-equal '#((1 1) "A") (loise-ascii:get-simplex-point 1 1 (tiny-opts)))
  (is-equal '#((2 0) "n") (loise-ascii:get-simplex-point 2 0 (tiny-opts))))

(deftest build-ascii
  (is-equal
    '(#((0 0) "*")
      #((0 1) "*")
      #((0 2) "*")
      #((1 0) "*")
      #((1 1) "*")
      #((1 2) "*")
      #((2 0) "*")
      #((2 1) "*")
      #((2 2) "*"))
    (loise-ascii:build-ascii #'loise-ascii:get-perlin-point/3 (tiny-opts)))
  (is-equal
    '(#((0 0) "*")
      #((0 1) "~")
      #((0 2) "*")
      #((1 0) "*")
      #((1 1) "A")
      #((1 2) "*")
      #((2 0) "n")
      #((2 1) "*")
      #((2 2) "~"))
    (loise-ascii:build-ascii #'loise-ascii:get-simplex-point/3 (tiny-opts))))
