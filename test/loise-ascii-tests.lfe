(defmodule loise-ascii-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")
(include-lib "include/options.lfe")

(defun tiny-opts ()
  (++ `(#(width 2)
        #(height 2)
      (loise-ascii:options)))

(deftest grades
  (let ((opts (loise-ascii:options)))
    (is-equal 6 (loise-opts:grades-count opts))
    (is-equal '(0 51.0 102.0 153.0 204.0 255.0)
              (loise-opts:grades opts))
    (is-equal '("A" "^" "n" "*" "~" "~")
              (loise-opts:ascii-map opts))
    (is-equal '(#(0 "A")
                #(51.0 "^")
                #(102.0 "n")
                #(153.0 "*")
                #(204.0 "~")
                #(255.0 "~"))
              (loise-opts:color-map opts))))

(deftest color-map
  (is-equal
    '(#(0 "A") #(51.0 "^") #(102.0 "n") #(153.0 "*") #(204.0 "~") #(255.0 "~"))
    (loise-opts:color-map (loise-ascii:options))))

(deftest get-perlin-point
  (is-equal 4.0 (loise-opts:multiplier (tiny-opts)))
  (is-equal '#((0 0) "*") (loise-ascii:perlin-point 0 0 (tiny-opts)))
  (is-equal '#((0 1) "*") (loise-ascii:perlin-point 0 1 (tiny-opts)))
  (is-equal '#((1 0) "*") (loise-ascii:perlin-point 1 0 (tiny-opts))))

(deftest get-simplex-point
  (is-equal '#((0 0) "*") (loise-ascii:simplex-point 0 0 (tiny-opts)))
  (is-equal '#((0 1) "~") (loise-ascii:simplex-point 0 1 (tiny-opts)))
  (is-equal '#((1 1) "A") (loise-ascii:simplex-point 1 1 (tiny-opts)))
  (is-equal '#((2 0) "n") (loise-ascii:simplex-point 2 0 (tiny-opts))))

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
    (loise-ascii:build-ascii #'loise-ascii:perlin-point/3 (tiny-opts)))
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
    (loise-ascii:build-ascii #'loise-ascii:simplex-point/3 (tiny-opts))))
