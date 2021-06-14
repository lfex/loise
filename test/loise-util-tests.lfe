(defmodule loise-util-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")

(defun opts ()
  (loise-const:base-options))

(defun perm ()
  (loise-const:perm-default))

(defun get-grad (index)
  (loise-util:index (loise-const:gradient-matrix) index))

(deftest get-seed
  (is-equal '#(1 0 0) (loise-util:get-seed 1))
  (is-equal '#(1 0 0) (loise-util:get-seed '(1)))
  (is-equal '#(1 2 0) (loise-util:get-seed '(1 2)))
  (is-equal '#(1 2 3) (loise-util:get-seed '(1 2 3))))

(defun tiny-opts ()
  `(#(width 2) #(height 2)))

(deftest get-ascii-map
  (is-equal
    '(#(0 "A") #(51.0 "^") #(102.0 "n") #(153.0 "*") #(204.0 "~") #(255.0 "~"))
    (loise-util:get-ascii-map (loise-ascii:default-options))))

(deftest get-dimensions
  (is-equal '(56 36) (loise-util:get-dimensions (loise-ascii:default-options)))
  (is-equal '(2 2) (loise-util:get-dimensions (tiny-opts))))

(deftest index
  (is-equal 42 (loise-util:index '(99 4 7 42 13) 3))
  (is-equal '(-1.0 -1.0 0.0) (get-grad 3)))

(deftest dot
  (is-equal 3.0 (loise-util:dot (get-grad 0) 1 2 3))
  (is-equal 1.0 (loise-util:dot (get-grad 1) 1 2 3))
  (is-equal -1.0 (loise-util:dot (get-grad 2) 1 2 3))
  (is-equal 4.0 (loise-util:dot (get-grad 4) 1 2 3)))

(defun do-things (m x y w h)
  (list m x y w h))

(deftest partial
  (let ((func-1 (loise-util:partial #'do-things/5 1))
        (func-2 (loise-util:partial #'do-things/5 '(1 1))))
    (is-equal '(1 1 2 6 24) (funcall func-1 '(1 2 6 24)))
    (is-equal '(1 1 2 6 24) (funcall func-2 '(2 6 24)))))

(deftest mix
  (is-equal 4.0 (loise-util:mix 1 2 3 (opts)))
  (is-equal 90010.0 (loise-util:mix 10 100 1000 (opts)))
  (is-equal 11.0 (loise-util:mix 1 2 10 (opts)))
  (is-equal 31.0 (loise-util:mix 1 2 30 (opts)))
  (is-equal 71.0 (loise-util:mix 1 2 70 (opts)))
  (is-equal 1.23 (loise-util:mix 1.1 1.2 1.3 (opts))))

(deftest fade
  (is-equal 0.103515625 (loise-util:fade 0.25 (opts)))
  (is-equal 0.5 (loise-util:fade 0.5 (opts)))
  (is-equal 0.896484375 (loise-util:fade 0.75 (opts)))
  (is-equal 0.0 (loise-util:fade 0.0 (opts)))
  (is-equal 1.0 (loise-util:fade 1.0 (opts)))
  (is-equal 3.375 (loise-util:fade 1.5 (opts)))
  (is-equal 32.0 (loise-util:fade 2.0 (opts)))
  (is-equal 156.25 (loise-util:fade 2.5 (opts)))
  (is-equal 10625.0 (loise-util:fade 5.0 (opts))))

(deftest get-gradient-index
  (is-equal 0 (loise-util:get-gradient-index 0 0 0 (opts)))
  (is-equal 2 (loise-util:get-gradient-index 0 0 1 (opts)))
  (is-equal 0 (loise-util:get-gradient-index 0 1 0 (opts)))
  (is-equal 8 (loise-util:get-gradient-index 0 1 1 (opts)))
  (is-equal 7 (loise-util:get-gradient-index 1 0 0 (opts)))
  (is-equal 8 (loise-util:get-gradient-index 1 0 1 (opts)))
  (is-equal 2 (loise-util:get-gradient-index 1 1 0 (opts)))
  (is-equal 3 (loise-util:get-gradient-index 1 1 1 (opts)))
  (is-equal 8 (loise-util:get-gradient-index 1 10 100 (opts)))
  (is-equal 6 (loise-util:get-gradient-index 100 10 1 (opts))))

(deftest get-noise-contribution
  (is-equal 0.0 (loise-util:get-noise-contribution 0 0 0 0 (opts)))
  (is-equal 0.0 (loise-util:get-noise-contribution 1 0 0 0 (opts)))
  (is-equal 0.0 (loise-util:get-noise-contribution 1 0 0 1 (opts)))
  (is-equal 0.0 (loise-util:get-noise-contribution 1 1 1 1 (opts)))
  (is-equal 0.0 (loise-util:get-noise-contribution 2 1 1 1 (opts)))
  (is-equal 2.0 (loise-util:get-noise-contribution 4 1 1 1 (opts)))
  (is-equal 20.0 (loise-util:get-noise-contribution 4 5 10 15 (opts)))
  (is-equal 101.0 (loise-util:get-noise-contribution 4 1 10 100 (opts)))
  (is-equal 2.0 (loise-util:get-noise-contribution 8 1 1 1 (opts)))
  (is-equal 25.0 (loise-util:get-noise-contribution 8 5 10 15 (opts)))
  (is-equal 110.0 (loise-util:get-noise-contribution 8 1 10 100 (opts))))
