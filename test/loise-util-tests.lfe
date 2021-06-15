(defmodule loise-util-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")
(include-lib "include/options.lfe")

(defun get-grad (index)
  (loise-util:index (default-gradient-matrix) index))

(defun tiny-opts ()
  `(#(width 2)
    #(height 2)))

(deftest get-dimensions
  (is-equal '(256 128) (loise-util:get-dimensions (default-egd-options)))
  (is-equal '(56 36) (loise-util:get-dimensions (default-ascii-options)))
  (is-equal '(2 2) (loise-util:get-dimensions (tiny-opts))))

(deftest update-perm-table-options
  (let* ((opts (default-options))
         (new-opts (loise-lutil:update-perm-table-options opts))) 
    (is-equal "XXX"
              (proplists:get_value 'perm-table opts))
    (is-equal "XXX"
              (proplists:get_value 'perm-table new-opts))))

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
  (is-equal 4.0 (loise-util:mix 1 2 3 (default-options)))
  (is-equal 90010.0 (loise-util:mix 10 100 1000 (default-options)))
  (is-equal 11.0 (loise-util:mix 1 2 10 (default-options)))
  (is-equal 31.0 (loise-util:mix 1 2 30 (default-options)))
  (is-equal 71.0 (loise-util:mix 1 2 70 (default-options)))
  (is-equal 1.23 (loise-util:mix 1.1 1.2 1.3 (default-options))))

(deftest fade
  (is-equal 0.103515625 (loise-util:fade 0.25 (default-options)))
  (is-equal 0.5 (loise-util:fade 0.5 (default-options)))
  (is-equal 0.896484375 (loise-util:fade 0.75 (default-options)))
  (is-equal 0.0 (loise-util:fade 0.0 (default-options)))
  (is-equal 1.0 (loise-util:fade 1.0 (default-options)))
  (is-equal 3.375 (loise-util:fade 1.5 (default-options)))
  (is-equal 32.0 (loise-util:fade 2.0 (default-options)))
  (is-equal 156.25 (loise-util:fade 2.5 (default-options)))
  (is-equal 10625.0 (loise-util:fade 5.0 (default-options))))

(deftest make-gradations
  (is-equal #(error "there must be two or more gradations") (loise-util:make-gradations 0))
  (is-equal #(error "there must be two or more gradations") (loise-util:make-gradations 1))
  (is-equal '(0 255.0) (loise-util:make-gradations 2))
  (is-equal '(0 28 57 85 113 142 170 198 227 255) (lists:map #'round/1 (loise-util:make-gradations 10))))

(deftest get-gradient-index
  (is-equal 0 (loise-util:get-gradient-index 0 0 0 (default-options)))
  (is-equal 2 (loise-util:get-gradient-index 0 0 1 (default-options)))
  (is-equal 0 (loise-util:get-gradient-index 0 1 0 (default-options)))
  (is-equal 8 (loise-util:get-gradient-index 0 1 1 (default-options)))
  (is-equal 7 (loise-util:get-gradient-index 1 0 0 (default-options)))
  (is-equal 8 (loise-util:get-gradient-index 1 0 1 (default-options)))
  (is-equal 2 (loise-util:get-gradient-index 1 1 0 (default-options)))
  (is-equal 3 (loise-util:get-gradient-index 1 1 1 (default-options)))
  (is-equal 8 (loise-util:get-gradient-index 1 10 100 (default-options)))
  (is-equal 6 (loise-util:get-gradient-index 100 10 1 (default-options))))

(deftest get-noise-contribution
  (is-equal 0.0 (loise-util:get-noise-contribution 0 0 0 0 (default-options)))
  (is-equal 0.0 (loise-util:get-noise-contribution 1 0 0 0 (default-options)))
  (is-equal 0.0 (loise-util:get-noise-contribution 1 0 0 1 (default-options)))
  (is-equal 0.0 (loise-util:get-noise-contribution 1 1 1 1 (default-options)))
  (is-equal 0.0 (loise-util:get-noise-contribution 2 1 1 1 (default-options)))
  (is-equal 2.0 (loise-util:get-noise-contribution 4 1 1 1 (default-options)))
  (is-equal 20.0 (loise-util:get-noise-contribution 4 5 10 15 (default-options)))
  (is-equal 101.0 (loise-util:get-noise-contribution 4 1 10 100 (default-options)))
  (is-equal 2.0 (loise-util:get-noise-contribution 8 1 1 1 (default-options)))
  (is-equal 25.0 (loise-util:get-noise-contribution 8 5 10 15 (default-options)))
  (is-equal 110.0 (loise-util:get-noise-contribution 8 1 10 100 (default-options))))
