(defmodule loise-png-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")

(deftest get-point-color-without-grades
  (is-equal 128 (loise-png:get-graded-point 0 '())))

(deftest get-point-color-with-grades
  (is-equal
    170.0
    (loise-png:get-graded-point
      0.5
      `(#(grades ,(loise-util:make-gradations 4))))))

(defmodule loise-png-system-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")

(deftest placeholder
  (is 'true))