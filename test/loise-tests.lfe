(defmodule loise-tests
  (behaviour ltest-unit)
  (export all)
  (import
    (from lutil-math
      (round 2))
    (from lfeunit-util
      (check-failed-assert 2)
      (check-wrong-assert-exception 2))
    (from erlang
      (integer_to_list 1)
      (round 1))
    (from lists
      (foreach 2)
      (seq 2)
      (zipwith 3)
      (zipwith3 4))
    (from loise
      (fade 1)
      (get-gradient-index 3)
      (get-noise-contribution 4)
      (mix 3)
      (perlin 1) (perlin 2) (perlin 3)
      (simplex 1) (simplex 2) (simplex 3)
      (which-simplex 3))
    (from loise-util
      (dot 4)
      (rem 2)
      (element-index 2))))

(include-lib "ltest/include/ltest-macros.lfe")

(defun list->string (data)
  (string:join
    (lists:map #'integer_to_list/1 data)
    ""))

(deftest element-index
  (is-equal 42 (element-index #(99 4 7 42 13) 3)))

(deftest dot
  (is 'true))

(deftest mix
  (is-equal 4.0 (mix 1 2 3))
  (is-equal 90010.0 (mix 10 100 1000))
  (is-equal 11.0 (mix 1 2 10))
  (is-equal 31.0 (mix 1 2 30))
  (is-equal 71.0 (mix 1 2 70))
  (is-equal 1.23 (mix 1.1 1.2 1.3)))

(deftest fade
  (is-equal 0.103515625 (fade 0.25))
  (is-equal 0.5 (fade 0.5))
  (is-equal 0.896484375 (fade 0.75))
  (is-equal 0.0 (fade 0.0))
  (is-equal 1.0 (fade 1.0))
  (is-equal 3.375 (fade 1.5))
  (is-equal 32.0 (fade 2.0))
  (is-equal 156.25 (fade 2.5))
  (is-equal 10625.0 (fade 5.0)))

(deftest get-gradient-index
  (is-equal 0 (get-gradient-index 0 0 0))
  (is-equal 2 (get-gradient-index 0 0 1))
  (is-equal 0 (get-gradient-index 0 1 0))
  (is-equal 8 (get-gradient-index 0 1 1))
  (is-equal 7 (get-gradient-index 1 0 0))
  (is-equal 8 (get-gradient-index 1 0 1))
  (is-equal 2 (get-gradient-index 1 1 0))
  (is-equal 3 (get-gradient-index 1 1 1))
  (is-equal 8 (get-gradient-index 1 10 100))
  (is-equal 6 (get-gradient-index 100 10 1)))

(deftest get-noise-contribution
  (is-equal 0.0 (get-noise-contribution 0 0 0 0))
  (is-equal 0.0 (get-noise-contribution 1 0 0 0))
  (is-equal 0.0 (get-noise-contribution 1 0 0 1))
  (is-equal 0.0 (get-noise-contribution 1 1 1 1))
  (is-equal 0.0 (get-noise-contribution 2 1 1 1))
  (is-equal 2.0 (get-noise-contribution 4 1 1 1))
  (is-equal 20.0 (get-noise-contribution 4 5 10 15))
  (is-equal 101.0 (get-noise-contribution 4 1 10 100))
  (is-equal 2.0 (get-noise-contribution 8 1 1 1))
  (is-equal 25.0 (get-noise-contribution 8 5 10 15))
  (is-equal 110.0 (get-noise-contribution 8 1 10 100)))

(deftest perlin
  (is-equal -0.3772216257243449 (perlin 3.14 1.59 2.65))
  (let ((expected (list 0.0 0.11 0.23 0.37 0.46 0.5 0.46 0.37 0.23 0.11))
        (input (lists:map (lambda (x) (/ x 10)) (seq 0 9))))
    (zipwith
      (lambda (a b)
        (is-equal a (round (perlin b) 2)))
      expected
      input)))

(deftest which-simplex
  (is-equal "100110" (list->string (which-simplex 0 0 0)))
  (is-equal "001101" (list->string (which-simplex 0 0 1)))
  (is-equal "010110" (list->string (which-simplex 0 1 0)))
  (is-equal "010011" (list->string (which-simplex 0 1 1)))
  (is-equal "100101" (list->string (which-simplex 1 0 1)))
  (is-equal "001011" (list->string (which-simplex 1 2 3))))

(deftest simplex
  (is-equal 0.44 (round (simplex 0.1) 2))
  (is-equal 0.81 (round (simplex 0.1 0.1) 2))
  (is-equal -0.39 (round (simplex 0.9 0.9) 2))
  (is-equal 0.94 (round (simplex 0.1 0.2) 2))
  (is-equal -0.08 (round (simplex 0.1 0.2 0.9) 2)))
