(defmodule loise-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")

(defun list->string (data)
  (string:join
    (lists:map #'integer_to_list/1 data)
    ""))

(defun opts ()
  (loise-const:base-options))

(defun perm ()
  (loise-const:perm-default))

(deftest mix
  (is-equal 4.0 (loise:mix 1 2 3))
  (is-equal 90010.0 (loise:mix 10 100 1000))
  (is-equal 11.0 (loise:mix 1 2 10))
  (is-equal 31.0 (loise:mix 1 2 30))
  (is-equal 71.0 (loise:mix 1 2 70))
  (is-equal 1.23 (loise:mix 1.1 1.2 1.3)))

(deftest fade
  (is-equal 0.103515625 (loise:fade 0.25))
  (is-equal 0.5 (loise:fade 0.5))
  (is-equal 0.896484375 (loise:fade 0.75))
  (is-equal 0.0 (loise:fade 0.0))
  (is-equal 1.0 (loise:fade 1.0))
  (is-equal 3.375 (loise:fade 1.5))
  (is-equal 32.0 (loise:fade 2.0))
  (is-equal 156.25 (loise:fade 2.5))
  (is-equal 10625.0 (loise:fade 5.0)))

(deftest get-gradient-index
  (is-equal 0 (loise:get-gradient-index 0 0 0 (perm)))
  (is-equal 2 (loise:get-gradient-index 0 0 1 (perm)))
  (is-equal 0 (loise:get-gradient-index 0 1 0 (perm)))
  (is-equal 8 (loise:get-gradient-index 0 1 1 (perm)))
  (is-equal 7 (loise:get-gradient-index 1 0 0 (perm)))
  (is-equal 8 (loise:get-gradient-index 1 0 1 (perm)))
  (is-equal 2 (loise:get-gradient-index 1 1 0 (perm)))
  (is-equal 3 (loise:get-gradient-index 1 1 1 (perm)))
  (is-equal 8 (loise:get-gradient-index 1 10 100 (perm)))
  (is-equal 6 (loise:get-gradient-index 100 10 1 (perm))))

(deftest get-noise-contribution
  (is-equal 0.0 (loise:get-noise-contribution 0 0 0 0))
  (is-equal 0.0 (loise:get-noise-contribution 1 0 0 0))
  (is-equal 0.0 (loise:get-noise-contribution 1 0 0 1))
  (is-equal 0.0 (loise:get-noise-contribution 1 1 1 1))
  (is-equal 0.0 (loise:get-noise-contribution 2 1 1 1))
  (is-equal 2.0 (loise:get-noise-contribution 4 1 1 1))
  (is-equal 20.0 (loise:get-noise-contribution 4 5 10 15))
  (is-equal 101.0 (loise:get-noise-contribution 4 1 10 100))
  (is-equal 2.0 (loise:get-noise-contribution 8 1 1 1))
  (is-equal 25.0 (loise:get-noise-contribution 8 5 10 15))
  (is-equal 110.0 (loise:get-noise-contribution 8 1 10 100)))

(deftest perlin
  (is-equal -0.3772216257243449 (loise:perlin 3.14 1.59 2.65 (opts)))
  (let ((expected (list 0.0 0.11 0.23 0.37 0.46 0.5 0.46 0.37 0.23 0.11))
        (input (lists:map (lambda (x) (/ x 10)) (lists:seq 0 9))))
    (lists:zipwith
      (lambda (a b)
        (is-equal a (lutil-math:round (loise:perlin b) 2)))
      expected
      input)))

(deftest which-simplex
  (is-equal "100110" (list->string (loise:which-simplex 0 0 0)))
  (is-equal "001101" (list->string (loise:which-simplex 0 0 1)))
  (is-equal "010110" (list->string (loise:which-simplex 0 1 0)))
  (is-equal "010011" (list->string (loise:which-simplex 0 1 1)))
  (is-equal "100101" (list->string (loise:which-simplex 1 0 1)))
  (is-equal "001011" (list->string (loise:which-simplex 1 2 3))))

(deftest simplex
  (is-equal 0.44 (lutil-math:round (loise:simplex 0.1) 2))
  (is-equal 0.44 (lutil-math:round (loise:simplex 0.1 (opts)) 2))
  (is-equal 0.81 (lutil-math:round (loise:simplex 0.1 0.1 (opts)) 2))
  (is-equal -0.39 (lutil-math:round (loise:simplex 0.9 0.9 (opts)) 2))
  (is-equal 0.94 (lutil-math:round (loise:simplex 0.1 0.2 (opts)) 2))
  (is-equal -0.08 (lutil-math:round (loise:simplex 0.1 0.2 0.9 (opts)) 2)))


(deftest get-perlin-point-without-opts
  (is-equal 0.0
    (lutil-math:round (loise:get-perlin-point '(0 0) '(256 256) 1) 2))
  (is-equal 0.5
    (lutil-math:round (loise:get-perlin-point '(127) '(256) 1) 2))
  (is-equal 0.56
    (lutil-math:round (loise:get-perlin-point '(127 64) '(256 256) 1) 2))
  (is-equal 0.49
    (lutil-math:round
      (loise:get-perlin-point '(127 64 32) '(256 256 256) 1) 2)))

(deftest get-simplex-point-without-opts
  (is-equal 0.0
    (lutil-math:round (loise:get-simplex-point '(0 0) '(256 256) 1) 2))
  (is-equal 0.31
    (lutil-math:round (loise:get-simplex-point '(127) '(256) 1) 2))
  (is-equal 0.14
    (lutil-math:round (loise:get-simplex-point '(127 64) '(256 256) 1) 2))
  (is-equal 0.21
    (lutil-math:round
      (loise:get-simplex-point '(127 64 32) '(256 256 256) 1) 2)))
