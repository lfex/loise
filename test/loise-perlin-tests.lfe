(defmodule loise-perlin-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")

(defun opts ()
  (loise-const:base-options))

(deftest perlin
  (is-equal -0.3772216257243449 (loise-perlin:3d 3.14 1.59 2.65 (opts)))
  (let ((expected (list 0.0 0.11 0.23 0.37 0.46 0.5 0.46 0.37 0.23 0.11))
        (input (lists:map (lambda (x) (/ x 10)) (lists:seq 0 9))))
    (lists:zipwith
     (lambda (a b)
       (is-equal a (lutil-math:round (loise:perlin b) 2)))
     expected
     input)))

(deftest point-without-opts
  (is-equal 0.0
            (lutil-math:round (loise-perlin:point '(0 0) '(256 256) 1) 2))
  (is-equal 0.5
            (lutil-math:round (loise-perlin:point '(127) '(256) 1) 2))
  (is-equal 0.56
            (lutil-math:round (loise-perlin:point '(127 64) '(256 256) 1) 2))
  (is-equal 0.49
            (lutil-math:round
             (loise-perlin:point '(127 64 32) '(256 256 256) 1) 2)))
