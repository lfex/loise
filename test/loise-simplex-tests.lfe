(defmodule loise-simplex-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")

(deftest which
  (is-equal "100110" (loise-util:int-list->str (loise-simplex:which 0 0 0)))
  (is-equal "001101" (loise-util:int-list->str (loise-simplex:which 0 0 1)))
  (is-equal "010110" (loise-util:int-list->str (loise-simplex:which 0 1 0)))
  (is-equal "010011" (loise-util:int-list->str (loise-simplex:which 0 1 1)))
  (is-equal "100101" (loise-util:int-list->str (loise-simplex:which 1 0 1)))
  (is-equal "001011" (loise-util:int-list->str (loise-simplex:which 1 2 3))))

(deftest simplex
  (let ((opts (loise-simplex:options)))
    (is-equal 0.44 (lutil-math:round (loise-simplex:1d 0.1 opts) 2))
    (is-equal 0.81 (lutil-math:round (loise-simplex:2d 0.1 0.1 opts) 2))
    (is-equal -0.39 (lutil-math:round (loise-simplex:2d 0.9 0.9 opts) 2))
    (is-equal 0.94 (lutil-math:round (loise-simplex:2d 0.1 0.2 opts) 2))
    (is-equal -0.08 (lutil-math:round (loise-simplex:3d 0.1 0.2 0.9 opts) 2))))

(deftest simplex-via-api
  (let ((opts (loise-simplex:options)))
    (is-equal 0.44 (lutil-math:round (loise:simplex 0.1 opts) 2))
    (is-equal 0.81 (lutil-math:round (loise:simplex 0.1 0.1 opts) 2))
    (is-equal -0.39 (lutil-math:round (loise:simplex 0.9 0.9 opts) 2))
    (is-equal 0.94 (lutil-math:round (loise:simplex 0.1 0.2 opts) 2))
    (is-equal -0.08 (lutil-math:round (loise:simplex 0.1 0.2 0.9 opts) 2))))

(deftest simplex-via-api-without-opts
  (is-equal 0.44 (lutil-math:round (loise:simplex 0.1) 2))
  (is-equal 0.81 (lutil-math:round (loise:simplex 0.1 0.1) 2))
  (is-equal -0.39 (lutil-math:round (loise:simplex 0.9 0.9) 2))
  (is-equal 0.94 (lutil-math:round (loise:simplex 0.1 0.2) 2))
  (is-equal -0.08 (lutil-math:round (loise:simplex 0.1 0.2 0.9) 2)))

(deftest point-without-opts
  (let ((precision 2))
    (is-equal 0.0
              (lutil-math:round
               (loise-simplex:point '(0 0) '(256 256) 1) precision))
    (is-equal 0.31
              (lutil-math:round
               (loise-simplex:point '(127) '(256) 1) precision))
    (is-equal 0.14
              (lutil-math:round
               (loise-simplex:point '(127 64) '(256 256) 1) precision))
    (is-equal 0.21
              (lutil-math:round
               (loise-simplex:point '(127 64 32) '(256 256 256) 1) precision))))
