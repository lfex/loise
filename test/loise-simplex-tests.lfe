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

(defmodule loise-simplex-system-tests
  (behaviour ltest-system)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")

(defun set-up ()
  (prog1
    (loise:start)
    (logger:set_primary_config #m(level error))))

(defun tear-down (setup-result)
  (let ((stop-result (loise:stop)))
    (is-equal 'ok stop-result)))


(deftestcase simplex (setup-result)
  (tuple "simplex"
         (let ((opts (loise-defaults:base-options)))
           (is-equal 0.44 (lutil-math:round (loise-simplex:1d 0.1 opts) 2))
           (is-equal 0.81 (lutil-math:round (loise-simplex:2d 0.1 0.1 opts) 2))
           (is-equal -0.39 (lutil-math:round (loise-simplex:2d 0.9 0.9 opts) 2))
           (is-equal 0.94 (lutil-math:round (loise-simplex:2d 0.1 0.2 opts) 2))
           (is-equal -0.08 (lutil-math:round (loise-simplex:3d 0.1 0.2 0.9 opts) 2)))))

(deftestcase simplex-via-api (setup-result)
  (tuple "simplex-via-api"
         (let ((opts (loise-defaults:base-options)))
           (is-equal 0.44 (lutil-math:round (loise:simplex 0.1 opts) 2))
           (is-equal 0.81 (lutil-math:round (loise:simplex 0.1 0.1 opts) 2))
           (is-equal -0.39 (lutil-math:round (loise:simplex 0.9 0.9 opts) 2))
           (is-equal 0.94 (lutil-math:round (loise:simplex 0.1 0.2 opts) 2))
           (is-equal -0.08 (lutil-math:round (loise:simplex 0.1 0.2 0.9 opts) 2)))))

(deftestcase simplex-via-api-without-opts (setup-result)
  (tuple "simplex-via-api-without-opts"
         (is-equal 0.44 (lutil-math:round (loise:simplex 0.1) 2))
         (is-equal 0.81 (lutil-math:round (loise:simplex 0.1 0.1) 2))
         (is-equal -0.39 (lutil-math:round (loise:simplex 0.9 0.9) 2))
         (is-equal 0.94 (lutil-math:round (loise:simplex 0.1 0.2) 2))
         (is-equal -0.08 (lutil-math:round (loise:simplex 0.1 0.2 0.9) 2))))

(deftestcase point (setup-result)
  (tuple "point"
         (let ((precision 2)
               (opts (loise-defaults:base-options)))
           (is-equal 0.0
                     (lutil-math:round
                      (loise-simplex:point '(0 0) '(256 256) 1 opts) precision))
           (is-equal 0.31
                     (lutil-math:round
                      (loise-simplex:point '(127) '(256) 1 opts) precision))
           (is-equal 0.14
                     (lutil-math:round
                      (loise-simplex:point '(127 64) '(256 256) 1 opts) precision))
           (is-equal 0.21
                     (lutil-math:round
                      (loise-simplex:point '(127 64 32) '(256 256 256) 1 opts) precision)))))

(deftestgen suite
  (tuple 'foreach
         (defsetup set-up)
         (defteardown tear-down)
         (deftestcases
           simplex
           simplex-via-api
           simplex-via-api-without-opts
           point)))
