(defmodule loise-state-tests
  (behaviour ltest-system))

(include-lib "ltest/include/ltest-macros.lfe")

(defun set-up () (loise-tests-support:set-up))
(defun tear-down (setup-result) (loise-tests-support:tear-down setup-result))

(deftestcase start-up (setup-result)
  (is-equal '#(ok (loise)) setup-result))

(deftestcase ping-check (setup-result)
  (is-equal 'pong (loise-state:ping)))

(deftestcase layers (setup-result)
  (loise-state:set-layer 'test-layer '(1 2 3) #m())
  (timer:sleep 1000)
  (is-equal '(1 2 3)
            (loise-state:get-layer 'test-layer)))

(deftestgen suite
  (tuple 'foreach
         (defsetup set-up)
         (defteardown tear-down)
         (deftestcases
           start-up
           ping-check
           layers)))
