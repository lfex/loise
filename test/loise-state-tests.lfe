(defmodule loise-state-tests
  (behaviour ltest-system))

(include-lib "ltest/include/ltest-macros.lfe")

(defun set-up ()
  (prog1
    (loise:start)
    (logger:set_primary_config #m(level error))))

(defun tear-down (setup-result)
  (let ((stop-result (loise:stop)))
    (is-equal 'ok stop-result)))

(deftestcase start-up (setup-result)
  (tuple "start-up"
         (is-equal '#(ok (loise)) setup-result)))

(deftestcase ping-check (setup-result)
  (tuple "ping-check"
         (is-equal 'pong (loise-state:ping))))

(deftestcase layers (setup-result)
  (tuple "set-and-get-layer"
         (loise-state:set-layer 'test-layer '(1 2 3))
         (is-equal '(1 2 3)
                   (loise-state:get-layer 'test-layer))))

(deftestgen suite
  (tuple 'foreach
         (defsetup set-up)
         (defteardown tear-down)
         (deftestcases
           start-up
           ping-check
           layers)))
