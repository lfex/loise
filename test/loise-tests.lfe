(defmodule loise-tests
  (behaviour ltest-system)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")

(defun set-up () (loise-tests-support:set-up))
(defun tear-down (setup-result) (loise-tests-support:tear-down setup-result))

;;; Data API

(deftestcase get-path (setup-result)
  (let* ((opts #m(round? true precision 2))
         (layer-name 'test-layer)
         (_ (loise:add-layer layer-name opts)))
    (timer:sleep 1000)
    (is-equal '(#(42 16) #(43 16) #(42 16) #(42 17) #(41 17)
                #(42 17) #(41 18) #(42 17) #(42 18) #(42 17))
              (lists:sublist
               (loise:get-path layer-name 'brownian #(42 16))
               1
               10))))

(deftestcase traverse (setup-result)
  (let* ((opts #m(round? true precision 1))
         (layer-name 'test-layer)
         (_ (loise:add-layer layer-name opts)))
    (timer:sleep 1000)
    (is-equal '(0.0 0.3 0.6 0.7 0.6 0.3 0.9 0.7 0.8 0.7)
              (lists:sublist
               (loise:traverse layer-name 'brownian #(0 0))
               1
               10))
    (is-equal '(-0.2 -0.1 -0.2 -0.3 -0.3 -0.2 -0.3 -0.2 -0.2 -0.1)
              (lists:sublist
               (loise:traverse layer-name 'brownian #(42 16))
               1
               10))))

(deftestcase traverse-default-start (setup-result)
  (let* ((opts #m(round? true precision 1))
         (layer-name 'test-layer)
         (_ (loise:add-layer layer-name opts)))
    (timer:sleep 1000)
    (is-equal '(0.0 0.3 0.6 0.7 0.6 0.3 0.9 0.7 0.8 0.7)
              (lists:sublist
               (loise:traverse layer-name 'brownian)
               1
               10))))

(deftestgen suite
  (tuple 'foreach
         (defsetup set-up)
         (defteardown tear-down)
         (deftestcases
           get-path
           traverse
           traverse-default-start)))
