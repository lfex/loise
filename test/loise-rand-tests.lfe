(defmodule loise-rand-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")

(deftest seed-tuple
  (is-equal #(42 0 0) (loise-rand:seed-tuple 42))
  (is-equal #(0 42 0) (loise-rand:seed-tuple '(0 42)))
  (is-equal #(0 0 42) (loise-rand:seed-tuple '(0 0 42)))
  (is-equal #(42 42 42) (loise-rand:seed-tuple '(42 42 42))))

(defmodule loise-rand-system-tests
  (behaviour ltest-system)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")

(defun set-up () (loise-tests-support:set-up))
(defun tear-down (setup-result) (loise-tests-support:tear-down setup-result))

(defun opts ()
  (loise-ascii:default-options))

(defun rand-opts ()
  (loise-ascii:default-options #m(random? true)))

(defun test-data ()
  '(apple blueberries kiwi mango orange strawberries))

(deftestcase choose-one (setup-result)
  (is-equal 'orange
            (element 1 (loise-rand:choose (test-data)
                                          (loise-rand:state #m(seed 42))))))

(deftestcase choose*-one (setup-result)
  (is-equal 'orange
            (loise-rand:choose* (test-data)
                                (loise-rand:state #m(seed 42)))))

(deftestcase choose*-many (setup-result)
  (is-equal '(mango strawberries orange strawberries apple
              kiwi strawberries orange blueberries orange)
            (loise-rand:choose* (test-data)
                                10
                                (loise-rand:state #m(seed 42)))))

(deftestcase update-perm-table-default (setup-result)
  (is-equal (loise-defaults:permutation-table)
            (loise-state:get 'perm-table)))

(deftestcase update-perm-table-without-random (setup-result)
  (opts)
  (is-equal '(151 160 137 91 90 15 131 13 201 95)
            (lists:sublist
             (loise-state:get 'perm-table)
             10)))
(deftestcase update-perm-table-with-random (setup-result)
  (rand-opts)
  (is-equal '(189 69 252 70 95 138 96 186 227 118)
            (lists:sublist
             (loise-state:get 'perm-table)
             10)))

(deftestgen suite
  (tuple 'foreach
         (defsetup set-up)
         (defteardown tear-down)
         (deftestcases
           choose-one
           choose*-one
           choose*-many
           update-perm-table-default
           update-perm-table-without-random
           update-perm-table-with-random)))
