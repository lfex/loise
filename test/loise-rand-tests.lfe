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

(defun set-up ()
  (prog1
    (loise:start)
    (logger:set_primary_config #m(level error))))

(defun tear-down (setup-result)
  (let ((stop-result (loise:stop)))
    (is-equal 'ok stop-result)))

(deftestcase update-perm-table-default (setup-result)
  (tuple "update-perm-table-default"
         (let* ((opts (loise-ascii:default-options))
                (rand-opts (loise-ascii:default-options #m(random? true))))
           (is-equal (loise-defaults:permutation-table)
                     (mref opts 'perm-table)))))

(deftestcase update-perm-table (setup-result)
  (tuple "update-perm-table"
         (let* ((opts (loise-ascii:default-options))
                (rand-opts (loise-ascii:default-options #m(random? true))))
           (is-equal '(151 160 137 91 90 15 131 13 201 95)
                     (lists:sublist
                      (mref opts 'perm-table)
                      10))
           (is-equal '(35 94 168 230 251 221 92 64 92 36)
                     (lists:sublist
                      (mref rand-opts 'perm-table)
                      10)))))

(deftestgen suite
  (tuple 'foreach
         (defsetup set-up)
         (defteardown tear-down)
         (deftestcases
           update-perm-table-default
           update-perm-table)))
