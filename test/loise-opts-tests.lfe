(defmodule loise-opts-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")
(include-lib "include/options.lfe")

(defun tiny-opts ()
  `(#(width 2)
    #(height 2)))

(deftest dimensions
  (is-equal '(2 2) (loise-opts:dimensions (tiny-opts))))

(deftest update-perm-table
  (let* ((opts (default-base-options))
         (opts2 (loise-opts:update-perm-table opts))
         (rand-opts (loise-opts:update-perm-table (cons #(random? true) opts))))
    (is-equal (default-permutation-table)
              (loise-opts:perm-table opts))
    (is-equal '(151 160 137 91 90 15 131 13 201 95)
              (lists:sublist
               (loise-opts:perm-table opts)
               10))
    (is-equal '(151 160 137 91 90 15 131 13 201 95)
              (lists:sublist
               (loise-opts:perm-table opts2)
               10))
    (is-equal '(35 94 168 230 251 221 92 64 92 36)
              (lists:sublist
               (loise-opts:perm-table rand-opts)
               10))))
