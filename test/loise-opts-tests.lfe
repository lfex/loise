(defmodule loise-opts-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")
(include-lib "include/options.lfe")

(defun tiny-opts ()
  `(#(width 2)
    #(height 2)))

(deftest dimensions
  (is-equal '(256 128) (loise-opts:dimensions (default-egd-options)))
  (is-equal '(56 36) (loise-opts:dimensions (default-ascii-options)))
  (is-equal '(2 2) (loise-opts:dimensions (tiny-opts))))

(deftest update-perm-table
  (let* ((opts (default-ascii-options))
         (new-opts (loise-opts:update-perm-table opts))) 
    (is-equal (default-permutation-table)
              (loise-opts:perm-table opts))
    (is-equal '(151 160 137 91 90 15 131 13 201 95)
              (lists:sublist
               (loise-opts:perm-table new-opts)
               10))))
