(defmodule loise-data-tests
  (behaviour ltest-system)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")

(defun set-up () (loise-tests-support:set-up))
(defun tear-down (setup-result) (loise-tests-support:tear-down setup-result))

(defun opts (overrides)
  (loise-data:default-options overrides))

(deftestcase opts-checks (setup-result)
  (let ((opts (opts #m(width 4 height 4))))
    (is-equal '(4 4)
              (mref opts 'dim))
    (is-equal '#(4 4)
              (mref opts 'size))))

(deftestcase row-with-grades (setup-result)
  (let* ((opts (opts #m(width 4 height 4 graded? true)))
         (scale-func (mref opts 'scale-func))
         (dim (mref opts 'dim))
         (mult (mref opts 'multiplier))
         (graded? (mref opts 'graded?))
         (grades (mref opts 'grades))
         (value-range (loise-opts:value-range opts))
         (size (mref opts 'size)))
    (is-equal '(#((0 1) 0) #((1 1) 0) #((2 1) 0) #((3 1) 0))
              (loise-data:row #'loise-png-tests:point-func/4
                              scale-func
                              1
                              #(0 0)
                              size
                              mult
                              graded?
                              grades
                              value-range
                              opts))))

(deftestcase row-without-grades (setup-result)
  (let* ((opts (opts #m(width 4 height 4)))
         (scale-func (mref opts 'scale-func))
         (dim (mref opts 'dim))
         (mult (mref opts 'multiplier))
         (graded? (mref opts 'graded?))
         (grades (mref opts 'grades))
         (value-range (loise-opts:value-range opts))
         (size (mref opts 'size)))
    (is-equal '(#((0 1) 0.125) #((1 1) 0.25) #((2 1) 0.375) #((3 1) 0.5))
              (loise-data:row #'loise-png-tests:point-func/4
                              scale-func
                              1
                              #(0 0)
                              size
                              mult
                              graded?
                              grades
                              value-range
                              opts))))

(deftestcase matrix-perlin (setup-result)
  (let ((opts (opts #m(width 4 height 4 round? true precision 2))))
    (is-equal '(#((0 0) 0.0) #((1 0) 0.0) #((2 0) 0.0) #((3 0) 0.0)
                #((0 1) 0.0) #((1 1) 0.0) #((2 1) 0.0) #((3 1) 0.0)
                #((0 2) 0.0) #((1 2) 0.0) #((2 2) 0.0) #((3 2) 0.0)
                #((0 3) 0.0) #((1 3) 0.0) #((2 3) 0.0) #((3 3) 0.0))
              (loise-data:matrix 'perlin opts))))

(deftestcase matrix-simplex (setup-result)
  (let ((opts (opts #m(width 4 height 4 round? true precision 2))))
    (is-equal '(#((0 0) 0.0) #((1 0) -0.77) #((2 0) 0.0) #((3 0) 0.0)
                #((0 1) 0.77) #((1 1) -0.77) #((2 1) 0.0) #((3 1) -0.77)
                #((0 2) 0.77) #((1 2) 0.0) #((2 2) -0.81) #((3 2) -0.04)
                #((0 3) 0.0) #((1 3) -0.73) #((2 3) 0.04) #((3 3) 0.0))
              (loise-data:matrix 'simplex opts))))

(deftestcase matrix-full (setup-result)
  (let ((opts (opts #m(width 4 height 4 round? true precision 2 data-format matrix))))
    (is-equal '(#(0 (#((0 0) 0.0)
                     #((1 0) -0.77)
                     #((2 0) 0.0)
                     #((3 0) 0.0)))
                #(1 (#((0 1) 0.77)
                     #((1 1) -0.77)
                     #((2 1) 0.0)
                     #((3 1) -0.77)))
                #(2 (#((0 2) 0.77)
                     #((1 2) 0.0)
                     #((2 2) -0.81)
                     #((3 2) -0.04)))
                #(3 (#((0 3) 0.0)
                     #((1 3) -0.73)
                     #((2 3) 0.04)
                     #((3 3) 0.0))))
              (loise-data:matrix 'simplex opts))))

(deftestcase matrix-flat (setup-result)
  (let ((opts (opts #m(width 4 height 4 round? true precision 2 data-format flat))))
    (is-equal '(#((0 0) 0.0) #((1 0) -0.77) #((2 0) 0.0) #((3 0) 0.0)
                #((0 1) 0.77) #((1 1) -0.77) #((2 1) 0.0) #((3 1) -0.77)
                #((0 2) 0.77) #((1 2) 0.0) #((2 2) -0.81) #((3 2) -0.04)
                #((0 3) 0.0) #((1 3) -0.73) #((2 3) 0.04) #((3 3) 0.0))
              (loise-data:matrix 'simplex opts))))

(deftestgen suite
  (tuple 'foreach
         (defsetup set-up)
         (defteardown tear-down)
         (deftestcases
           opts-checks
           row-with-grades
           row-without-grades
           matrix-perlin
           matrix-simplex
           matrix-full
           matrix-flat)))
