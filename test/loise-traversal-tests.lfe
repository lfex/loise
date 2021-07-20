(defmodule loise-traversal-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")

(deftest neighbor-check
  (let ((opts #m(width 4 height 3)))
    (is-equal #(0 0) (loise-traversal:neighbor-check #(1 1) #(0 0) opts))
    (is-equal #(1 1) (loise-traversal:neighbor-check #(2 2) #(1 1) opts))
    (is-equal 'false (loise-traversal:neighbor-check #(1 1) #(1 1) opts))
    (is-equal #(3 2) (loise-traversal:neighbor-check #(2 2) #(3 2) opts))
    ;; Exceed lower bounds
    (is-equal #(0 2) (loise-traversal:neighbor-check #(1 1) #(-1 2) opts))
    (is-equal #(2 0) (loise-traversal:neighbor-check #(1 1) #(2 -4) opts))
    ;; Exceed upper bounds
    (is-equal #(3 1) (loise-traversal:neighbor-check #(2 2) #(14 1) opts))
    (is-equal #(1 2) (loise-traversal:neighbor-check #(2 2) #(1 23) opts))
    ;; Exceed both bounds
    (is-equal #(0 0) (loise-traversal:neighbor-check #(1 1) #(-22 -4) opts))
    (is-equal #(3 2) (loise-traversal:neighbor-check #(2 2) #(14 23) opts))))

(defun nbr-data-1 ()
  '((#(-1 -1) #(-1 0) #(-1 1))
    (#( 0 -1) #( 0 0) #( 0 1))
    (#( 1 -1) #( 1 0) #( 1 1))))

(defun nbr-data-2 ()
  '((#(-2 -2) #(-2 -1) #(-2 0) #(-2 1) #(-2 2))
    (#(-1 -2) #(-1 -1) #(-1 0) #(-1 1) #(-1 2))
    (#( 0 -2) #( 0 -1) #( 0 0) #( 0 1) #( 0 2))
    (#( 1 -2) #( 1 -1) #( 1 0) #( 1 1) #( 1 2))
    (#( 2 -2) #( 2 -1) #( 2 0) #( 2 1) #( 2 2))))

(defun nbr-data-3 ()
  '((#(1 1) #(1 2) #(1 3))
    (#(2 1) #(2 2) #(2 3))
    (#(3 1) #(3 2) #(3 3))))

(defun nbr-data-4 ()
  '((#(1 1) #(1 2) #(1 3) #(1 4) #(1 5))
    (#(2 1) #(2 2) #(2 3) #(2 4) #(2 5))
    (#(3 1) #(3 2) #(3 3) #(3 4) #(3 5))
    (#(4 1) #(4 2) #(4 3) #(4 4) #(4 5))
    (#(5 1) #(5 2) #(5 3) #(5 4) #(5 5))))

(defun nbr-data-5 ()
  '((#(92 16) #(92 17) #(92 18))
    (#(93 16) #(93 17) #(93 18))
    (#(94 16) #(94 17) #(94 18))))

(deftest neighbors-with-center
  (is-equal (nbr-data-1)
            (loise-traversal:neighbors #(0 0) 1 #m(include-center? true)))
  (is-equal (nbr-data-2)
            (loise-traversal:neighbors #(0 0) 2 #m(include-center? true)))
  (is-equal (nbr-data-3)
            (loise-traversal:neighbors #(2 2) 1 #m(include-center? true)))
  (is-equal (nbr-data-4)
            (loise-traversal:neighbors #(3 3) 2 #m(include-center? true)))
  (is-equal (nbr-data-5)
            (loise-traversal:neighbors #(93 17) 1 #m(include-center? true))))

(defun nbr-data-nc-1 ()
  '((#(-1 -1) #(-1 0) #(-1 1))
    (#( 0 -1)  false  #( 0 1))
    (#( 1 -1) #( 1 0) #( 1 1))))

(defun nbr-data-nc-2 ()
  '((#(-2 -2) #(-2 -1) #(-2 0) #(-2 1) #(-2 2))
    (#(-1 -2) #(-1 -1) #(-1 0) #(-1 1) #(-1 2))
    (#( 0 -2) #( 0 -1)  false  #( 0 1) #( 0 2))
    (#( 1 -2) #( 1 -1) #( 1 0) #( 1 1) #( 1 2))
    (#( 2 -2) #( 2 -1) #( 2 0) #( 2 1) #( 2 2))))

(defun nbr-data-nc-3 ()
  '((#(1 1) #(1 2) #(1 3))
    (#(2 1)  false #(2 3))
    (#(3 1) #(3 2) #(3 3))))

(defun nbr-data-nc-4 ()
  '((#(1 1) #(1 2) #(1 3) #(1 4) #(1 5))
    (#(2 1) #(2 2) #(2 3) #(2 4) #(2 5))
    (#(3 1) #(3 2)  false #(3 4) #(3 5))
    (#(4 1) #(4 2) #(4 3) #(4 4) #(4 5))
    (#(5 1) #(5 2) #(5 3) #(5 4) #(5 5))))

(defun nbr-data-nc-5 ()
  '((#(92 16) #(92 17) #(92 18))
    (#(93 16)  false   #(93 18))
    (#(94 16) #(94 17) #(94 18))))

(deftest neighbors-without-center
  (is-equal (nbr-data-nc-1)
            (loise-traversal:neighbors #(0 0) 1 #m(include-center? false)))
  (is-equal (nbr-data-nc-2)
            (loise-traversal:neighbors #(0 0) 2 #m(include-center? false)))
  (is-equal (nbr-data-nc-3)
            (loise-traversal:neighbors #(2 2) 1 #m(include-center? false)))
  (is-equal (nbr-data-nc-4)
            (loise-traversal:neighbors #(3 3) 2 #m(include-center? false)))
  (is-equal (nbr-data-nc-5)
            (loise-traversal:neighbors #(93 17) 1 #m(include-center? false))))

(defun nbr-data-f-1 ()
  '(       #(0 1)
    #(1 0) #(1 1)))

(defun nbr-data-f-2 ()
  '(#(0 0) #(0 1) #(0 2)
    #(1 0)        #(1 2)
    #(2 0) #(2 1) #(2 2)))

(defun nbr-data-f-3 ()
  '(#(1 1) #(1 2)
    #(2 1)      ))

(defun nbr-data-f-4 ()
  '(       #(0 1) #(0 2)
    #(1 0) #(1 1) #(1 2)
    #(2 0) #(2 1) #(2 2)))

(defun nbr-data-f-5 ()
  '(#(0 0) #(0 1) #(0 2) #(0 3)
    #(1 0)        #(1 2) #(1 3)
    #(2 0) #(2 1) #(2 2) #(2 3)
    #(3 0) #(3 1) #(3 2) #(3 3)))

(defun nbr-data-f-6 ()
  '(#(0 0) #(0 1) #(0 2) #(0 3) #(0 4)
    #(1 0) #(1 1) #(1 2) #(1 3) #(1 4)
    #(2 0) #(2 1)        #(2 3) #(2 4)
    #(3 0) #(3 1) #(3 2) #(3 3) #(3 4)
    #(4 0) #(4 1) #(4 2) #(4 3) #(4 4)))

(defun nbr-data-f-7 ()
  '(#(2 2) #(2 3) #(2 4)
    #(3 2) #(3 3) #(3 4)
    #(4 2) #(4 3)       ))

(deftest neighbors-radius-1
  (let ((opts (loise-traversal:options #m(width 3 height 3 radius 1))))
    (is-equal (nbr-data-f-1)
              (loise-traversal:neighbors #(0 0) opts))
    (is-equal (nbr-data-f-2)
              (loise-traversal:neighbors #(1 1) opts))
    (is-equal (nbr-data-f-3)
              (loise-traversal:neighbors #(2 2) opts))
    (is-equal '(#(2 2))
              (loise-traversal:neighbors #(93 17) opts))))

(deftest neighbors-radius-2
  (let ((opts (loise-traversal:options #m(width 5 height 5 radius 2))))
    (is-equal (nbr-data-f-4)
              (loise-traversal:neighbors #(0 0) opts))
    (is-equal (nbr-data-f-5)
              (loise-traversal:neighbors #(1 1) opts))
    (is-equal (nbr-data-f-6)
              (loise-traversal:neighbors #(2 2) opts))
    (is-equal (nbr-data-f-7)
              (loise-traversal:neighbors #(4 4) opts))))

(defmodule loise-traversal-system-tests
  (behaviour ltest-system)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")

(defun set-up () (loise-tests-support:set-up))
(defun tear-down (setup-result) (loise-tests-support:tear-down setup-result))

(deftestcase brownian-path (setup-result)
  (let* ((opts (loise-traversal:options))
         (layer-name 'test-layer)
         (_ (loise:add-layer layer-name opts))
         (path (loise-traversal:brownian layer-name #(42 16) opts)))
    (is-equal '(#(42 16) #(43 16) #(42 16) #(42 17) #(41 17)
                #(42 17) #(41 18) #(42 17) #(42 18) #(42 17))
              (lists:sublist path 1 10))))

(deftestcase walk-path (setup-result)
  (let* ((opts (loise-traversal:options #m(round? true)))
         (layer-name 'test-layer)
         (_ (loise:add-layer layer-name opts))
         (path (loise-traversal:brownian layer-name #(0 0) opts))
         (values (loise-traversal:walk layer-name path opts)))
    (is-equal '(0.0 0.32779 0.57816 0.69742 0.57816
                0.32779 0.92603 0.69742 0.82776 0.68234)
              (lists:sublist values 1 10))))

(deftestcase walk-path-scaled (setup-result)
  (let* ((opts (loise-traversal:options
               `#m(scale-func ,#'lutil-math:midi-scale/2)))
         (layer-name 'test-layer)
         (_ (loise:add-layer layer-name opts))
         (path (loise-traversal:brownian layer-name #(0 0) opts))
         (values (loise-traversal:walk layer-name path opts)))
    (is-equal '(64 84 100 108 100 84 122 108 116 107)
              (lists:sublist values 1 10))))

(deftestgen suite
  (tuple 'foreach
         (defsetup set-up)
         (defteardown tear-down)
         (deftestcases
           brownian-path
           walk-path
           walk-path-scaled)))
