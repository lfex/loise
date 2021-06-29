(defmodule loise-ascii-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")

(defun opts ()
  (loise-ascii:default-options))

(defun tiny-opts ()
  (loise-ascii:default-options #m(width 2 height 2)))

(defun tiny-grid-perlin ()
  (++ "* * *" "\n"
      "* * *" "\n"
      "* * *"))

(defun tiny-grid-simplex ()
  (++ "* * n" "\n"
      "~ A *" "\n"
      "* * ~"))

(deftest dimensions
  (is-equal '(56 36) (loise:dim (opts))))

(deftest grades
    (is-equal 6 (mref (opts) 'grades-count))
    (is-equal '(0 51.0 102.0 153.0 204.0 255.0)
              (mref (opts) 'grades))
    (is-equal '("A" "^" "n" "*" "~" "~")
              (mref (opts) 'ascii-map))
    (is-equal '(#(0 #("A" whiteb))
                #(51.0 #("^" yellow))
                #(102.0 #("n" green))
                #(153.0 #("*" greenb))
                #(204.0 #("~" blue))
                #(255.0 #("~" blue)))
              (mref (opts) 'color-map)))

(deftest color-map
  (is-equal
   '(#(0 #("A" whiteb))
     #(51.0 #("^" yellow))
     #(102.0 #("n" green))
     #(153.0 #("*" greenb))
     #(204.0 #("~" blue))
     #(255.0 #("~" blue)))
   (mref (opts) 'color-map)))

(deftest point-perlin
  (let* ((scale-func (mref (opts) 'scale-func))
         (dim (mref (opts) 'dim))
         (mult (mref (opts) 'multiplier))
         (graded? (mref (opts) 'graded?))
         (grades (mref (opts) 'grades))
         (value-range (mref (opts) 'value-range))
         (legend (mref (opts) 'color-map))
         (test-func (lambda (x) (loise-ascii:point
                                 #'loise-perlin:point/4
                                 scale-func
                                 x
                                 dim
                                 mult
                                 graded?
                                 grades
                                 value-range
                                 legend
                                 (opts)))))
    (is-equal #("*" greenb) (funcall test-func (0 0)))
    (is-equal #("~" blue) (funcall test-func (5 3)))
    (is-equal #("n" green) (funcall test-func (10 10)))
    (is-equal #("n" green) (funcall test-func (10 10)))
    (is-equal #("^" yellow) (funcall test-func (8 16)))))

(deftest point-simplex
  (let* ((scale-func (mref (opts) 'scale-func))
         (dim (mref (opts) 'dim))
         (mult (mref (opts) 'multiplier))
         (graded? (mref (opts) 'graded?))
         (grades (mref (opts) 'grades))
         (value-range (mref (opts) 'value-range))
         (legend (mref (opts) 'color-map))
         (test-func (lambda (x) (loise-ascii:point
                                 #'loise-simplex:point/4
                                 scale-func
                                 x
                                 dim
                                 mult
                                 graded?
                                 grades
                                 value-range
                                 legend
                                 (opts)))))
    (is-equal #("*" greenb) (funcall test-func (0 0)))
    (is-equal #("~" blue) (funcall test-func (3 3)))
    (is-equal #("n" green) (funcall test-func (42 15)))
    (is-equal #("^" yellow) (funcall test-func (15 0)))
    (is-equal #("A" whiteb) (funcall test-func (14 10)))))

(deftest row-perlin
  (let* ((scale-func (mref (opts) 'scale-func))
         (size (mref (opts) 'size))
         (mult (mref (opts) 'multiplier))
         (graded? (mref (opts) 'graded?))
         (grades (mref (opts) 'grades))
         (value-range (mref (opts) 'value-range))
         (legend (mref (opts) 'color-map))
         (test-func (lambda (row-index)
                      (lists:flatten
                       (string:replace
                        (loise-ascii:row #'loise-perlin:point/4
                                         scale-func
                                         row-index
                                         #(0 0)
                                         size
                                         mult
                                         graded?
                                         grades
                                         value-range
                                         legend
                                         (opts))
                        " " "" 'all)))))
    (is-equal "*****~~~~~*****nnnn^^^^^nnnn*****************************"
              (funcall test-func 0))
    (is-equal "*nnnn^^^^^nnnn***************nnnnnnnnnnnnn********nnnnnn*"
              (funcall test-func 18))
    (is-equal "*nnnnnnnnnnnnn*******************************************"
              (funcall test-func 36))))

(deftest row-simplex
  (let* ((scale-func (mref (opts) 'scale-func))
         (size (mref (opts) 'size))
         (mult (mref (opts) 'multiplier))
         (graded? (mref (opts) 'graded?))
         (grades (mref (opts) 'grades))
         (value-range (mref (opts) 'value-range))
         (legend (mref (opts) 'color-map))
         (test-func (lambda (row-index)
                      (lists:flatten
                       (string:replace
                        (loise-ascii:row #'loise-simplex:point/4
                                         scale-func
                                         row-index
                                         #(0 0)
                                         size
                                         mult
                                         graded?
                                         grades
                                         value-range
                                         legend
                                         (opts))
                        " " "" 'all)))))
    (is-equal "**~~~~~***nn^^^^^nn**nnnnnnn**~~~~~~*************~~~~~~*n"
              (funcall test-func 0))
    (is-equal "~~~**nnn^^^^^n**~~~~~***nn^^A^^^^^nnnnnnn*nn^^^^^n*~~~~**"
              (funcall test-func 18))
    (is-equal "*********nn**~~~~~~~~~~~~~~**n^^^^^nnn**~~~~~*nnnnn***~~~"
              (funcall test-func 36))))

(deftest grid-perlin
  (let* ((opts (tiny-opts))
         (size  (mref (opts) 'size)))
    (is-equal (tiny-grid-perlin)
              (lists:flatten
               (loise-ascii:grid #'loise-perlin:point/4
                                 #(0 0)
                                 size
                                 opts)))))

(deftest grid-simplex
  (let* ((opts (tiny-opts))
         (size  (mref (opts) 'size)))
    (is-equal (tiny-grid-simplex)
              (lists:flatten
               (loise-ascii:grid #'loise-simplex:point/4
                                 #(0 0)
                                 size
                                 opts)))))
