(defmodule loise-ascii-tests
  (behaviour ltest-system)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")

(defun set-up () (loise-tests-support:set-up))
(defun tear-down (setup-result) (loise-tests-support:tear-down setup-result))

(defun opts()
  (opts #m()))

(defun opts (overrides)
  (loise-ascii:default-options overrides))

(defun tiny-opts ()
  (opts #m(width 2 height 2)))

(defun tiny-opts (overrides)
  (opts (maps:merge #m(width 2 height 2) overrides)))

(defun tiny-grid-perlin ()
  (++ "* * *" "\n"
      "* * *" "\n"
      "* * *"))

(defun tiny-grid-simplex ()
  (++ "* * n" "\n"
      "~ A *" "\n"
      "* * ~"))

(deftestcase dimensions (setup-result)
  (tuple "dimensions"
         (is-equal '(56 36) (loise:dim (opts)))))

(deftestcase grades (setup-result)
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

(deftestcase color-map (setup-result)
  (is-equal
   '(#(0 #("A" whiteb))
     #(51.0 #("^" yellow))
     #(102.0 #("n" green))
     #(153.0 #("*" greenb))
     #(204.0 #("~" blue))
     #(255.0 #("~" blue)))
   (mref (opts) 'color-map)))

(deftestcase point-perlin (setup-result)
  (let* ((scale-func (mref (opts) 'scale-func))
         (dim (mref (opts) 'dim))
         (mult (mref (opts) 'multiplier))
         (graded? (mref (opts) 'graded?))
         (grades (mref (opts) 'grades))
         (value-range (loise-opts:value-range (opts)))
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

(deftestcase point-simplex (setup-result)
  (let* ((scale-func (mref (opts) 'scale-func))
         (dim (mref (opts) 'dim))
         (mult (mref (opts) 'multiplier))
         (graded? (mref (opts) 'graded?))
         (grades (mref (opts) 'grades))
         (value-range (loise-opts:value-range (opts)))
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

(deftestcase row-perlin (setup-result)
  (let* ((scale-func (mref (opts) 'scale-func))
         (size (mref (opts) 'size))
         (mult (mref (opts) 'multiplier))
         (graded? (mref (opts) 'graded?))
         (grades (mref (opts) 'grades))
         (value-range (loise-opts:value-range (opts)))
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

(deftestcase row-simplex (setup-result)
  (let* ((scale-func (mref (opts) 'scale-func))
         (size (mref (opts) 'size))
         (mult (mref (opts) 'multiplier))
         (graded? (mref (opts) 'graded?))
         (grades (mref (opts) 'grades))
         (value-range (loise-opts:value-range (opts)))
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

(deftestcase grid-perlin (setup-result)
  (let* ((opts (tiny-opts #m(data-format matrix)))
         (size  (mref opts 'size)))
    (is-equal (tiny-grid-perlin)
              (lists:flatten
               (loise-ascii:grid #'loise-perlin:point/4
                                 #(0 0)
                                 size
                                 opts)))))

(deftestcase grid-simplex (setup-result)
  (let* ((opts (tiny-opts #m(data-format matrix)))
         (size  (mref opts 'size)))
    (is-equal (tiny-grid-simplex)
              (lists:flatten
               (loise-ascii:grid #'loise-simplex:point/4
                                 #(0 0)
                                 size
                                 opts)))))

(deftestgen suite
  (tuple 'foreach
         (defsetup set-up)
         (defteardown tear-down)
         (deftestcases
           dimensions
           grades
           color-map
           point-perlin
           point-simplex
           row-perlin
           row-simplex
           grid-perlin
           grid-simplex)))
