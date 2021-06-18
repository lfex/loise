(defmodule loise-ascii-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")
(include-lib "include/options.lfe")

;; XXX fix this up to use override args to options
(defun tiny-opts ()
  (++ `(#(width 2)
        #(height 2))
      (loise-ascii:options)))

(defun tiny-grid-perlin ()
  (++ "* * *" "\n"
      "* * *" "\n"
      "* * *"))

(defun tiny-grid-simplex ()
  (++ "* * n" "\n"
      "~ A *" "\n"
      "* * ~"))

(deftest grades
  (let ((opts (loise-ascii:options)))
    (is-equal 6 (loise-opts:grades-count opts))
    (is-equal '(0 51.0 102.0 153.0 204.0 255.0)
              (loise-opts:grades opts))
    (is-equal '("A" "^" "n" "*" "~" "~")
              (loise-opts:ascii-map opts))
    (is-equal '(#(0 #("A" whiteb))
                #(51.0 #("^" yellow))
                #(102.0 #("n" green))
                #(153.0 #("*" greenb))
                #(204.0 #("~" blue))
                #(255.0 #("~" blue)))
              (loise-opts:color-map opts))))

(deftest color-map
  (is-equal
   '(#(0 #("A" whiteb))
     #(51.0 #("^" yellow))
     #(102.0 #("n" green))
     #(153.0 #("*" greenb))
     #(204.0 #("~" blue))
     #(255.0 #("~" blue)))
    (loise-opts:color-map (loise-ascii:options))))

(deftest point-data-perlin
  (let* ((opts (loise-ascii:options))
         (dim (loise-opts:dimensions opts))
         (mult (loise-opts:multiplier opts))
         (grades (loise-opts:grades opts))
         (legend (loise-opts:color-map opts))
         (test-func (lambda (x) (loise-ascii:point-data
                                 #'loise-perlin:point/4
                                 x
                                 dim
                                 mult
                                 grades
                                 legend
                                 opts))))
    (is-equal #("*" greenb) (funcall test-func (0 0)))
    (is-equal #("~" blue) (funcall test-func (5 3)))
    (is-equal #("n" green) (funcall test-func (10 10)))
    (is-equal #("n" green) (funcall test-func (10 10)))
    (is-equal #("^" yellow) (funcall test-func (8 16)))))

(deftest point-data-simplex
  (let* ((opts (loise-ascii:options))
         (dim (loise-opts:dimensions opts))
         (mult (loise-opts:multiplier opts))
         (grades (loise-opts:grades opts))
         (legend (loise-opts:color-map opts))
         (test-func (lambda (x) (loise-ascii:point-data
                                 #'loise-simplex:point/4
                                 x
                                 dim
                                 mult
                                 grades
                                 legend
                                 opts))))
    (is-equal #("*" greenb) (funcall test-func (0 0)))
    (is-equal #("~" blue) (funcall test-func (3 3)))
    (is-equal #("n" green) (funcall test-func (42 15)))
    (is-equal #("^" yellow) (funcall test-func (15 0)))
    (is-equal #("A" whiteb) (funcall test-func (14 10)))))

(deftest make-row-perlin
  (let* ((opts (loise-ascii:options))
         (size (loise-opts:size opts))
         (test-func (lambda (row-index)
                      (lists:flatten
                       (string:replace
                        (loise-ascii:make-row #'loise-perlin:point/4
                                              row-index
                                              #(0 0)
                                              size
                                              opts)
                        " " "" 'all)))))
    (is-equal "*****~~~~~*****nnnn^^^^^nnnn*****************************"
              (funcall test-func 0))
    (is-equal "*nnnn^^^^^nnnn***************nnnnnnnnnnnnn********nnnnnn*"
              (funcall test-func 18))
    (is-equal "*nnnnnnnnnnnnn*******************************************"
              (funcall test-func 36))))

(deftest make-row-simplex
  (let* ((opts (loise-ascii:options))
         (size (loise-opts:size opts))
         (test-func (lambda (row-index)
                      (lists:flatten
                       (string:replace
                        (loise-ascii:make-row #'loise-simplex:point/4
                                              row-index
                                              #(0 0)
                                              size
                                              opts)
                        " " "" 'all)))))
    (is-equal "**~~~~~***nn^^^^^nn**nnnnnnn**~~~~~~*************~~~~~~*n"
              (funcall test-func 0))
    (is-equal "~~~**nnn^^^^^n**~~~~~***nn^^A^^^^^nnnnnnn*nn^^^^^n*~~~~**"
              (funcall test-func 18))
    (is-equal "*********nn**~~~~~~~~~~~~~~**n^^^^^nnn**~~~~~*nnnnn***~~~"
              (funcall test-func 36))))

(deftest make-grid-perlin
  (let* ((opts (tiny-opts))
         (size (loise-opts:size opts)))
    (is-equal (tiny-grid-perlin)
              (lists:flatten
               (loise-ascii:make-grid #'loise-perlin:point/4
                                      #(0 0)
                                      size
                                      opts)))))

(deftest make-grid-simplex
  (let* ((opts (tiny-opts))
         (size (loise-opts:size opts)))
    (is-equal (tiny-grid-simplex)
              (lists:flatten
               (loise-ascii:make-grid #'loise-simplex:point/4
                                      #(0 0)
                                      size
                                      opts)))))
