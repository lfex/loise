(defmodule loise-egd-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")

(deftest get-point-color-without-grades
  (is-equal 128 (loise-egd:get-graded-point 0 #m())))

(deftest get-point-color-with-grades
  (let* ((value 0.5)
         (adjusted (lutil-math:color-scale value (loise-perlin:value-range)))
         (grades (loise-util:make-gradations 4)))
    (is-equal 191 adjusted)
    (is-equal '(0 85.0 170.0 255.0) grades)
    (is-equal
     170.0
     (loise-egd:get-graded-point
      0.5
      #m(graded? true
         grades ,grades)))))

(deftest get-image-filetype
  (is-equal 'png (loise-egd:get-image-filetype "myfile.png"))
  (is-equal 'gif (loise-egd:get-image-filetype "myfile.gif"))
  (is-equal 'jpg (loise-egd:get-image-filetype "myfile.jpg")))

(defmodule loise-egd-system-tests
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

(deftestcase dimensions (setup-result)
  (tuple "dimensions"
         (is-equal '(256 128) (mref (opts) 'dim))))

(deftestgen suite
  (tuple 'foreach
         (defsetup set-up)
         (defteardown tear-down)
         (deftestcases
           dimensions
           dimensions)))
