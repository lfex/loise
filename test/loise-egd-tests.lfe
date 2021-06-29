(defmodule loise-egd-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")

(deftest get-point-color-without-grades
  (is-equal 128 (loise-egd:get-graded-point 0 '())))

(deftest get-point-color-with-grades
  (is-equal
    170.0
    (loise-egd:get-graded-point
      0.5
      `(#(grades ,(loise-util:make-gradations 4))))))

(deftest get-image-filetype
  (is-equal 'png (loise-egd:get-image-filetype "myfile.png"))
  (is-equal 'gif (loise-egd:get-image-filetype "myfile.gif"))
  (is-equal 'jpg (loise-egd:get-image-filetype "myfile.jpg")))

(deftest dimensions
  (is-equal '(256 128) (mref (loise-egd:default-options) 'size)))
