(defmodule loise-img-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")

(deftest get-point-color-without-grades
  (is-equal 128 (loise-img:get-point-color 0 '())))

(deftest get-point-color-with-grades
  (is-equal
    170.0
    (loise-img:get-point-color
      0.5
      `(#(grades ,(loise-util:get-gradations 4))))))

(deftest get-image-filetype
  (is-equal 'png (loise-img:get-image-filetype "myfile.png"))
  (is-equal 'gif (loise-img:get-image-filetype "myfile.gif"))
  (is-equal 'jpg (loise-img:get-image-filetype "myfile.jpg")))
