(defmodule loise-png-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")
(include-lib "include/options.lfe")

(defun tiny-opts ()
  (loise-png:options `(#(width 2)
                       #(height 2))))

(defun point-func (x _ _ opts)
  x)

(defun test-values-1 ()
  (let* ((v1 '(0.001 0.01 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0))
         (v2 (lists:map (lambda (x) (* -1 x)) v1)))
    (++ v1 v2)))

(deftest point-data-without-grades
  (let* ((opts (loise-png:options))
         (dim (loise:dim opts))
         (mult (loise-opts:multiplier opts))
         (grades (loise-opts:grades opts)))
    (is-equal '(128 129 140 153 166 179 191 204 217 230 242 255
                127 126 115 102  89  77  64  51  38  25  13  0)
              (lists:map (lambda (p)
                           (loise-png:point-data #'point-func/4 p dim mult grades opts))
                         (test-values-1)))))

(deftest point-data-with-grades
  (let* ((opts (loise-png:options '(#(graded? true))))
         (dim (loise:dim opts))
         (mult (loise-opts:multiplier opts))
         (grades (loise-opts:grades opts)))
    (is-equal '(153.0 153.0 153.0 153.0 153.0
                204.0 204.0 204.0 204.0
                255.0 255.0 255.0
                102.0 102.0 102.0 102.0 102.0 102.0
                 51.0  51.0  51.0
                  0     0     0)
              (lists:map (lambda (p)
                           (loise-png:point-data #'point-func/4 p dim mult grades opts))
                         (test-values-1)))))

(defmodule loise-png-system-tests
  (behaviour ltest-system)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")

(defun outfile-tmpl () "test/~p-~p.png")

(defun write-images (noise-type multipliers)
  (lists:map (lambda (x)
               (loise-png:write-image
                (io_lib:format (outfile-tmpl) (list noise-type x))
                noise-type
                (loise-png:options `(#(noise ,noise-type) #(multiplier ,x)))))
             multipliers))

(defun get-image-sizes (noise-type multipliers)
  (lists:map (lambda (x)
               (filelib:file_size (io_lib:format (outfile-tmpl) (list noise-type x))))
             multipliers))

(deftest generate-perlin()
  (let ((multipliers '(1 2 4 8 16))
        (noise-type 'perlin))
    (write-images noise-type multipliers)
    (is-equal '(5010 8109 11214 14894 18256)
              (get-image-sizes noise-type multipliers))))

(deftest generate-simplex()
  (let ((multipliers '(1 2 4 8 16))
        (noise-type 'simplex))
    (write-images noise-type multipliers)
    (is-equal '(8833 14218 19670 23208 22549)
              (get-image-sizes noise-type multipliers))))

