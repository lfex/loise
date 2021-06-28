(defmodule loise-png-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")
(include-lib "include/options.lfe")

(defun tiny-opts ()
  (loise-png:options `(#(width 4)
                       #(height 4))))

(defun point-func
  ((`(,x ,y) `(,max-x ,max-y) mult _)
   (/ (+ (/ x max-x) (/ y max-y)) 2)))

(defun test-values-1 ()
  (let* ((v1 '(0.001 0.01 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0))
         (v2 (lists:map (lambda (x) (* -1 x)) v1)))
    (++ v1 v2)))

(deftest dimensions
  (is-equal '(256 128) (loise-opts:dimensions (loise-png:default-options))))

(deftest row-without-grades
  (let* ((opts (tiny-opts))
         (scale-func (loise-opts:scale-func opts))
         (mult (loise-opts:multiplier opts))
         (graded? (loise-opts:graded? opts))
         (grades (loise-opts:grades opts))
         (value-range (loise-opts:value-range opts))
         (size (loise-opts:size opts)))
    (is-equal '(143 159 175 191 207)
              (loise-png:row #'point-func/4
                             scale-func
                             1
                             #(0 0)
                             size
                             mult
                             graded?
                             grades
                             value-range
                             opts))))


(deftest row-with-grades
  (let* ((opts (cons #(graded? true) (tiny-opts)))
         (scale-func (loise-opts:scale-func opts))
         (mult (loise-opts:multiplier opts))
         (graded? (loise-opts:graded? opts))
         (grades (loise-opts:grades opts))
         (value-range (loise-opts:value-range opts))
         (size (loise-opts:size opts)))
    (is-equal '(153.0 153.0 153.0 204.0 204.0)
              (loise-png:row #'point-func/4
                             scale-func
                             1
                             #(0 0)
                             size
                             mult
                             graded?
                             grades
                             value-range
                             opts))))

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

