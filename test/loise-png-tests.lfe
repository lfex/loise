(defmodule loise-png-tests
  (behaviour ltest-system)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")

(defun opts ()
  (loise-png:default-options))

(defun tiny-opts ()
  (loise-png:default-options #m(width 4 height 4)))

(defun graded-opts ()
  (loise-png:default-options #m(width 4 height 4 graded? true)))

(defun point-func
  ((`(,x ,y) `(,max-x ,max-y) mult _)
   (/ (+ (/ x max-x) (/ y max-y)) 2)))

(defun test-values-1 ()
  (let* ((v1 '(0.001 0.01 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0))
         (v2 (lists:map (lambda (x) (* -1 x)) v1)))
    (++ v1 v2)))

(defun outfile-tmpl () "test/~p-~p.png")

(defun write-images (noise-type multipliers)
  (lists:map (lambda (x)
               (loise-png:write-image
                (io_lib:format (outfile-tmpl) (list noise-type x))
                noise-type
                (loise-png:options `#m(noise ,noise-type multiplier ,x))))
             multipliers))

(defun get-image-sizes (noise-type multipliers)
  (lists:map (lambda (x)
               (filelib:file_size (io_lib:format (outfile-tmpl) (list noise-type x))))
             multipliers))

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

(deftestcase generate-perlin (setup-result)
  (tuple "generate-perlin"
         (let ((multipliers '(1 2 4 8 16))
               (noise-type 'perlin))
           (write-images noise-type multipliers)
           (is-equal '(5010 8109 11214 14894 18256)
                     (get-image-sizes noise-type multipliers)))))

(deftestcase generate-simplex (setup-result)
  (tuple "generate-simplex"
         (let ((multipliers '(1 2 4 8 16))
               (noise-type 'simplex))
           (write-images noise-type multipliers)
           (is-equal '(8833 14218 19670 23208 22549)
                     (get-image-sizes noise-type multipliers)))))

(deftestcase row-with-grades (setup-result)
  (tuple "row-with-grades"
         (let* ((opts (graded-opts))
                (scale-func (mref opts 'scale-func))
                (dim (mref opts 'dim))
                (mult (mref opts 'multiplier))
                (graded? (mref opts 'graded?))
                (grades (mref opts 'grades))
                (value-range (loise-opts:value-range opts))
                (size (mref opts 'size)))
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
                                    opts)))))

(deftestcase row-without-grades (setup-result)
  (tuple "row-without-grades"
         (let* ((opts (tiny-opts))
                (scale-func (mref opts 'scale-func))
                (dim (mref opts 'dim))
                (mult (mref opts 'multiplier))
                (graded? (mref opts 'graded?))
                (grades (mref opts 'grades))
                (value-range (loise-opts:value-range opts))
                (size (mref opts 'size)))
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
                                    opts)))))

(deftestgen suite
  (tuple 'foreach
         (defsetup set-up)
         (defteardown tear-down)
         (deftestcases
           dimensions
           generate-perlin
           generate-simplex
           row-with-grades
           row-without-grades)))
