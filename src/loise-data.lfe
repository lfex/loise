(defmodule loise-data
  (export all))

(include-lib "include/options.lfe")

;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;; Options and Defaults
;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

(defun options ()
  (options '()))

(defun options (overrides)
  (let ((opts (++ `(#(scale-func ,#'loise-util:identity/1)
                    #(format flat))
                  (default-options overrides))))
    (loise-opts:update-perm-table opts)))

;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;; API
;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

(defun perlin-matrix ()
  (perlin-matrix (options)))

(defun perlin-matrix (opts)
  (perlin-matrix (loise:size opts) opts))

(defun perlin-matrix (end-point opts)
  (perlin-matrix #(0 0) end-point opts))

(defun perlin-matrix (start-point end-point opts)
  (data #'loise-perlin:point/4 start-point end-point opts))

(defun simplex-matrix ()
  (simplex-matrix (options)))

(defun simplex-matrix (opts)
  (simplex-matrix (loise:size opts) opts))

(defun simplex-matrix (end-point opts)
  (simplex-matrix #(0 0) end-point opts))

(defun simplex-matrix (start-point end-point opts)
  (data #'loise-simplex:point/4 start-point end-point opts))

(defun matrix (matrix-type)
  (matrix matrix-type (options)))

(defun matrix (matrix-type opts)
  (case matrix-type
    ('perlin (perlin-matrix opts))
    ('simplex (simplex-matrix opts))))

;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;; Supporting functions
;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

(defun data (point-func start end opts)
  (case (loise-opts:data-format opts)
    ('flat (flat point-func start end opts))
    ('matrix (matrix point-func start end opts))
    (format `#(error (lists:flatten "Unknown data format: ~p" (lists format))))))

(defun matrix
  ((point-func (= `#(,start-x ,start-y) start) (= `#(,end-x ,end-y) end) opts)
   (let ((scale-func (loise-opts:scale-func opts))
         (mult (loise-opts:multiplier opts))
         (graded? (loise-opts:graded? opts))
         (grades (loise-opts:grades opts))
         (value-range (loise-opts:value-range opts)))
     (list-comp ((<- y (lists:seq start-y end-y)))
       (tuple y
              (row point-func scale-func y start end mult graded? grades value-range opts))))))

(defun flat (point-func start end opts)
  (lists:flatten (matrix point-func start end opts)))

(defun row
  ((point-func scale-func y `#(,start-x ,_) `#(,end-x ,end-y) mult graded? grades value-range opts)
   (let ((legend (loise-opts:color-map opts)))
     (list-comp ((<- x (lists:seq start-x end-x)))
       (tuple `(,x ,y)
              (cell point-func
                    scale-func
                    ;; XXX why change from tuple to list here?
                    `(,x ,y)
                    `(,end-x ,end-y)
                    mult
                    graded?
                    grades
                    value-range
                    opts))))))

(defun cell (point-func scale-func point max mult graded? grades value-range opts)
  (let* ((value (apply point-func (list point max mult opts)))
         (scaled (apply scale-func (list value value-range))))
    (if graded?
      (lutil-math:get-closest scaled grades)
      scaled)))
