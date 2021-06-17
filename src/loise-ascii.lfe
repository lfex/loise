(defmodule loise-ascii
  (export all))

(include-lib "include/options.lfe")

;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;; Options and Defaults
;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

(defun options ()
  (options '()))

(defun options (overrides)
  (let* ((opts (default-ascii-options overrides))
         (grades-count (loise-opts:grades-count opts)))
    (++ `(#(grades ,(loise-util:make-gradations grades-count)))
        (loise-opts:update-perm-table opts))))

(defun cell-separator () " ")
(defun row-separator () "\n")
(defun value-range () #(-1 1))

;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;; API
;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

(defun perlin-grid ()
  (perlin-grid (options)))

(defun perlin-grid (opts)
  (perlin-grid (loise:size opts) opts))

(defun perlin-grid (end-point opts)
  (perlin-grid #(0 0) end-point opts))

(defun perlin-grid (start-point end-point opts)
  (make-grid #'loise-perlin:point/4 start-point end-point opts))

(defun simplex-grid ()
  (simplex-grid (options)))

(defun simplex-grid (opts)
  (simplex-grid (loise:size opts) opts))

(defun simplex-grid (end-point opts)
  (simplex-grid #(0 0) end-point opts))

(defun simplex-grid (start-point end-point opts)
  (make-grid #'loise-simplex:point/4 start-point end-point opts))

(defun grid (grid-type)
  (grid grid-type (options)))

(defun grid (grid-type opts)
  (case grid-type
    ('perlin (perlin-grid opts))
    ('simplex (simplex-grid opts))))

(defun write (filename data)
  (file:write_file filename data))

(defun write-grid (filename grid-type opts)
  (write filename (grid grid-type opts)))

(defun format-grid (grid-type)
  (format-grid grid-type (options)))

(defun format-grid (grid-type opts)
  (io:format "~s~n" (list (grid grid-type opts))))

;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;; Supporting functions
;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

(defun point-data (point-func point max mult grades legend opts)
  (let* ((value (apply point-func (list point max mult opts)))
         (scaled (lutil-math:color-scale value (value-range)))
         (graded (lutil-math:get-closest scaled grades)))
    (proplists:get_value graded legend)))

(defun make-grid
  ((point-func (= `#(,start-x ,start-y) start) (= `#(,end-x ,end-y) end) opts)
   (lists:join
    (row-separator)
    (list-comp ((<- y (lists:seq start-y end-y)))
      (make-row point-func y start end opts)))))

(defun make-row
  ((point-func y `#(,start-x ,_) `#(,end-x ,end-y) opts)
   (let ((legend (loise-opts:color-map opts))
         (mult (loise-opts:multiplier opts))
         (grades (loise-opts:grades opts)))
     (lists:join
      (cell-separator)
      (list-comp ((<- x (lists:seq start-x end-x)))
        (let ((`#(,char ,color) (point-data point-func
                                            `(,x ,y)
                                            `(,end-x ,end-y)
                                            mult
                                            grades
                                            legend
                                            opts)))
          (loise-util:colorize color char opts)))))))
