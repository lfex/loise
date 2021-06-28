(defmodule loise-ascii
  (export all))

(include-lib "loise/include/options.lfe")

;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;; Options and Defaults
;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

(defun default-ascii-width () 56)
(defun default-ascii-height () 36)
(defun default-ascii-map () '("A" "^" "n" "*" "~" "~"))
(defun default-ascii-colors ()
  '(whiteb
    yellow
    green
    greenb
    blue
    blue))

(defun default-options ()
  (default-options '()))

(defun default-options (overrides)
  (++ overrides
      `(#(output-backend loise)
        #(output-type ascii)
        #(output-format text)
        #(width ,(default-ascii-width))
        #(height ,(default-ascii-height))
        #(ascii-map ,(default-ascii-map))
        #(color false)
        #(colors ,(default-ascii-colors))
        #(graded? true))
      (default-output-options)
      (default-base-options)))

(defun options ()
  (options '()))

(defun options (overrides)
  (let* ((opts (default-options overrides))
         (grades-count (loise-opts:grades-count opts)))
    (++ `(#(grades ,(loise-util:make-gradations grades-count)))
        (loise-opts:update-perm-table opts))))

(defun cell-separator () " ")
(defun row-separator () "\n")

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
  (grid #'loise-perlin:point/4 start-point end-point opts))

(defun simplex-grid ()
  (simplex-grid (options)))

(defun simplex-grid (opts)
  (simplex-grid (loise:size opts) opts))

(defun simplex-grid (end-point opts)
  (simplex-grid #(0 0) end-point opts))

(defun simplex-grid (start-point end-point opts)
  (grid #'loise-simplex:point/4 start-point end-point opts))

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

(defun grid
  ((point-func (= `#(,start-x ,start-y) start) (= `#(,end-x ,end-y) end) opts)
   (let ((scale-func (loise-opts:scale-func opts))
         (mult (loise-opts:multiplier opts))
         (graded? (loise-opts:graded? opts))
         (grades (loise-opts:grades opts))
         (value-range (loise-opts:value-range opts))
         (legend (loise-opts:color-map opts)))
     (lists:join
      (row-separator)
      (list-comp ((<- y (lists:seq start-y end-y)))
        (row point-func scale-func y start end mult graded? grades value-range legend opts))))))

(defun row
  ((point-func scale-func y `#(,start-x ,_) `#(,end-x ,end-y) mult graded? grades value-range legend opts)
     (lists:join
      (cell-separator)
      (list-comp ((<- x (lists:seq start-x end-x)))
        (let ((`#(,char ,color) (point point-func
                                       scale-func
                                       ;; XXX why change from tuple to list here?
                                       `(,x ,y)
                                       `(,end-x ,end-y)
                                       mult
                                       graded?
                                       grades
                                       value-range
                                       legend
                                       opts)))
          (loise-util:colorize color char opts))))))

(defun point (point-func scale-func point max mult graded? grades value-range legend opts)
  (let ((graded (loise-data:cell point-func
                                 scale-func
                                 point
                                 max
                                 mult
                                 graded?
                                 grades
                                 value-range
                                 opts)))
    (proplists:get_value graded legend)))