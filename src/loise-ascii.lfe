(defmodule loise-ascii
  (export all))

;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;; Options and Defaults
;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

(defun default-width () 56)
(defun default-height () 36)
(defun default-map () '("A" "^" "n" "*" "~" "~"))
(defun default-colors ()
  '(whiteb
    yellow
    green
    greenb
    blue
    blue))

(defun default-options ()
  (default-options #m()))

(defun default-options (overrides)
  (let ((ascii-opts `#m(output-backend loise
                        output-type loise
                        output-format text
                        width ,(default-width)
                        height ,(default-height)
                        ascii-map ,(default-map)
                        color? false
                        colors ,(default-colors)
                        color-map undefined
                        graded? true)))
    (clj:-> (loise-state:get 'base-opts)
            (maps:merge (loise-state:get 'output-opts))
            (maps:merge ascii-opts)
            (maps:merge overrides)
            (loise-opts:update-calculated))))

(defun options ()
  (options #m()))

(defun options (overrides)
  (case (maps:get 'ascii-opts (loise-state:get) 'undefined)
    ('undefined (let ((opts (default-options overrides)))
                  (loise-state:set 'ascii-opts opts)
                  opts))
    (stored-opts (loise-opts:update-calculated
                  (maps:merge stored-opts overrides)))))

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
   (let ((scale-func (mref opts 'scale-func))
         (mult (mref opts 'multiplier))
         (graded? (mref opts 'graded?))
         (grades (mref opts 'grades))
         (value-range (loise-opts:value-range opts))
         (legend (mref opts 'color-map)))
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