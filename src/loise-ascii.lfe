(defmodule loise-ascii
  (export all))

(include-lib "include/options.lfe")

;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;; Options
;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

(defun options ()
  (options '()))

(defun options (overrides)
  (let* ((opts (default-ascii-options overrides))
         (grades-count (loise-opts:grades-count opts)))
    (++ `(#(grades ,(loise-util:make-gradations grades-count)))
        opts)))

;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;; API
;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

(defun perlin ()
  (let ((opts (options)))
    (print (build-ascii #'perlin-point/3 opts) opts)))

(defun perlin
  (((= (cons `#(,_ ,_) _) opts))
   (print (build-ascii #'perlin-point/3 opts) opts))
  ((filename)
    (let ((opts (options)))
      (write filename
             (build-ascii #'perlin-point/3 opts)
             opts))))

(defun perlin (filename opts)
  (write filename
         (build-ascii #'perlin-point/3 opts)
         opts))

(defun simplex ()
  (let ((opts (options)))
    (print (build-ascii #'simplex-point/3 opts) opts)))

(defun simplex
  (((= (cons `#(,_ ,_) _) opts))
   (print (build-ascii #'simplex-point/3 opts) opts))
  ((filename)
    (let ((opts (options)))
      (write filename
             (build-ascii #'simplex-point/3 opts)
             opts))))

(defun simplex (filename opts)
  (write filename
         (build-ascii #'simplex-point/3 opts)
         opts))

(defun point (x y func opts)
  (let* ((value (funcall func
                  `(,x ,y)
                  (loise-opts:dimensions opts)
                  (loise-opts:multiplier opts)
                  opts))
         (adjusted (lutil-math:color-scale value #(-1 1)))
         (graded (lutil-math:get-closest
                  adjusted
                  (loise-opts:grades opts)))
         (legend (loise-opts:color-map opts)))
    `#((,x ,y) ,(proplists:get_value graded legend))))

(defun perlin-point (x y opts)
  (point x y #'loise-perlin:point/4 opts))

(defun simplex-point (x y opts)
  (point x y #'loise-simplex:point/4 opts))

;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;; Supporting functions
;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

(defun get-color (colors key)
  (proplists:get_value key colors))

(defun print (data opts)
  (io:format "~s~n" `(,(render data opts))))

(defun write (filename data opts)
  (file:write_file filename (render data opts)))

(defun build-ascii (func opts)
  "Builds an ASCII map of the specified size and shape by calling the specified
  function on the coordinates of each point.

  The function takes an x and y coordinate as agument and returns an x y
  coordinate as well as an egd color value."
  (let ((new-opts (loise-opts:update-perm-table opts)))
    (list-comp ((<- x (lists:seq 0 (loise-opts:width opts)))
                (<- y (lists:seq 0 (loise-opts:height opts))))
      (funcall func x y new-opts))))

(defun render-row (y data opts)
  (let ((colors (loise-opts:color-map opts))
        (separator " "))
    (string:join
     (list-comp ((<- x (lists:seq 0 (loise-opts:width opts))))
       (let* ((char (proplists:get_value `(,x ,y) data))
              (color (get-color colors char)))
         (loise-util:colorize color char)))
     separator)))

(defun render (data opts)
  (let ((separator "\n"))
    (string:join
     (list-comp ((<- y (lists:seq 0 (loise-opts:height opts))))
       (render-row y data opts))
     separator)))
