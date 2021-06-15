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
         (grades-count (proplists:get_value 'grades-count opts)))
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
                  (loise-util:get-dimensions opts)
                  (proplists:get_value 'multiplier opts)
                  opts))
         (adjusted (lutil-math:color-scale value #(-1 1)))
         (graded (lutil-math:get-closest
                  adjusted
                  (proplists:get_value 'grades opts)))
         (legend (color-map opts)))
    `#((,x ,y) ,(proplists:get_value graded legend))))

(defun perlin-point (x y opts)
  (point x y #'loise-perlin:point/4 opts))

(defun simplex-point (x y opts)
  (point x y #'loise-simplex:point/4 opts))

;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;; Supporting functions
;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

(defun color-map (opts)
  (lists:zip
    (proplists:get_value 'grades opts)
    (proplists:get_value 'ascii-map opts)))

(defun print (data options)
  (io:format "~s~n" `(,(render data options))))

(defun write (filename data options)
  (file:write_file filename (render data options)))

(defun build-ascii (func options)
  "Builds an ASCII map of the specified size and shape by calling the specified
  function on the coordinates of each point.

  The function takes an x and y coordinate as agument and returns an x y
  coordinate as well as an egd color value."
  (let ((new-opts (++ (loise-util:update-perm-table-options options)
                      (default-options))))
    (list-comp ((<- x (lists:seq 0 (proplists:get_value 'width options)))
                (<- y (lists:seq 0 (proplists:get_value 'height options))))
               (funcall func x y new-opts))))

(defun render-row (y data options)
  (let ((color-map (loise-util:get-color-map options)))
    (string:join
      (list-comp ((<- x (lists:seq 0 (proplists:get_value 'width options))))
                  (let ((ascii (proplists:get_value `(,x ,y) data)))
                    (funcall (proplists:get_value ascii color-map) ascii)))
      " ")))

(defun render (data options)
  (string:join
    (list-comp ((<- y (lists:seq 0 (proplists:get_value 'height options))))
               (render-row y data options))
    "\n"))
