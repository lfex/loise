(defmodule loise-ascii
  (export all)
  (import
    (from proplists
      (get_value 2))
    (from loise-util
      (get-perlin-for-point 3)
      (get-simplex-for-point 3))))

(defun get-default-options ()
  `(#(width 32)
    #(height 16)
    #(multiplier 1.0)
    #(grades ,(loise-util:get-gradations 6))
    #(ascii-map ("A" "^" "n" "*" "." "~"))
    #(colors '())
    #(random false)
    #(seed 43)))

(defun get-ascii-map (options)
  (lists:zip
    (get_value 'grades options)
    (get_value 'ascii-map options)))

(defun get-dimensions (options)
  `(,(get_value 'width options)
    ,(get_value 'height options)))

(defun get-point (x y func options)
  (let* ((value (funcall func
                  `(,x ,y)
                  (get-dimensions options)
                  (get_value 'multiplier options)))
         (adjusted (lutil-math:color-scale value #(-1 1)))
         (graded (lutil-math:get-closest adjusted (get_value 'grades options)))
         (ascii-map (get-ascii-map options)))
    `(,x ,y ,(get_value graded ascii-map))))

(defun get-perlin-point (x y options)
  (get-point x y #'get-perlin-for-point/3 options))

(defun get-simplex-point (x y options)
  (get-point x y #'get-simplex-for-point/3 options))

(defun build-ascii (func options)
  "Builds an ASCII map of the specified size and shape by calling the specified
  function on the coordinates of each point.

  The function takes an x and y coordinate as agument and returns an x y
  coordinate as well as an egd color value."
  (list-comp ((<- x (lists:seq 0 (get_value 'width options)))
              (<- y (lists:seq 0 (get_value 'height options))))
             (funcall func x y options)))

(defun write (data filename)
  )

(defun print (data)
  )

(defun create-perlin ()
  (print (build-ascii #'get-perlin-point/3 (get-default-options))))

(defun create-perlin
  ((filename) (when (is_list filename))
   (write filename (build-ascii #'get-perlin-point/3 (get-default-options))))
  ((options)
   (print (build-ascii #'get-perlin-point/3 options))))

(defun create-perlin (filename options)
  )

(defun create-simplex ()
  )

(defun create-simplex
  ((filename) (when (is_list filename))
   )
  ((options)
   ))

(defun create-simplex (filename options)
  )
