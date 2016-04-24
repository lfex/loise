(defmodule loise-ascii
  (export all)
  (import
    (from proplists
      (get_value 2))))

(defun default-options ()
  (++
    `(#(width 56)
      #(height 36)
      #(multiplier 4.0)
      #(grades ,(loise-util:get-gradations 6))
      #(ascii-map ("A" "^" "n" "*" "~" "~"))
      #(colors (,#'color:whiteb/1 ,#'color:yellow/1 ,#'color:green/1
                ,#'color:greenb/1 ,#'color:blue/1 ,#'color:blue/1))
      #(random false)
      #(seed 42))
    (loise-const:base-options)))

(defun get-point (x y func options)
  (let* ((value (funcall func
                  `(,x ,y)
                  (loise-util:get-dimensions options)
                  (get_value 'multiplier options)
                  options))
         (adjusted (lutil-math:color-scale value #(-1 1)))
         (graded (lutil-math:get-closest adjusted (get_value 'grades options)))
         (ascii-map (loise-util:get-ascii-map options)))
    `#((,x ,y) ,(get_value graded ascii-map))))

(defun get-perlin-point (x y options)
  (get-point x y #'loise:get-perlin-point/4 options))

(defun get-simplex-point (x y options)
  (get-point x y #'loise:get-simplex-point/4 options))

(defun build-ascii (func options)
  "Builds an ASCII map of the specified size and shape by calling the specified
  function on the coordinates of each point.

  The function takes an x and y coordinate as agument and returns an x y
  coordinate as well as an egd color value."
  (let ((new-opts (++ (loise-util:update-perm-table-options options)
                      (default-options))))
    (list-comp ((<- x (lists:seq 0 (get_value 'width options)))
                (<- y (lists:seq 0 (get_value 'height options))))
               (funcall func x y new-opts))))

(defun render-row (y data options)
  (let ((color-map (loise-util:get-color-map options)))
    (string:join
      (list-comp ((<- x (lists:seq 0 (get_value 'width options))))
                  (let ((ascii (get_value `(,x ,y) data)))
                    (funcall (get_value ascii color-map) ascii)))
      " ")))

(defun render (data options)
  (string:join
    (list-comp ((<- y (lists:seq 0 (get_value 'height options))))
               (render-row y data options))
    "\n"))

(defun print (data options)
  (io:format "~s~n" `(,(render data options))))

(defun write (filename data options)
  (file:write_file filename (render data options)))

(defun create-perlin ()
  (let ((options (default-options)))
    (print (build-ascii #'get-perlin-point/3 options) options)))

(defun create-perlin
  (((= (cons `#(,_ ,_) _) options)) ;; work harder, hello kitty!
   (print (build-ascii #'get-perlin-point/3 options) options))
  ((filename)
    (let ((options (default-options)))
      (write filename
             (build-ascii #'get-perlin-point/3 options)
             options))))

(defun create-perlin (filename options)
  (write filename
         (build-ascii #'get-perlin-point/3 options)
         options))

(defun create-simplex ()
  (let ((options (default-options)))
    (print (build-ascii #'get-simplex-point/3 options) options)))

(defun create-simplex
  (((= (cons `#(,_ ,_) _) options))
   (print (build-ascii #'get-simplex-point/3 options) options))
  ((filename)
    (let ((options (default-options)))
      (write filename
             (build-ascii #'get-simplex-point/3 options)
             options))))

(defun create-simplex (filename options)
  (write filename
         (build-ascii #'get-simplex-point/3 options)
         options))
