(defmodule loise-ascii
  (export all)
  (import
    (from proplists
      (get_value 2))))

;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;; Options
;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

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

;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;; API
;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

(defun perlin ()
  (let ((options (default-options)))
    (print (build-ascii #'perlin-point/3 options) options)))

(defun perlin
  (((= (cons `#(,_ ,_) _) options)) ;; work harder, hello kitty!
   (print (build-ascii #'perlin-point/3 options) options))
  ((filename)
    (let ((options (default-options)))
      (write filename
             (build-ascii #'perlin-point/3 options)
             options))))

(defun perlin (filename options)
  (write filename
         (build-ascii #'perlin-point/3 options)
         options))

(defun simplex ()
  (let ((options (default-options)))
    (print (build-ascii #'simplex-point/3 options) options)))

(defun simplex
  (((= (cons `#(,_ ,_) _) options))
   (print (build-ascii #'simplex-point/3 options) options))
  ((filename)
    (let ((options (default-options)))
      (write filename
             (build-ascii #'simplex-point/3 options)
             options))))

(defun simplex (filename options)
  (write filename
         (build-ascii #'simplex-point/3 options)
         options))

;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;; Aliases, for backwards compatibility
;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

(defun create-perlin () (perlin))
(defun create-perlin (a) (perlin a))
(defun create-perlin (a b) (perlin a b))
(defun create-simplex () (simplex))
(defun create-simplex (a) (simplex a))
(defun create-simplex (a b) (simplex a b))

;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;; Supporting functions
;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

(defun print (data options)
  (io:format "~s~n" `(,(render data options))))

(defun write (filename data options)
  (file:write_file filename (render data options)))

(defun point (x y func options)
  (let* ((value (funcall func
                  `(,x ,y)
                  (loise-util:get-dimensions options)
                  (get_value 'multiplier options)
                  options))
         (adjusted (lutil-math:color-scale value #(-1 1)))
         (graded (lutil-math:get-closest adjusted (get_value 'grades options)))
         (ascii-map (loise-util:get-ascii-map options)))
    `#((,x ,y) ,(get_value graded ascii-map))))

(defun perlin-point (x y options)
  (point x y #'loise-perlin:point/4 options))

(defun simplex-point (x y options)
  (point x y #'loise-simplex:point/4 options))

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
