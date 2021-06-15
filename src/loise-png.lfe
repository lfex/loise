(defmodule loise-png
  (export all))

(include-lib "include/options.lfe")

;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;; API
;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

(defun perlin (filename)
  (perlin filename (default-png-options)))

(defun perlin (filename opts)
  (write filename #'loise-perlin:point/4 opts))

(defun simplex (filename)
  (simplex filename (default-png-options)))

(defun simplex (filename opts)
  (write filename #'loise-simplex:point/4 opts))

(defun point (x y dim multiplier func opts)
  (let* ((value (funcall func
                         `(,x ,y)
                         dim
                         multiplier
                         opts))
         (adjusted (get-graded-point value opts)))
    ;; XXX create point entry as expected for png lib
    ;;(egd:line
    ;; image
    ;; `#(,x ,y) `#(,x ,y)
    ;; (egd:color `#(,adjusted ,adjusted ,adjusted)))))
    'tbd))

(defun row (y func opts)
  (let ((dim (loise-opts:dim opts))
        (mult (loise-opts:multiplier opts)))
    `#(row ,(list-comp ((<- x (lists:seq 0 (loise-opts:width opts))))
              (point x y dim mult func opts)))))

(defun rows (func opts)
  (list-comp ((<- y (lists:seq 0 (loise-opts:height opts))))
    (row y func opts)))

(defun write (filename point-func opts)
  "Write a generated image to a given file.

  filename is a string value."
  (let ((img-data (rows point-func opts)))
    ;; XXX create config
    ;; XXX create png object
    ;; XXX append rows
    ;; XXX close files/objects
    'tbd))

;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;; Supporting functions
;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

(defun generate-config (opts)
  'tbd)

(defun get-graded-point (value options)
  (let ((adjusted (lutil-math:color-scale value #(-1 1)))
        (grades (loise-opts:grades options)))
    (case grades
      ('undefined
        adjusted)
      (_
        (lutil-math:get-closest adjusted grades)))))

;;(defun build-image (func opts)
;;  "Builds an image of the specified size and shape by calling the specified
;;  function on the coordinates of each pixel.
;;
;;  The function takes an x and y coordinate as agument and returns an x y
;;  coordinate as well as an egd color value.
;;
;;  Based on the Racket function defined here:
;;    http://docs.racket-lang.org/picturing-programs/#%28def._%28%28lib._picturing-programs/private/map-image..rkt%29._build-image%29%29"
;;  (let* ((width (proplists:get_value 'width opts))
;;         (height (proplists:get_value 'height opts))
;;         (image (egd:create width height)))
;;    (list-comp ((<- x (lists:seq 0 width))
;;                (<- y (lists:seq 0 height)))
;;      (funcall func image x y opts))
;;    image))
