(defmodule loise-egd
  (export all))

(include-lib "include/options.lfe")

;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;; API
;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

(defun perlin (filename)
  (perlin filename (default-egd-options)))

(defun perlin (filename options)
  (create filename #'draw-perlin-point!/4 options))

(defun simplex (filename)
  (simplex filename (default-egd-options)))

(defun simplex (filename options)
  (create filename #'draw-simplex-point!/4 options))

(defun draw-point! (image x y func options)
  "egd doesn't have a function for drawing just a point.

  This has got to be an incredibly inefficient function; please don't treat
  like anything other than what this is: A toy."
  (let* ((value (funcall func
                  `(,x ,y)
                  (loise-util:get-dimensions options)
                  (proplists:get_value 'multiplier options)
                  options))
         (adjusted (get-graded-point value options)))
    (egd:line
      image
      `#(,x ,y) `#(,x ,y)
      (egd:color `#(,adjusted ,adjusted ,adjusted)))))

(defun draw-perlin-point! (image x y options)
  (draw-point! image x y #'loise:get-perlin-point/4 options))

(defun draw-simplex-point! (image x y options)
  (draw-point! image x y #'loise:get-simplex-point/4 options))

;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;; Supporting functions
;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

(defun create (filename func options)
  "A wrapper function for the 'build-image' and 'write' functions."
  (write
    (build-image func options)
    filename))

(defun write (image filename)
  "Write the image data.

  image is an egd image type
  filename is a string value."
  (egd:save
    (egd:render image
                (get-image-filetype filename))
    filename))

(defun get-graded-point (value options)
  (let ((adjusted (lutil-math:color-scale value #(-1 1)))
        (grades (proplists:get_value 'grades options)))
    (case grades
      ('undefined
        adjusted)
      (_
        (lutil-math:get-closest adjusted grades)))))

(defun get-image-filetype (filename)
  (list_to_atom (cdr (filename:extension filename))))

(defun build-image (func options)
  "Builds an image of the specified size and shape by calling the specified
  function on the coordinates of each pixel.

  The function takes an x and y coordinate as agument and returns an x y
  coordinate as well as an egd color value.

  Based on the Racket function defined here:
    http://docs.racket-lang.org/picturing-programs/#%28def._%28%28lib._picturing-programs/private/map-image..rkt%29._build-image%29%29"
  (let* ((new-opts (++ (loise-util:update-perm-table-options options)
                        (default-options)))
         (width (proplists:get_value 'width new-opts))
         (height (proplists:get_value 'height new-opts))
         (image (egd:create width height)))
     (list-comp ((<- x (lists:seq 0 width))
                 (<- y (lists:seq 0 height)))
                (funcall func image x y new-opts))
     image))
