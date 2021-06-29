(defmodule loise-egd
  (export all))

;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;; Options and Defaults
;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

(defun default-egd-width () 256)
(defun default-egd-height () 128)

(defun default-options ()
  (default-options #m()))

(defun default-options (overrides)
  (let ((egd-opts `#m(output-backend egd
                      output-type image
                      output-format png
                      width ,(default-egd-width)
                      height ,(default-egd-height))))
    (clj:-> (loise-state:get 'base-opts)
            (maps:merge (loise-state:get 'output-opts))
            (maps:merge egd-opts)
            (maps:merge overrides)
            (loise-opts:update-calculated-opts))))

(defun options ()
  (options #m()))

(defun options (overrides)
  (case (maps:get 'egd-opts (loise-state:get) 'undefined)
    ('undefined  (default-options overrides))
    (stored-opts stored-opts)))

;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;; API
;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

(defun perlin (filename)
  (perlin filename (options)))

(defun perlin (filename opts)
  (create filename #'draw-perlin-point!/4 opts))

(defun simplex (filename)
  (simplex filename (options)))

(defun simplex (filename opts)
  (create filename #'draw-simplex-point!/4 opts))

(defun draw-point! (image x y func opts)
  "egd doesn't have a function for drawing just a point.

  This has got to be an incredibly inefficient function; please don't treat
  like anything other than what this is: A toy."
  (let* ((value (funcall func
                  `(,x ,y)
                  (mref opts 'dimensions)
                  (mref opts 'multiplier)
                  opts))
         (adjusted (get-graded-point value opts)))
    (egd:line
      image
      `#(,x ,y) `#(,x ,y)
      (egd:color `#(,adjusted ,adjusted ,adjusted)))))

(defun draw-perlin-point! (image x y opts)
  (draw-point! image x y #'loise:get-perlin-point/4 opts))

(defun draw-simplex-point! (image x y opts)
  (draw-point! image x y #'loise:get-simplex-point/4 opts))

(defun write-image (filename opts)
  (case (mref opts 'noise)
    ('perlin (perlin filename opts))
    ('simplex (simplex filename opts))))
  
;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;; Supporting functions
;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

(defun create (filename func opts)
  "A wrapper function for the 'build-image' and 'write' functions."
  (write
    (build-image func opts)
    filename))

(defun write (image filename)
  "Write the image data.

  image is an egd image type
  filename is a string value."
  (egd:save
    (egd:render image
                (get-image-filetype filename))
    filename))

(defun get-graded-point (value opts)
  (let ((adjusted (lutil-math:color-scale value #(-1 1)))
        (grades (mref opts 'grades)))
    (case grades
      ('undefined
        adjusted)
      (_
        (lutil-math:get-closest adjusted grades)))))

(defun get-image-filetype (filename)
  (list_to_atom (cdr (filename:extension filename))))

(defun build-image (func opts)
  "Builds an image of the specified size and shape by calling the specified
  function on the coordinates of each pixel.

  The function takes an x and y coordinate as agument and returns an x y
  coordinate as well as an egd color value.

  Based on the Racket function defined here:
    http://docs.racket-lang.org/picturing-programs/#%28def._%28%28lib._picturing-programs/private/map-image..rkt%29._build-image%29%29"
  (let* ((width (loise-opts:get 'width opts))
         (height (loise-opts:get 'height opts))
         (image (egd:create width height)))
     (list-comp ((<- x (lists:seq 0 width))
                 (<- y (lists:seq 0 height)))
                (funcall func image x y opts))
     image))
