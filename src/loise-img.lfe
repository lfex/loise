(defmodule loise-img
  (export all))

(defun draw-graded-perlin-point! (multiplier grades image width height x y)
  (let* ((value (loise:get-perlin-point `(,x ,y) `(,width ,height) multiplier))
         (adjusted (lutil-math:color-scale value #(-1 1)))
         (graded (lutil-math:get-closest adjusted grades)))
        (draw-point!
          image x y
          `#(,graded ,graded ,graded))))

(defun draw-perlin-point! (multiplier image width height x y)
  (let* ((value (loise:get-perlin-point `(,x ,y) `(,width ,height) multiplier))
         (adjusted (lutil-math:color-scale value #(-1 1))))
        (draw-point!
          image x y
          `#(,adjusted ,adjusted ,adjusted))))

(defun draw-graded-simplex-point! (multiplier grades image width height x y)
  (let* ((value (loise:get-simplex-point `(,x ,y) `(,width ,height) multiplier))
         (adjusted (lutil-math:color-scale value #(-1 1)))
         (graded (lutil-math:get-closest adjusted grades)))
        (draw-point!
          image x y
          `#(,graded ,graded ,graded))))

(defun draw-simplex-point! (multiplier image width height x y)
  (let* ((value (loise:get-simplex-point `(,x ,y) `(,width ,height) multiplier))
         (adjusted (lutil-math:color-scale value #(-1 1))))
        (draw-point!
          image x y
          `#(,adjusted ,adjusted ,adjusted))))

(defun draw-point! (image x y color-tuple)
  "egd doesn't have a function for drawing just a point.

  This has got to be an incredibly inefficient function; please don't treat
  like anything othat that what this is: A toy."
  (egd:line image `#(,x ,y) `#(,x ,y) (egd:color color-tuple)))

(defun build-image (width height func)
  "Builds an image of the specified size and shape by calling the specified
  function on the coordinates of each pixel.

  The function takes an x and y coordinate as agument and returns an x y
  coordinate as well as an egd color value.

  Based on the Racket function defined here:
    http://docs.racket-lang.org/picturing-programs/#%28def._%28%28lib._picturing-programs/private/map-image..rkt%29._build-image%29%29"
   (let ((image (egd:create width height)))
     (list-comp ((<- x (lists:seq 0 width))
                 (<- y (lists:seq 0 height)))
                (funcall func (list image width height x y)))
     image))

(defun write (image filename filetype)
  "Write the image data.

  image is an egd image type
  filename is a string value
  filetype is an atom, e.g., 'png"
  (egd:save
    (egd:render image filetype)
    filename))

(defun create (filename filetype width height func)
  "A wrapper function for 'build-image' and 'write'."
  (let ((image (build-image width height func)))
    (write image filename filetype)))

(defun create-white (filename filetype width height)
  "A convenience function for creating test images."
  (create filename filetype width height
    (lambda (x y)
      (list x y (egd:color (tuple 255 255 255))))))

(defun create-black (filename filetype width height)
  "A convenience function for creating test images."
  (create filename filetype width height
    (lambda (x y)
      (list x y (egd:color 'black)))))

(defun create-perlin (filename filetype)
  (create-perlin filename filetype 256 256 1.0))

(defun create-perlin (filename filetype width height)
  (create-perlin filename filetype width height 1.0))

(defun create-perlin (filename filetype width height multiplier)
  (create filename filetype width height
          (loise-util:partial #'draw-perlin-point!/6 multiplier)))

(defun create-perlin (filename filetype width height multiplier grades)
  (create filename filetype width height
          (loise-util:partial
            #'draw-graded-perlin-point!/7 `(,multiplier ,grades))))

(defun create-simplex (filename filetype)
  (create-simplex filename filetype 256 256))

(defun create-simplex (filename filetype width height)
  (create-simplex filename filetype width height 1.0))

(defun create-simplex (filename filetype width height multiplier)
  (create filename filetype width height
          (loise-util:partial
            #'draw-simplex-point!/6 multiplier)))

(defun create-simplex (filename filetype width height multiplier grades)
  (create filename filetype width height
          (loise-util:partial
            #'draw-graded-simplex-point!/7 `(,multiplier ,grades))))
