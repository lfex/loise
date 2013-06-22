(defmodule loise-img
  (export all)
  (import
    (from lfe-utils
      (color-scale 2))
    (from loise
      (perlin 1) (perlin 2) (perlin 3)
      (simplex 1) (simplex 2) (simplex 3))
    (from loise-util
      (get-perlin-for-point 3)
      (get-simplex-for-point 3))
    (from lists
      (foreach 2)
      (seq 2))))

(defun draw-point (image x y color)
  "
  egd doesn't have a function for drawing just a point.

  This has got to be an incredibly inefficient function; please don't treat
  like anything othat that what this is: A toy.
  "
  (: egd line image (tuple x y) (tuple x y) color))

(defun process-pixel (image func x-in y-in)
  "
  Call the passed function to get a color value, and then draw that color at
  the given point.
  "
  (let* (((list x-out y-out color) (funcall func x-in y-in)))
    (draw-point image x-out y-out color)))

(defun build-image (width height func)
  "
  Builds an image of the specified size and shape by calling the specified
  function on the coordinates of each pixel.

  The function takes an x and y coordinate as agument and returns an x y
  coordinate as well as an egd color value.

  Based on the Racket function defined here:
    http://docs.racket-lang.org/picturing-programs/#%28def._%28%28lib._picturing-programs/private/map-image..rkt%29._build-image%29%29
  "
  (let ((image (: egd create width height)))
    (foreach
      (lambda (x)
        (foreach
          (lambda (y)
            (process-pixel image func x y))
          (seq 0 height)))
      (seq 0 width))
    image))

(defun write-image (image filename filetype)
  "
  image is an egd image type
  filename is a string value
  filetype is an atom, e.g., 'png
  "
  (: egd save
    (: egd render image filetype)
    filename))

(defun create-image (filename filetype width height func)
  "
  A wrapper function for build- and write-image.
  "
  (let ((image (build-image width height func)))
    (write-image image filename filetype)))

(defun create-white-image (filename filetype width height)
  "
  A convenience function for creating test images.
  "
  (create-image filename filetype width height
    (lambda (x y)
      (list x y (: egd color (tuple 255 255 255))))))

(defun create-black-image (filename filetype width height)
  "
  A convenience function for creating test images.
  "
  (create-image filename filetype width height
    (lambda (x y)
      (list x y (: egd color 'black)))))

(defun create-perlin-image (filename filetype)
  "
  "
  (create-perlin-image filename filetype 256 256))

(defun create-perlin-image (filename filetype width height)
  "
  "
  (create-perlin-image filename filetype width height 1.0))


(defun create-perlin-image (filename filetype width height multiplier)
  "
  "
  (create-image filename filetype width height
    (lambda (x y)
      (let* ((value (get-perlin-for-point
                      (tuple x y) (tuple width height) multiplier))
             (adjusted (color-scale value #(-1 1))))
        (list x y (: egd color (tuple adjusted adjusted adjusted)))))))

(defun create-simplex-image (filename filetype)
  "
  "
  (create-perlin-image filename filetype 256 256))

(defun create-simplex-image (filename filetype width height)
  "
  "
  (create-perlin-image filename filetype width height 1.0))


(defun create-simplex-image (filename filetype width height multiplier)
  "
  "
  (create-image filename filetype width height
    (lambda (x y)
      (let* ((value (get-simplex-for-point
                      (tuple x y) (tuple width height) multiplier))
             (adjusted (color-scale value #(-1 1))))
        (list x y (: egd color (tuple adjusted adjusted adjusted)))))))