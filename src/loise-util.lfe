(defmodule loise-util
  (export all)
  (import
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