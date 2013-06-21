(defmodule loise-util
  (export all)
  (import
    (from lists
      (flatten 1)
      (map 2))
    (from loise
      (perlin 1) (perlin 2) (perlin 3)
      (simplex 1) (simplex 2) (simplex 3))
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

(defun shift (value amount)
  (+ value amount))

(defun scale (value amount)
  (* value amount))

(defun scale (value current-frame new-frame)
  "
  "
  (let* (((tuple lower-bound upper-bound) current-frame)
         ((tuple lower-bound-prime upper-bound-prime) new-frame)
         (fraction (/
                     (+ (abs lower-bound) value)
                     (+ (abs lower-bound) upper-bound)))
         (new-range (- upper-bound-prime lower-bound-prime)))
    (+ (* fraction new-range) lower-bound-prime)))

(defun unit-scale (value current-frame)
  "
  "
  (scale value current-frame #(0.0 1.0)))

(defun color-scale (value current-frame)
  "
  "
  (round (scale value current-frame #(0.0 255.0))))

(defun get-perlin-for-point
  "
  "
  (((tuple x) (tuple width) multiplier)
    (perlin (* multiplier (/ x width))))
  (((tuple x y) (tuple width height) multiplier)
    (perlin (* multiplier (/ x width))
            (* multiplier (/ y height))))
  (((tuple x y z) (tuple width height depth) multiplier)
    (perlin (* multiplier (/ x width))
            (* multiplier (/ y height))
            (* multiplier (/ z depth)))))

(defun get-perlin-range
  "
  This function is used for generating large lists of perlin noise numbers
  across a range of multipliers and sizes.
  "
  (((tuple mult-start mult-end) (tuple width))
    (flatten
      (map
        (lambda (multiplier)
          (map
            (lambda (x)
              (get-perlin-for-point (tuple x) (tuple width) multiplier))
            (seq 0 (- width 1))))
          (seq mult-start mult-end))))
  (((tuple mult-start mult-end) (tuple width height))
    (flatten
      (map
        (lambda (multiplier)
          (map
            (lambda (x)
              (map
                (lambda (y)
                  (get-perlin-for-point
                    (tuple x y)
                    (tuple width height)
                    multiplier))
                (seq 0 (- height 1))))
            (seq 0 (- width 1))))
          (seq mult-start mult-end))))
  (((tuple mult-start mult-end) (tuple width height depth)))
  ; let's save this one for later...
  )

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
             (adjusted (color-scale value #(-0.5 0.5))))
        (list x y (: egd color (tuple adjusted adjusted adjusted)))))))