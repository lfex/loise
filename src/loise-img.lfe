(defmodule loise-img
  (export all)
  (import
    (from loise
      (perlin 1) (perlin 2) (perlin 3)
      (simplex 1) (simplex 2) (simplex 3))))

(defun partial
  "Something akin to a partial that will suit our purposes ;-)
  See code comments for usage."
  ;;
  ;; > (defun do-things (m x y w h)
  ;;     (lfe_io:format "m: ~p, x: ~p, y: ~p, w: ~p, h: ~p~n" (list m x y w h)))
  ;; do-things
  ;;
  ;; > (set func (partial #'do-things/5 10))
  ;; #Fun<lfe_eval.10.132627770>
  ;; > (funcall func '(1 2 100 200))
  ;; m: 10, x: 1, y: 2, w: 100, h: 200
  ;; ok
  ;;
  ;; > (set func-2 (partial #'do-things/5 '(10 1)))
  ;; #Fun<lfe_eval.10.132627770>
  ;; > (funcall func-2 '(2 100 200))
  ;; m: 10, x: 1, y: 2, w: 100, h: 200
  ;; ok
  ((f (cons arg1 (cons arg2 '())))
    (lambda (args)
      (apply f
        (++ `(,arg1 ,arg2) args))))
  ((f arg)
    (lambda (args)
      (apply f
        (cons arg args)))))

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

(defun write-image (image filename filetype)
  "Write the image data.

  image is an egd image type
  filename is a string value
  filetype is an atom, e.g., 'png"
  (egd:save
    (egd:render image filetype)
    filename))

(defun create-image (filename filetype width height func)
  "A wrapper function for build- and write-image."
  (let ((image (build-image width height func)))
    (write-image image filename filetype)))

(defun create-white-image (filename filetype width height)
  "A convenience function for creating test images."
  (create-image filename filetype width height
    (lambda (x y)
      (list x y (egd:color (tuple 255 255 255))))))

(defun create-black-image (filename filetype width height)
  "A convenience function for creating test images."
  (create-image filename filetype width height
    (lambda (x y)
      (list x y (egd:color 'black)))))

(defun create-perlin-image (filename filetype)
  (create-perlin-image filename filetype 256 256 1.0))

(defun create-perlin-image (filename filetype width height)
  (create-perlin-image filename filetype width height 1.0))

(defun create-perlin-image (filename filetype width height multiplier)
  (create-image filename filetype
                width height
                (partial #'draw-perlin-point!/6 multiplier)))

(defun create-perlin-image (filename filetype width height multiplier grades)
  (create-image filename filetype
                width height
                (partial #'draw-graded-perlin-point!/7
                         `(,multiplier ,grades))))

(defun create-simplex-image (filename filetype)
  (create-perlin-image filename filetype 256 256))

(defun create-simplex-image (filename filetype width height)
  (create-perlin-image filename filetype width height 1.0))

(defun create-simplex-image (filename filetype width height multiplier)
  (create-image filename filetype
                width height
                (partial #'draw-simplex-point!/6 multiplier)))

(defun create-simplex-image (filename filetype width height multiplier grades)
  (create-image filename filetype
                width height
                (partial #'draw-graded-simplex-point!/7
                         `(,multiplier ,grades))))
