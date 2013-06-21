(defmodule loise-util
  (export all)
  (import
    (from erlang
      (list_to_tuple 1)
      (rem 2)
      (round 1)
      (trunc 1)
      (tuple_to_list 1))
    (from lists
      (flatten 1)
      (foldl 3)
      (map 2)
      (seq 2)
      (zipwith 3))
    (from loise
      (perlin 1) (perlin 2) (perlin 3)
      (simplex 1) (simplex 2) (simplex 3))
    (from math
      (pow 2))))

(defun add-tuples (a)
  "
  If there's a better way to do this, pull requests welcome!
  "
  (list_to_tuple
    (flatten
      (map (lambda (x) (tuple_to_list x)) a))))

(defun fast-floor (int)
  "
  Sadly, this is named 'fast-floor' only because the Racket version was given
  that name (it makes copying and pasting the code that much easier!). There
  is no good floor function in Erlang... so this should probably have been
  called 'slow-floor'.
  "
  (let* ((trunc (trunc int))
         (check (- int trunc)))
    (cond
      ((< check 0) (- trunc 1))
      ((> check 0) trunc)
      ('true trunc))))

(defun round (number precision)
  "
  Round a floating point number to the given number of decimal places.
  "
  (let ((p (pow 10 precision)))
    (/ (round (* number p)) p)))

(defun vector-ref (tuple position)
  "
  This provides the same interface as the Racket function of the same name.
  "
  (: erlang element (+ 1 position) tuple))

(defun remainder (a b)
  "
  This is essentially an alias so that Racket-based code will be easier to use.
  "
  (rem a b))

(defun bitwise-and (a b)
  "
  This is essentially an alias so that Racket-based code will be easier to use.
  "
  (band a b))

(defun dot-product (a b)
  "
  This doesn't appear to be needed for this particular library, but it was fun
  to write, and is quite pretty, so it's staying ;-)
  "
  (foldl #'+/2 0
    (zipwith #'*/2 a b)))

(defun dot (g x y z)
   (+ (* (vector-ref g 0) x)
      (* (vector-ref g 1) y)
      (* (vector-ref g 2) z)))

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