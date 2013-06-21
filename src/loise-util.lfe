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
      (seq 2))))

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