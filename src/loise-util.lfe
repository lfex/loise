(defmodule loise-util
  (export all)
  (import
    (from lists
      (flatten 1)
      (map 2)
      (seq 2))
    (from loise
      (perlin 1) (perlin 2) (perlin 3)
      (simplex 1) (simplex 2) (simplex 3))))

(defun get-loise-version ()
  (lutil:get-app-src-version "src/loise.app.src"))

(defun get-versions ()
  (++ (lutil:get-version)
      `(#(loise ,(get-loise-version)))))

(defun index (data position)
  "A list-based version of element-index."
  (lists:nth (+ 1 position) data))

(defun dot (g x y z)
   (+ (* (index g 0) x)
      (* (index g 1) y)
      (* (index g 2) z)))

(defun get-perlin-for-point
  ((`(,x) `(,width) multiplier)
    (perlin (* multiplier (/ x width))))
  ((`(,x ,y) `(,width ,height) multiplier)
    (perlin (* multiplier (/ x width))
            (* multiplier (/ y height))))
  ((`(,x ,y ,z) `(,width ,height ,depth) multiplier)
    (perlin (* multiplier (/ x width))
            (* multiplier (/ y height))
            (* multiplier (/ z depth)))))

(defun get-simplex-for-point
  ((`(,x) `(,width) multiplier)
    (simplex (* multiplier (/ x width))))
  ((`(,x ,y) `(,width ,height) multiplier)
    (simplex (* multiplier (/ x width))
             (* multiplier (/ y height))))
  ((`(,x ,y ,z) `(,width ,height ,depth) multiplier)
    (simplex (* multiplier (/ x width))
             (* multiplier (/ y height))
             (* multiplier (/ z depth)))))

(defun get-gradations (count)
  "The number 'count' passed in this function represents the total number of
  gradations we expect to get back. The 'lutil-math:get-gradations' function
  expects a different parameter: 'divisions'. In other words, 'Tell me how many
  divisions you want in the given range.' These two parameters differ by one.

  Loise uses the same color range that Erlang's egd does: 0 to 255."
  (lutil-math:get-gradations '(0 255) (- count 1)))
