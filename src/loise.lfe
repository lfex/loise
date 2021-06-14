(defmodule loise
  (export
   (perlin 1) (perlin 2) (perlin 3) (perlin 4)
   (perlin-point 2) (perlin-point 3)
   (simplex 1) (simplex 2) (simplex 3) (simplex 4)
   (simplex-point 2) (simplex-point 3)))

;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;; API
;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

(defun perlin (x)
  (loise-perlin:1d x))

(defun perlin (x opts)
  (loise-perlin:1d x opts))

(defun perlin (x y opts)
  (loise-perlin:2d x y opts))

(defun perlin (x y z opts)
  (loise-perlin:3d x y z opts))

(defun perlin-point(coords opts)
  (loise-perlin:point coords opts))

(defun perlin-point(coords size mult)
  (loise-perlin:point coords size mult))

(defun simplex (x)
  (loise-simplex:1d x))

(defun simplex (x opts)
  (loise-simplex:1d x opts))

(defun simplex (x y opts)
  (loise-simplex:2d x y opts))

(defun simplex (x y z opts)
  (loise-simplex:3d x y z opts))

(defun simplex-point(coords opts)
  (loise-simplex:point coords opts))

(defun simplex-point(coords size mult)
  (loise-simplex:point coords size mult))
