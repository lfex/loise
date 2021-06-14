(defmodule loise-perlin
  (export
   (1d 1) (1d 2)
   (2d 3)
   (3d 4)
   (point 3) (point 4)))

(defun 1d (a)
  (3d a 0.0 0.0 (default-options)))

(defun 1d (a options)
  (3d a 0.0 0.0 options))

(defun 2d (a b options)
  (3d a b 0.0 options))

(defun 3d (a b c options)
  "Perlin noise is a computer-generated visual effect developed by Ken Perlin,
  who won an Academy Award for Technical Achievement for inventing it. It can
  be used to simulate elements from nature, and is especially useful in
  circumstances where computer memory is limited."
  (let*
    (; find unit grid cell containing point
     (A (lutil-math:fast-floor a))
     (B (lutil-math:fast-floor b))
     (C (lutil-math:fast-floor c))
     ; get relative xyz coordinates of point within cell
     (x (- a A))
     (y (- b B))
     (z (- c C))
     ; wrap the integer cells at 255 (smaller integer period can be
     ; introduced here)
     (X (band A 255))
     (Y (band B 255))
     (Z (band C 255))
     ; calculate a set of eight hashed gradient indices
     (gi000 (loise-util:get-gradient-index X Y Z options))
     (gi001 (loise-util:get-gradient-index X Y (+ Z 1) options))
     (gi010 (loise-util:get-gradient-index X (+ Y 1) Z options))
     (gi011 (loise-util:get-gradient-index X (+ Y 1) (+ Z 1) options))
     (gi100 (loise-util:get-gradient-index (+ X 1) Y Z options))
     (gi101 (loise-util:get-gradient-index (+ X 1) Y (+ Z 1) options))
     (gi110 (loise-util:get-gradient-index (+ X 1) (+ Y 1) Z options))
     (gi111 (loise-util:get-gradient-index (+ X 1) (+ Y 1) (+ Z 1) options))
     ; calculate noise contributions from each of the eight corners
     (n000 (loise-util:get-noise-contribution gi000 x y z options))
     (n001 (loise-util:get-noise-contribution gi001 x y (- z 1) options))
     (n010 (loise-util:get-noise-contribution gi010 x (- y 1) z options))
     (n011 (loise-util:get-noise-contribution gi011 x (- y 1) (- z 1) options))
     (n100 (loise-util:get-noise-contribution gi100 (- x 1) y z options))
     (n101 (loise-util:get-noise-contribution gi101 (- x 1) y (- z 1) options))
     (n110 (loise-util:get-noise-contribution gi110 (- x 1) (- y 1) z options))
     (n111 (loise-util:get-noise-contribution gi111 (- x 1) (- y 1) (- z 1) options))
     ; compute the fade curve value for each of x, y, z
     (u (loise-util:fade x options))
     (v (loise-util:fade y options))
     (w (loise-util:fade z options))
     ; interpolate along x the contributions from each of the corners
     (nx00 (loise-util:mix n000 n100 u options))
     (nx01 (loise-util:mix n001 n101 u options))
     (nx10 (loise-util:mix n010 n110 u options))
     (nx11 (loise-util:mix n011 n111 u options))
     ; interpolate the four results along y
     (nxy0 (loise-util:mix nx00 nx10 v options))
     (nxy1 (loise-util:mix nx01 nx11 v options)))
     ; finally, interpolate the two last results along z and return the result
     (loise-util:mix nxy0 nxy1 w options)))

(defun point (coords dims multiplier)
  (point coords dims multiplier (default-options)))

(defun point
  ((`(,x) `(,width) multiplier options)
   (1d (* multiplier (/ x width)) options))
  ((`(,x ,y) `(,width ,height) multiplier options)
   (2d (* multiplier (/ x width))
       (* multiplier (/ y height))
       options))
  ((`(,x ,y ,z) `(,width ,height ,depth) multiplier options)
   (3d (* multiplier (/ x width))
       (* multiplier (/ y height))
       (* multiplier (/ z depth))
       options)))

;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;; Supporting functions
;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

;; XXX replace with stuff from new .hrl
(defun default-options ()
  (loise-const:base-options))
