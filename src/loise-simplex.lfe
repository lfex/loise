(defmodule loise-simplex
  (export
   (1d 1)
   (1d 2)
   (2d 3)
   (3d 4)
   (point 3) (point 4)
   (value-range 0)
   (which 3)))

(include-lib "include/options.lfe")

(defun value-range () #(-1 1))

(defun 1d (a)
  (3d a 0.0 0.0 (default-options)))

(defun 1d (a opts)
  (3d a 0.0 0.0 opts))

(defun 2d (a b opts)
  (3d a b 0.0 opts))

(defun 3d (a b c opts)
  "Simplex noise is a method for constructing an n-dimensional noise function
  comparable to Perlin noise ('classic' noise) but with a lower computational
  overhead, especially in larger dimensions. Ken Perlin designed the algorithm
  in 2001 to address the limitations of his classic noise function, especially
  in higher dimensions."
  (let*
      (;; opts for re-use
       (unskew-factor (loise-opts:unskew-factor opts))
       ;; skew the input space to determine which simplex cell we're in
       (s (* (+ a b c) (loise-opts:skew-factor opts)))
       (i (lutil-math:fast-floor (+ a s)))
       (j (lutil-math:fast-floor (+ b s)))
       (k (lutil-math:fast-floor (+ c s)))
       (t (* (+ i j k) unskew-factor))
       ;; unskew the cell origin back to (x,y,z) space
       (X0 (- i t))
       (Y0 (- j t))
       (Z0 (- k t))
       ;; the x,y,z distances from the cell origin
       (x0 (- a X0))
       (y0 (- b Y0))
       (z0 (- c Z0))
       ;; find out which simplex we are in
       ((list i1 j1 k1 i2 j2 k2) (which x0 y0 z0))
       ;; A step of (1,0,0) in (i,j,k) means a step of (1-c,-c,-c) in (x,y,z),
       ;; a step of (0,1,0) in (i,j,k) means a step of (-c,1-c,-c) in (x,y,z),
       ;; and a step of (0,0,1) in (i,j,k) means a step of (-c,-c,1-c) in
       ;; (x,y,z), where c = 1/6.
       ;;
       ;; Offsets for second corner in (x,y,z) coords
       (x1 (+ (- x0 i1) unskew-factor))
       (y1 (+ (- y0 j1) unskew-factor))
       (z1 (+ (- z0 k1) unskew-factor))
       ;; Offsets for third corner in (x,y,z) coords
       (x2 (+ (- x0 i2) (* 2.0 unskew-factor)))
       (y2 (+ (- y0 j2) (* 2.0 unskew-factor)))
       (z2 (+ (- z0 k2) (* 2.0 unskew-factor)))
       ;; Offsets for last corner in (x,y,z) coords
       (x3 (+ (- x0 1.0) (* 3.0 unskew-factor)))
       (y3 (+ (- y0 1.0) (* 3.0 unskew-factor)))
       (z3 (+ (- z0 1.0) (* 3.0 unskew-factor)))
       ;; Work out the hashed gradient indices of the four simplex corners
       (ii (band i 255))
       (jj (band j 255))
       (kk (band k 255))
       (gi0 (loise-util:get-gradient-index ii jj kk opts))
       (gi1 (loise-util:get-gradient-index (+ ii i1) (+ jj j1) (+ kk k1) opts))
       (gi2 (loise-util:get-gradient-index (+ ii i2) (+ jj j2) (+ kk k2) opts))
       (gi3 (loise-util:get-gradient-index (+ ii 1) (+ jj 1) (+ kk 1) opts))
       ;; Calculate the contribution from the four corners
       (n0 (loise-util:corner-contribution gi0 x0 y0 z0 opts))
       (n1 (loise-util:corner-contribution gi1 x1 y1 z1 opts))
       (n2 (loise-util:corner-contribution gi2 x2 y2 z2 opts))
       (n3 (loise-util:corner-contribution gi3 x3 y3 z3 opts)))
    ;; Add contributions from each corner to get the final noise value.
    ;; The result is scaled to stay just inside [-1,1]
    ;; NOTE: This scaling factor seems to work better than the given one
    ;;       I'm not sure why
    (* (loise-opts:simplex-scale opts) (+ n0 n1 n2 n3))))

(defun which (a b c)
  "For the 3D case, the simplex shape is a slightly irregular tetrahedron.
  This function determines which simplex we are in."
  (cond
   ((and (>= a b) (>= b c)) (list 1 0 0 1 1 0)) ; X Y Z order
   ((and (>= a b) (>= a c)) (list 1 0 0 1 0 1)) ; X Z Y order
   ((>= a b) (list 0 0 1 1 0 1)) ; Z X Y order
   ((< b c) (list 0 0 1 0 1 1)) ; Z Y X order
   ((< a c) (list 0 1 0 0 1 1)) ; Y Z X order
   (else (list 0 1 0 1 1 0)))) ; Y X Z order

(defun point (coords dims multiplier)
  (point coords dims multiplier (default-options)))

(defun point
  ((`(,x) `(,width) multiplier opts)
   (1d (* multiplier (/ x width)) opts))
  ((`(,x ,y) `(,width ,height) multiplier opts)
   (2d (* multiplier (/ x width))
       (* multiplier (/ y height))
       opts))
  ((`(,x ,y ,z) `(,width ,height ,depth) multiplier opts)
   (3d (* multiplier (/ x width))
       (* multiplier (/ y height))
       (* multiplier (/ z depth))
       opts)))
