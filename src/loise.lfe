(defmodule loise
  (export all)
  (import
    (from proplists
      (get_value 2))))

(include-lib "clj/include/compose.lfe")

;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;; API
;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

(defun perlin (a)
  (perlin a 0.0 0.0 (loise-const:base-options)))

(defun perlin (a options)
  (perlin a 0.0 0.0 options))

(defun perlin (a b options)
  (perlin a b 0.0 options))

(defun perlin (a b c options)
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
     (gi000 (get-gradient-index X Y Z options))
     (gi001 (get-gradient-index X Y (+ Z 1) options))
     (gi010 (get-gradient-index X (+ Y 1) Z options))
     (gi011 (get-gradient-index X (+ Y 1) (+ Z 1) options))
     (gi100 (get-gradient-index (+ X 1) Y Z options))
     (gi101 (get-gradient-index (+ X 1) Y (+ Z 1) options))
     (gi110 (get-gradient-index (+ X 1) (+ Y 1) Z options))
     (gi111 (get-gradient-index (+ X 1) (+ Y 1) (+ Z 1) options))
     ; calculate noise contributions from each of the eight corners
     (n000 (get-noise-contribution gi000 x y z options))
     (n001 (get-noise-contribution gi001 x y (- z 1) options))
     (n010 (get-noise-contribution gi010 x (- y 1) z options))
     (n011 (get-noise-contribution gi011 x (- y 1) (- z 1) options))
     (n100 (get-noise-contribution gi100 (- x 1) y z options))
     (n101 (get-noise-contribution gi101 (- x 1) y (- z 1) options))
     (n110 (get-noise-contribution gi110 (- x 1) (- y 1) z options))
     (n111 (get-noise-contribution gi111 (- x 1) (- y 1) (- z 1) options))
     ; compute the fade curve value for each of x, y, z
     (u (fade x options))
     (v (fade y options))
     (w (fade z options))
     ; interpolate along x the contributions from each of the corners
     (nx00 (mix n000 n100 u options))
     (nx01 (mix n001 n101 u options))
     (nx10 (mix n010 n110 u options))
     (nx11 (mix n011 n111 u options))
     ; interpolate the four results along y
     (nxy0 (mix nx00 nx10 v options))
     (nxy1 (mix nx01 nx11 v options)))
     ; finally, interpolate the two last results along z and return the result
     (mix nxy0 nxy1 w options)))

(defun simplex (a)
  (simplex a 0.0 0.0 (loise-const:base-options)))

(defun simplex (a options)
  (simplex a 0.0 0.0 options))

(defun simplex (a b options)
  (simplex a b 0.0 options))

(defun simplex (a b c options)
  "Simplex noise is a method for constructing an n-dimensional noise function
  comparable to Perlin noise ('classic' noise) but with a lower computational
  overhead, especially in larger dimensions. Ken Perlin designed the algorithm
  in 2001 to address the limitations of his classic noise function, especially
  in higher dimensions."
  (let*
    (; skew the input space to determine which simplex cell we're in
     (s (* (+ a b c) (loise-const:skew-factor)))
     (i (lutil-math:fast-floor (+ a s)))
     (j (lutil-math:fast-floor (+ b s)))
     (k (lutil-math:fast-floor (+ c s)))
     (t (* (+ i j k) (loise-const:unskew-factor)))
     ; unskew the cell origin back to (x,y,z) space
     (X0 (- i t))
     (Y0 (- j t))
     (Z0 (- k t))
     ; the x,y,z distances from the cell origin
     (x0 (- a X0))
     (y0 (- b Y0))
     (z0 (- c Z0))
     ; find out which simplex we are in
     ((list i1 j1 k1 i2 j2 k2) (which-simplex x0 y0 z0))
     ; A step of (1,0,0) in (i,j,k) means a step of (1-c,-c,-c) in (x,y,z),
     ; a step of (0,1,0) in (i,j,k) means a step of (-c,1-c,-c) in (x,y,z),
     ; and a step of (0,0,1) in (i,j,k) means a step of (-c,-c,1-c) in
     ; (x,y,z), where c = 1/6.
     ;
     ; Offsets for second corner in (x,y,z) coords
     (x1 (+ (- x0 i1) (loise-const:unskew-factor)))
     (y1 (+ (- y0 j1) (loise-const:unskew-factor)))
     (z1 (+ (- z0 k1) (loise-const:unskew-factor)))
     ; Offsets for third corner in (x,y,z) coords
     (x2 (+ (- x0 i2) (* 2.0 (loise-const:unskew-factor))))
     (y2 (+ (- y0 j2) (* 2.0 (loise-const:unskew-factor))))
     (z2 (+ (- z0 k2) (* 2.0 (loise-const:unskew-factor))))
     ; Offsets for last corner in (x,y,z) coords
     (x3 (+ (- x0 1.0) (* 3.0 (loise-const:unskew-factor))))
     (y3 (+ (- y0 1.0) (* 3.0 (loise-const:unskew-factor))))
     (z3 (+ (- z0 1.0) (* 3.0 (loise-const:unskew-factor))))
     ; Work out the hashed gradient indices of the four simplex corners
     (ii (band i 255))
     (jj (band j 255))
     (kk (band k 255))
     (gi0 (get-gradient-index ii jj kk options))
     (gi1 (get-gradient-index (+ ii i1) (+ jj j1) (+ kk k1) options))
     (gi2 (get-gradient-index (+ ii i2) (+ jj j2) (+ kk k2) options))
     (gi3 (get-gradient-index (+ ii 1) (+ jj 1) (+ kk 1) options))
     ; Calculate the contribution from the four corners
     (n0 (corner-contribution gi0 x0 y0 z0 options))
     (n1 (corner-contribution gi1 x1 y1 z1 options))
     (n2 (corner-contribution gi2 x2 y2 z2 options))
     (n3 (corner-contribution gi3 x3 y3 z3 options)))
     ; Add contributions from each corner to get the final noise value.
     ; The result is scaled to stay just inside [-1,1]
     ; NOTE: This scaling factor seems to work better than the given one
     ;       I'm not sure why
     (* (get_value 'simplex-scale options) (+ n0 n1 n2 n3))))

;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;; Supporting functions
;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

(defun mix (a b t options)
  (+ (* (- (get_value 'mix-shift options) t) a) (* t b)))

(defun fade (t options)
  (* t t t
    (+ (get_value 'fade-shift-2 options)
       (* t (- (* t (get_value 'fade-factor options))
               (get_value 'fade-shift-1 options))))))

(defun get-gradient-index (a b c options)
  (let ((perm (get_value 'perm-table options))
        (modulus (get_value 'grad-modulus options)))
    (rem (->> (loise-util:index perm c)
              (+ b)
              (loise-util:index perm)
              (+ a)
              (loise-util:index perm))
         modulus)))

(defun get-noise-contribution (g x y z options)
  (loise-util:dot
    (loise-util:index (get_value 'grad-matrix options) g)
    x y z))


(defun which-simplex (a b c)
  "For the 3D case, the simplex shape is a slightly irregular tetrahedron.
  This function determines which simplex we are in."
  (cond
    ((and (>= a b) (>= b c)) (list 1 0 0 1 1 0)) ; X Y Z order
    ((and (>= a b) (>= a c)) (list 1 0 0 1 0 1)) ; X Z Y order
    ((>= a b) (list 0 0 1 1 0 1)) ; Z X Y order
    ((< b c) (list 0 0 1 0 1 1)) ; Z Y X order
    ((< a c) (list 0 1 0 0 1 1)) ; Y Z X order
    (else (list 0 1 0 1 1 0)))) ; Y X Z order

(defun corner-contribution (g x y z options)
  (let* ((t (- 0.5 (* x x) (* y y) (* z z)))
         (t^2 (* t t)))
    (if (< t 0)
      0.0
      (* t^2 t^2 (loise-util:dot
                   (loise-util:index (get_value 'grad-matrix options) g)
                   x y z)))))

(defun get-perlin-point (coords size multiplier)
  (get-perlin-point coords size multiplier (loise-const:base-options)))

(defun get-perlin-point
  ((`(,x) `(,width) multiplier options)
    (perlin (* multiplier (/ x width)) options))
  ((`(,x ,y) `(,width ,height) multiplier options)
    (perlin (* multiplier (/ x width))
            (* multiplier (/ y height))
            options))
  ((`(,x ,y ,z) `(,width ,height ,depth) multiplier options)
    (perlin (* multiplier (/ x width))
            (* multiplier (/ y height))
            (* multiplier (/ z depth))
            options)))

(defun get-simplex-point (coords size multiplier)
  (get-simplex-point coords size multiplier (loise-const:base-options)))

(defun get-simplex-point
  ((`(,x) `(,width) multiplier options)
    (simplex (* multiplier (/ x width)) options))
  ((`(,x ,y) `(,width ,height) multiplier options)
    (simplex (* multiplier (/ x width))
             (* multiplier (/ y height))
             options))
  ((`(,x ,y ,z) `(,width ,height ,depth) multiplier options)
    (simplex (* multiplier (/ x width))
             (* multiplier (/ y height))
             (* multiplier (/ z depth))
             options)))
