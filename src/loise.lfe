(defmodule loise
  (export all)
  (import
    (from lfe-utils
      (add-tuples 1)
      (fast-floor 1))
    (from loise-util
      (element-index 2)
      (dot 4))))

(defmacro grad3
  '#(#( 1.0  1.0  0.0) #(-1.0  1.0  0.0) #( 1.0 -1.0  0.0) #(-1.0 -1.0  0.0)
     #( 1.0  0.0  1.0) #(-1.0  0.0  1.0) #( 1.0  0.0 -1.0) #(-1.0  0.0 -1.0)
     #( 0.0  1.0  1.0) #( 0.0 -1.0  1.0) #( 0.0  1.0 -1.0) #( 0.0 -1.0 -1.0)))

(defmacro F3 ()
  "Very nice and simple skew factor for 3D"
  (/ 1.0 3.0))

(defmacro G3 ()
  "Very nice and simple unskew factor, too"
  (/ 1.0 6.0))

(defmacro perm-half ()
  (tuple 151 160 137 91 90 15 131 13 201 95 96 53 194 233 7
         225 140 36 103 30 69 142 8 99 37 240 21 10 23 190 6
         148 247 120 234 75 0 26 197 62 94 252 219 203 117 35
         11 32 57 177 33 88 237 149 56 87 174 20 125 136 171
         168 68 175 74 165 71 134 139 48 27 166 77 146 158
         231 83 111 229 122 60 211 133 230 220 105 92 41 55
         46 245 40 244 102 143 54 65 25 63 161 1 216 80 73
         209 76 132 187 208 89 18 169 200 196 135 130 116 188
         159 86 164 100 109 198 173 186 3 64 52 217 226 250
         124 123 5 202 38 147 118 126 255 82 85 212 207 206
         59 227 47 16 58 17 182 189 28 42 223 183 170 213 119
         248 152 2 44 154 163 70 221 153 101 155 167 43 172 9
         129 22 39 253 19 98 108 110 79 113 224 232 178 185
         112 104 218 246 97 228 251 34 242 193 238 210 144 12
         191 179 162 241 81 51 145 235 249 14 239 107 49 192
         214 31 181 199 106 157 184 84 204 176 115 121 50 45
         127 4 150 254 138 236 205 93 222 114 67 29 24 72 243
         141 128 195 78 66 215 61 156 180))

(defmacro perm ()
  `(add-tuples (list (perm-half) (perm-half))))

(defun mix (a b t)
  (+ (* (- 1.0 t) a) (* t b)))

(defun fade (t)
  (* t t t (+ (* t (- (* t 6.0) 15.0)) 10.0)))

(defun get-gradient-index (a b c)
  (rem
    (element-index (perm)
      (+ a
        (element-index (perm)
          (+ b
            (element-index (perm) c))))) 12))

(defun get-noise-contribution (g x y z)
  (dot
    (element-index (grad3) g)
    x y z))

(defun perlin (a)
  (perlin a 0.0 0.0))

(defun perlin (a b)
  (perlin a b 0.0))

(defun perlin (a b c)
  "
  Perlin  noise is a computer-generated visual effect developed by Ken Perlin,
  who won an Academy Award for Technical Achievement for inventing it. It can
  be used to simulate elements from nature, and is especially useful in
  circumstances where computer memory is limited.
  "
  (let*
    (
      ; find unit grid cell containing point
      (A (fast-floor a))
      (B (fast-floor b))
      (C (fast-floor c))
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
      (gi000 (get-gradient-index X Y Z))
      (gi001 (get-gradient-index X Y (+ Z 1)))
      (gi010 (get-gradient-index X (+ Y 1) Z))
      (gi011 (get-gradient-index X (+ Y 1) (+ Z 1)))
      (gi100 (get-gradient-index (+ X 1) Y Z))
      (gi101 (get-gradient-index (+ X 1) Y (+ Z 1)))
      (gi110 (get-gradient-index (+ X 1) (+ Y 1) Z))
      (gi111 (get-gradient-index (+ X 1) (+ Y 1) (+ Z 1)))
      ; calculate noise contributions from each of the eight corners
      (n000 (get-noise-contribution gi000 x y z))
      (n001 (get-noise-contribution gi001 x y (- z 1)))
      (n010 (get-noise-contribution gi010 x (- y 1) z))
      (n011 (get-noise-contribution gi011 x (- y 1) (- z 1)))
      (n100 (get-noise-contribution gi100 (- x 1) y z))
      (n101 (get-noise-contribution gi101 (- x 1) y (- z 1)))
      (n110 (get-noise-contribution gi110 (- x 1) (- y 1) z))
      (n111 (get-noise-contribution gi111 (- x 1) (- y 1) (- z 1)))
      ; compute the fade curve value for each of x, y, z
      (u (fade x))
      (v (fade y))
      (w (fade z))
      ; interpolate along x the contributions from each of the corners
      (nx00 (mix n000 n100 u))
      (nx01 (mix n001 n101 u))
      (nx10 (mix n010 n110 u))
      (nx11 (mix n011 n111 u))
      ; interpolate the four results along y
      (nxy0 (mix nx00 nx10 v))
      (nxy1 (mix nx01 nx11 v)))
      ; finally, interpolate the two last results along z and return the result
      (mix nxy0 nxy1 w)))

(defun which-simplex (a b c)
  "
  For the 3D case, the simplex shape is a slightly irregular tetrahedron.
  This function determines which simplex we are in.
  "
  (cond
    ((and (>= a b) (>= b c)) (list 1 0 0 1 1 0)) ; X Y Z order
    ((and (>= a b) (>= a c)) (list 1 0 0 1 0 1)) ; X Z Y order
    ((>= a b) (list 0 0 1 1 0 1)) ; Z X Y order
    ((< b c) (list 0 0 1 0 1 1)) ; Z Y X order
    ((< a c) (list 0 1 0 0 1 1)) ; Y Z X order
    (else (list 0 1 0 1 1 0)))) ; Y X Z order

(defun corner-contribution (g x y z)
  (let* ((t (- 0.5 (* x x) (* y y) (* z z)))
         (t^2 (* t t)))
    (if (< t 0)
      0.0
      (* t^2 t^2 (dot (element-index (grad3) g) x y z)))))

(defun simplex (a)
  (simplex a 0.0 0.0))

(defun simplex (a b)
  (simplex a b 0.0))

(defun simplex (a b c)
  "
  Simplex noise is a method for constructing an n-dimensional noise function
  comparable to Perlin noise ('classic' noise) but with a lower computational
  overhead, especially in larger dimensions. Ken Perlin designed the algorithm
  in 2001 to address the limitations of his classic noise function, especially
  in higher dimensions.
  "
  (let*
    (
      ; skew the input space to determine which simplex cell we're in
      (s (* (+ a b c) (F3)))
      (i (fast-floor (+ a s)))
      (j (fast-floor (+ b s)))
      (k (fast-floor (+ c s)))
      (t (* (+ i j k) (G3)))
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
      (x1 (+ (- x0 i1) (G3)))
      (y1 (+ (- y0 j1) (G3)))
      (z1 (+ (- z0 k1) (G3)))
      ; Offsets for third corner in (x,y,z) coords
      (x2 (+ (- x0 i2) (* 2.0 (G3))))
      (y2 (+ (- y0 j2) (* 2.0 (G3))))
      (z2 (+ (- z0 k2) (* 2.0 (G3))))
      ; Offsets for last corner in (x,y,z) coords
      (x3 (+ (- x0 1.0) (* 3.0 (G3))))
      (y3 (+ (- y0 1.0) (* 3.0 (G3))))
      (z3 (+ (- z0 1.0) (* 3.0 (G3))))
      ; Work out the hashed gradient indices of the four simplex corners
      (ii (band i 255))
      (jj (band j 255))
      (kk (band k 255))
      (gi0 (get-gradient-index ii jj kk))
      (gi1 (get-gradient-index (+ ii i1) (+ jj j1) (+ kk k1)))
      (gi2 (get-gradient-index (+ ii i2) (+ jj j2) (+ kk k2)))
      (gi3 (get-gradient-index (+ ii 1) (+ jj 1) (+ kk 1)))
      ; Calculate the contribution from the four corners
      (n0 (corner-contribution gi0 x0 y0 z0))
      (n1 (corner-contribution gi1 x1 y1 z1))
      (n2 (corner-contribution gi2 x2 y2 z2))
      (n3 (corner-contribution gi3 x3 y3 z3)))
    ; Add contributions from each corner to get the final noise value.
    ; The result is scaled to stay just inside [-1,1]
    ; NOTE: This scaling factor seems to work better than the given one
    ;       I'm not sure why
    (* 76.5 (+ n0 n1 n2 n3))))

