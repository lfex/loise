(defmodule loise
  (export all)
  (import
    (from erlang
      (list_to_tuple 1)
      (rem 2)
      (trunc 1)
      (tuple_to_list 1))
    (from lists
      (flatten 1)
      (foldl 3)
      (map 2)
      (zipwith 3))))

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

(defun mix (a b t)
  (+ (* (- 1.0 t) a) (* t b)))

(defun fade (t)
  (* t t t (+ (* t (- (* t 6.0) 15.0)) 10.0)))

(defun get-gradient-index (a b c)
  (remainder
    (vector-ref (perm)
      (+ a
        (vector-ref (perm)
          (+ b
            (vector-ref (perm) c))))) 12))

(defun get-noise-contribution (g x y z)
  (dot
    (vector-ref (grad3) g)
    x y z))

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
      (X (bitwise-and A 255))
      (Y (bitwise-and B 255))
      (Z (bitwise-and C 255))
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

(defun simplex (x y z)
  "
  Simplex noise is a method for constructing an n-dimensional noise function
  comparable to Perlin noise ('classic' noise) but with a lower computational
  overhead, especially in larger dimensions. Ken Perlin designed the algorithm
  in 2001 to address the limitations of his classic noise function, especially
  in higher dimensions.
  "
  )