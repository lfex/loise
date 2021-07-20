(defmodule loise-defaults
  (export all))

;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;; Default Data
;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

(defun gradient-matrix ()
  '(( 1.0  1.0  0.0)  (-1.0  1.0  0.0)  ( 1.0 -1.0  0.0)
    (-1.0 -1.0  0.0)  ( 1.0  0.0  1.0)  (-1.0  0.0  1.0)
    ( 1.0  0.0 -1.0)  (-1.0  0.0 -1.0)  ( 0.0  1.0  1.0)
    ( 0.0 -1.0  1.0)  ( 0.0  1.0 -1.0)  ( 0.0 -1.0 -1.0)))

(defun half-permutation-table ()
  '(151 160 137  91  90  15 131  13 201  95  96  53
    194 233   7 225 140  36 103  30  69 142   8  99
     37 240  21  10  23 190   6 148 247 120 234  75
      0  26 197  62  94 252 219 203 117  35  11  32
     57 177  33  88 237 149  56  87 174  20 125 136
    171 168  68 175  74 165  71 134 139  48  27 166
     77 146 158 231  83 111 229 122  60 211 133 230
    220 105  92  41  55 46  245  40 244 102 143  54
     65  25  63 161   1 216  80  73 209  76 132 187
    208  89  18 169 200 196 135 130 116 188 159  86
    164 100 109 198 173 186   3  64  52 217 226 250
    124 123   5 202  38 147 118 126 255  82  85 212
    207 206  59 227  47  16  58  17 182 189  28  42
    223 183 170 213 119 248 152   2  44 154 163  70
    221 153 101 155 167  43 172   9 129  22  39 253
    19   98 108 110  79 113 224 232 178 185 112 104
    218 246  97 228 251  34 242 193 238 210 144  12
    191 179 162 241  81  51 145 235 249  14 239 107
    49  192 214  31 181 199 106 157 184  84 204 176
    115 121  50  45 127   4 150 254 138 236 205  93
    222 114  67  29  24  72 243 141 128 195  78  66
    215  61 156 180))

(defun permutation-table ()
  (let ((perm (half-permutation-table)))
    (++ perm perm)))

;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;; Default Values
;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

;; Base default values
(defun default-noise () 'simplex)
(defun default-multiplier () 4.0)
(defun default-skew-factor () (/ 1.0 3.0))
(defun default-unskew-factor () (/ 1.0 6.0))
(defun default-mix-shift () 1.0)
(defun default-fade-factor () 6.0)
(defun default-fade-shift-1 () 15.0)
(defun default-fade-shift-2 () 10.0)
(defun default-grad-modulus () 12)
;; (defun default-simplex-scale-factor () 70.0)
(defun default-simplex-scale-factor () 76.5)

;; Output default values
(defun default-output-backend () 'undefined)
(defun default-output-type () 'undefined)
(defun default-output-format () 'undefined)
(defun default-image-format () 'png)
(defun default-grades-count () 6)
(defun default-grades () '())
(defun default-graded? () 'false)
(defun default-random? () 'false)
(defun default-seed () 42)

;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;; Default Options
;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

(defun base-options ()
  `#m(noise ,(default-noise)
      random? ,(default-random?)
      seed ,(default-seed)
      size undefined
      dim undefined
      multiplier ,(default-multiplier)
      simplex-scale ,(default-simplex-scale-factor)
      mix-shift ,(default-mix-shift)
      skew-factor ,(default-skew-factor)
      unskew-factor ,(default-unskew-factor)
      fade-factor ,(default-fade-factor)
      fade-shift-1 ,(default-fade-shift-1)
      fade-shift-2 ,(default-fade-shift-2)
      grad-modulus ,(default-grad-modulus)
      grades-count ,(default-grades-count)
      grades ,(default-grades)
      graded? ,(default-graded?)
      scale-func undefined
      round? false
      precision 5
      data-format flat
      colors? undefined
      colors ()
      ascii-map ()
      color-map ()))

(defun base-options (overrides)
  (maps:merge (base-options) overrides))

(defun output-options ()
  `#m(output-backend ,(default-output-backend)
      output-type ,(default-output-type)
      output-format ,(default-output-format)))
