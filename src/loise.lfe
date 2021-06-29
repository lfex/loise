(defmodule loise
  (export
   (start 0)
   (stop 0))
  (export
   (perlin 1) (perlin 2) (perlin 3) (perlin 4)
   (perlin-point 2) (perlin-point 3)
   (simplex 1) (simplex 2) (simplex 3) (simplex 4)
   (simplex-point 2) (simplex-point 3))
  (export
   (ascii 0) (ascii 1)
   (format-ascii 0) (format-ascii 1))
  (export
   (image 1) (image 2))
  (export
   (dim 1)
   (size 1)
   (gradations 1)
   (round 2))
  (export
   (version 0)
   (versions 0)))

;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;; API
;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
(defun start ()
  (application:ensure_all_started 'loise))

(defun stop ()
  (application:stop 'loise))

(defun perlin (x)
  (loise-perlin:1d x (loise-perlin:options)))

(defun perlin
  ((x y) (when (orelse (is_float y) (is_integer y)))
   (loise-perlin:2d x y (loise-perlin:options)))
  ((x opts)
   (loise-perlin:1d x opts)))

(defun perlin
  ((x y z) (when (orelse (is_float z) (is_integer z)))
   (loise-perlin:3d x y z (loise-perlin:options)))
  ((x y opts)
   (loise-perlin:2d x y opts)))

(defun perlin(x y z opts)
  (loise-perlin:3d x y z opts))

(defun perlin-point(coords opts)
  (loise-perlin:point coords opts))

(defun perlin-point(coords size mult)
  (loise-perlin:point coords size mult))

(defun simplex (x)
  (loise-simplex:1d x (loise-simplex:options)))

(defun simplex
  ((x y) (when (orelse (is_float y) (is_integer y)))
   (loise-simplex:2d x y (loise-simplex:options)))
  ((x opts)
   (loise-simplex:1d x opts)))

(defun simplex
  ((x y z) (when (orelse (is_float z) (is_integer z)))
   (loise-simplex:3d x y z (loise-simplex:options)))
  ((x y opts)
   (loise-simplex:2d x y opts)))

(defun simplex (x y z opts)
  (loise-simplex:3d x y z opts))

(defun simplex-point(coords opts)
  (loise-simplex:point coords opts))

(defun simplex-point(coords size mult)
  (loise-simplex:point coords size mult))

;; Data API
(defun data ()
  (data '()))

(defun data (opts)
  (let ((opts (loise-data:options opts)))
    (loise-data:matrix (loise-opts:get 'noise opts) opts)))

(defun data-row (matrix index)
  (proplists:get_value index matrix))

(defun data-cell (row point)
  (proplists:get_value point row))

;; ASCII API
(defun ascii ()
  (ascii '()))

(defun ascii (opts)
  (let ((opts (loise-ascii:options opts)))
    (loise-ascii:grid (loise-opts:get 'noise opts) opts)))

(defun format-ascii ()
  (io:format "~s~n" `(,(ascii))))

(defun format-ascii (opts)
  (io:format "~s~n" `(,(ascii opts))))

;; Image API

(defun image (filename)
  (image filename (loise-png:options)))

(defun image (filename overrides)
  (case (get-file-type filename overrides)
    ('png (let ((opts (loise-png:options overrides)))
            (loise-png:write-image filename (mref opts 'noise) opts)))
    ('egd (let ((opts (loise-egd:options overrides)))
            (loise-egd:write-image filename opts)))
    (type `#(error ,(io_lib:format "unsupported image type '%p'" (list type))))))

;; Common operations

(defun dim (opts) (mref opts 'dim))

(defun size (opts) (mref opts 'size))

(defun gradations (count) (loise-util:make-gradations count))

(defun round (float precision) (lutil-math:round float precision))

;; Project metadata

(defun version () (loise-state:get 'version))

(defun versions () (loise-state:get 'versions))

;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;; Supporting functions
;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

(defun get-file-type (filename opts)
  ;; XXX add type-extractor (read file extension, fall back to opts
  (mref opts 'output-format))