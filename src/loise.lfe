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
   (data 0) (data 1)
   (data-cell 2)
   (data-row 2)
   (add-layer 1) (add-layer 2)
   (get-layer 1) (get-layer 2)
   (get-path 2) (get-path 3)
   (traverse 2) (traverse 3))
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
  ((x overrides)
   (loise-perlin:1d x (loise-perlin:options overrides))))

(defun perlin
  ((x y z) (when (orelse (is_float z) (is_integer z)))
   (loise-perlin:3d x y z (loise-perlin:options)))
  ((x y overrides)
   (loise-perlin:2d x y (loise-perlin:options overrides))))

(defun perlin (x y z overrides)
  (loise-perlin:3d x y z (loise-perlin:options overrides)))

(defun perlin-point (coords overrides)
  (let ((opts (loise-perlin:options overrides)))
    (loise-perlin:point coords
                        (mref opts 'size)
                        (mref opts 'multiplier)
                        opts)))

(defun perlin-point (coords size mult)
  (loise-perlin:point coords size mult (loise-perlin:options)))

(defun simplex (x)
  (loise-simplex:1d x (loise-simplex:options)))

(defun simplex
  ((x y) (when (orelse (is_float y) (is_integer y)))
   (loise-simplex:2d x y (loise-simplex:options)))
  ((x overrides)
   (loise-simplex:1d x (loise-simplex:options overrides))))

(defun simplex
  ((x y z) (when (orelse (is_float z) (is_integer z)))
   (loise-simplex:3d x y z (loise-simplex:options)))
  ((x y overrides)
   (loise-simplex:2d x y (loise-simplex:options overrides))))

(defun simplex (x y z opts)
  (loise-simplex:3d x y z opts))

(defun simplex-point (coords overrides)
  (let ((opts (loise-simplex:options overrides)))
    (loise-simplex:point coords
                         (mref opts 'size)
                         (mref opts 'multiplier)
                         opts)))

(defun simplex-point (coords size mult)
  (loise-simplex:point coords size mult (loise-simplex:options)))

;; Data API
(defun data ()
  (data #m()))

(defun data (overrides)
  (let ((opts (loise-data:options overrides)))
    (loise-data:matrix (mref opts 'noise) opts)))

(defun data-cell (row point)
  (proplists:get_value point row))

(defun data-row (matrix index)
  (proplists:get_value index matrix))

(defun add-layer (name)
  (add-layer name '#m()))

(defun add-layer
  ((name overrides) (when (is_list name))
   (add-layer (list_to_atom name) (loise-data:options overrides)))
  ((name overrides) (when (is_atom name))
   (loise-state:set-layer name (loise-data:options overrides))))

(defun get-layer (layer-name)
  (loise-state:get-layer layer-name))

(defun get-layer (layer-name opts)
  (let ((layer (get-layer layer-name)))
    (if (== 'true (maps:get 'flatten opts 'false))
      (loise-data:flatten layer)
      layer)))

(defun get-path (layer-name type)
  (get-path layer-name type #(0 0)))

(defun get-path (layer-name type start-point)
  (let ((opts (loise-state:get-layer-opts layer-name)))
    (call 'loise-traversal type layer-name start-point opts)))

(defun traverse
  ((layer-name points) (when (is_list points))
   (let ((opts (loise-state:get-layer-opts layer-name)))
     (loise-traversal:walk layer-name points opts)))
  ((layer-name type) (when (is_atom type))
   (traverse layer-name (get-path layer-name type))))

(defun traverse (layer-name type start-point)
  (traverse layer-name (get-path layer-name type start-point)))

;; ASCII API
(defun ascii ()
  (ascii #m()))

(defun ascii (overrides)
  (let ((opts (loise-ascii:options overrides)))
    (loise-ascii:grid (mref opts 'noise) opts)))

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
  (maps:get 'output-format opts (loise-defaults:default-image-format)))
