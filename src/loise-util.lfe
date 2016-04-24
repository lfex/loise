(defmodule loise-util
  (export all))

(defun get-version ()
  (lr3-ver-util:get-app-version 'loise))

(defun get-versions ()
  (++ (lr3-ver-util:get-versions)
      `(#(loise ,(get-version)))))

(defun random-permutation-table ()
  (lists:map
    (lambda (_)
      (- (random:uniform 256) 1))
    (lists:seq 1 512)))

(defun get-seed
  ((int) (when (is_integer int))
    `#(,int 0 0))
  ((`(,int))
    `#(,int 0 0))
  ((`(,int-1 ,int-2))
    `#(,int-1 ,int-2 0))
  ((`(,int-1 ,int-2 ,int-3))
    `#(,int-1 ,int-2 ,int-3)))

(defun update-perm-table-options (options)
  "If 'random' is enabled (has a 'true' value), then don't use the default
  permutation table, but rather generate a new one."
  (case (proplists:get_value 'random options)
    ('true
      (random:seed (get-seed (proplists:get_value 'seed options)))
      (++ `(#(perm-table ,(random-permutation-table))) options))
    (_ options)))

(defun get-ascii-map (options)
  (lists:zip
    (proplists:get_value 'grades options)
    (proplists:get_value 'ascii-map options)))

(defun get-color-map (options)
  (lists:zip
    (proplists:get_value 'ascii-map options)
    (proplists:get_value 'colors options)))

(defun get-dimensions (options)
  `(,(proplists:get_value 'width options)
    ,(proplists:get_value 'height options)))

(defun index (data position)
  "A list-based version of element-index."
  (lists:nth (+ 1 position) data))

(defun dot (grad x y z)
   (+ (* (index grad 0) x)
      (* (index grad 1) y)
      (* (index grad 2) z)))

(defun get-gradations (count)
  "The number 'count' passed in this function represents the total number of
  gradations we expect to get back. The 'lutil-math:get-gradations' function
  expects a different parameter: 'divisions'. In other words, 'Tell me how many
  divisions you want in the given range.' These two parameters differ by one.

  Loise uses the same color range that Erlang's egd does: 0 to 255."
  (lutil-math:get-gradations '(0 255) (- count 1)))

(defun partial
  "Something akin to a partial that will suit our purposes ;-)
  See unit tests for usage."
  ((f (cons arg1 (cons arg2 '())))
    (lambda (args)
      (apply f
        (++ `(,arg1 ,arg2) args))))
  ((f arg)
    (lambda (args)
      (apply f
        (cons arg args)))))
