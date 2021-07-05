(defmodule loise-rand
  (export all))

(defun default-alg ()
  ;; exrop is used since it is available in all supported versions of
  ;; Erlang
  'exrop)

(defun seed-tuple
  ((int) (when (is_integer int))
    `#(,int 0 0))
  ((`(,int))
    `#(,int 0 0))
  ((`(,int-1 ,int-2))
    `#(,int-1 ,int-2 0))
  ((`(,int-1 ,int-2 ,int-3))
    `#(,int-1 ,int-2 ,int-3)))

(defun permutation-table (state)
  (permutation-table state 256))

(defun permutation-table (state half-size)
  (element 1
           (lists:foldl
            (match-lambda ((_ `#(,acc ,st1))
                           (let ((`#(,num ,st2) (rand:uniform_s half-size st1)))
                             `#(,(cons num acc) ,st2))))
            `#(() ,state)
            (lists:seq 1 (* 2 half-size)))))

(defun update-perm-table (opts)
  "If 'random' is enabled (has a 'true' value), then don't use the default
  permutation table, but rather generate a new one."
  (case (mref opts 'random?)
    ('true
     (let* ((seed (mref opts 'seed))
            (state (rand:seed_s (default-alg) (seed-tuple seed)))
            (perm-table (permutation-table state)))
       (loise-state:set 'perm-table perm-table)))
    (_ 'ok)))

