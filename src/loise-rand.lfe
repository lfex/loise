(defmodule loise-rand
  (export all))

(defun state-key () 'rand-state)

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

(defun init-state (opts)
  (rand:seed_s (default-alg) (seed-tuple (mref opts 'seed))))

(defun set-state (st)
  (loise-state:set (state-key) st))

(defun state (opts)
  (let ((key (state-key)))
    (case (loise-state:get key)
      ('undefined (let ((st (init-state opts)))
                    (set-state st)
                    st))
      (st st))))

(defun choose (list-data)
  (let* ((idx (rand:uniform (length list-data))))
    (lists:nth idx list-data)))

(defun choose (list-data st1)
  (let ((`#(,idx ,st2) (rand:uniform_s (length list-data) st1)))
    `#(,(lists:nth idx list-data) ,st2)))

(defun choose* (list-data st)
  (element 1 (choose list-data st)))

(defun choose-next*
  ((_count `#(,acc ,list-data ,st1))
   (let ((`#(,choice ,st2) (choose list-data st1)))
     `#(,(cons choice acc) ,list-data ,st2))))

(defun choose* (list-data count st0)
  (let ((`#(,choices ,_ ,st1) (lists:foldl #'choose-next*/2
                                        `#(() ,list-data ,st0)
                                        (lists:seq 1 count))))
    (set-state st1)
    choices))

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
            (st (state opts))
            (perm-table (permutation-table st)))
       (loise-state:set 'perm-table perm-table)))
    (_ 'ok)))
