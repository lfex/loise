(defmodule loise-traverse
  (export all))

(defun sup () 'loise-traver-sup)
(defun default-start-point () #(0 0))
(defun default-count () 100)
(defun default-type () 'random-neighbor)

(defun options ()
  (options #m()))

(defun options (overrides)
  (maps:merge `#m(start ,(default-start-point)
                  end undefined
                  count ,(default-count)
                  type ,(default-type)
                  duration undefined)
              overrides))

(defun brownian (layer-name)
  (brownian layer-name #m(type random-neighbor)))

(defun brownian (layer-name overrides)
  (let ((`#(ok ,pid) (supervisor:start_child
                      (sup)
                      (list layer-name
                            (loise-state:get-layer layer-name)
                            (options overrides)))))
    (prog1
      (list
       (loise-traver-work:ping pid)
       (loise-traver-work:get pid))
      (supervisor:terminate_child
       (sup)
       pid))))