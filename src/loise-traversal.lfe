(defmodule loise-traversal
  (export all))

(defun sup () 'loise-traver-sup)
(defun default-start-point () #(0 0))
(defun default-count () 100)
(defun default-type () 'random-neighbor)
(defun default-radius () 1)
(defun default-include-center? () 'false)
(defun default-reverse? () 'true)
(defun default-func () #'loise-traverse:random-walk/2)

(defun options ()
  (options #m()))

(defun options (overrides)
  (maps:merge `#m(start ,(default-start-point)
                  end undefined
                  count ,(default-count)
                  type ,(default-type)
                  duration undefined
                  radius ,(default-radius)
                  include-center? ,(default-include-center?)
                  reverse? ,(default-reverse?))
              overrides))

(defun neighbors (point opts)
  (let ((result (lists:map (lambda (p) (neighbor-check point p opts))
                           (lists:flatten
                            (neighbors point (mref opts 'radius) opts)))))
    (lists:filter (lambda (r) (not (== r 'false)))
                  (lists:usort result))))

(defun neighbors
  (((= `#(,x ,y) point) radius opts)
   (let ((center? (maps:get 'include-center? opts (default-include-center?))))
     (list-comp ((<- xn (lists:seq (- x radius) (+ x radius))))
       (list-comp ((<- yn (lists:seq (- y radius) (+ y radius))))
         (let ((pn `#(,xn ,yn)))
           (if (and (== pn point) (not center?))
             'false
             pn)))))))

(defun neighbor-check
  ((center point _) (when (== center point))
   'false)
  ((p0 `#(,x ,y) opts) (when (< x 0))
   (neighbor-check p0 `#(0 ,y) opts))
  ((p0 `#(,x ,y) opts) (when (< y 0))
   (neighbor-check p0 `#(,x 0) opts))
  ((p0 `#(,x ,y) (= `#m(width ,w) opts)) (when (>= x w))
   (neighbor-check p0 `#(,(- w 1) ,y) opts))
  ((p0 `#(,x ,y) (= `#m(height ,h) opts)) (when (>= y h))
   (neighbor-check p0 `#(,x ,(- h 1)) opts))
  ((_ point _) point))

(defun random-step
  ((_ `#(,acc ,p1 ,st1 ,opts))
   (let* ((neighbs (neighbors p1 opts))
          (`#(,p2 ,st2) (loise-rand:choose neighbs st1)))
     `#(,(cons p2 acc) ,p2 ,st2 ,opts))))

(defun random-walk (start-point opts)
  (let* ((count (mref opts 'count))
         (st0 (loise-rand:state opts))
         (`#(,steps ,_ ,st1 ,_) (lists:foldl #'random-step/2
                                             `#((,start-point) ,start-point ,st0 ,opts)
                                             (lists:seq 1 count))))
    (loise-rand:set-state st1)
    steps))

(defun get-func (type)
  (case type
    ('random-neighbor #'loise-traverse:random-walk/2)
    (_ (default-func))))

(defun brownian (layer-name start-point)
  (brownian layer-name start-point #m(type random-neighbor)))

(defun brownian (layer-name start-point overrides)
  (let* ((opts (clj:-> (options)
                       (maps:merge (loise-state:get-layer-opts layer-name))
                       (maps:merge overrides)))
         (traverse-func (get-func (mref opts 'type)))
         (`#(ok ,pid) (supervisor:start_child
                       (sup)
                       (list layer-name
                             (loise-state:get-layer layer-name)
                             opts))))
    (prog1
        (let ((hash (crypto:hash
                     'sha
                     (list_to_binary
                      (io_lib:format "~p~p~p"
                                     (list traverse-func start-point opts))))))
          (loise-traver-work:execute pid hash traverse-func `(,start-point ,opts))
          (let ((results (loise-traver-work:get-results pid hash)))
            (if (mref opts 'reverse?)
              (lists:reverse results)
              results)))
      (supervisor:terminate_child
       (sup)
       pid))))
