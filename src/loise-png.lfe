(defmodule loise-png
  (export all))

;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;; Options and Defaults
;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

(defun default-png-width () 256)
(defun default-png-height () 128)
(defun default-png-mode () #(grayscale 8))
(defun default-png-palette () 'undefined)

(defun default-options ()
  (default-options #m()))

(defun default-options (overrides)
  (let ((png-opts `#m(output-backend png
                      output-type image
                      output-format png
                      width ,(default-png-width)
                      height ,(default-png-height)
                      png-mode ,(default-png-mode)
                      png-palette ,(default-png-palette))))
    (clj:-> (loise-state:get 'base-opts)
            (maps:merge (loise-state:get 'output-opts))
            (maps:merge png-opts)
            (maps:merge overrides)
            (loise-opts:update-calculated-opts))))

(defun options ()
  (options #m()))

(defun options (overrides)
  (case (maps:get 'ascii-opts (loise-state:get) 'undefined)
    ('undefined (default-options overrides))
    (stored-opts stored-opts)))

;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;; API
;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

(defun perlin-image (png opts)
  (perlin-image png (loise:size opts) opts))

(defun perlin-image (png end-point opts)
  (perlin-image png #(1 1) end-point opts))

(defun perlin-image (png start-point end-point opts)
  (image png #'loise-perlin:point/4 start-point end-point opts))

(defun simplex-image (png opts)
  (simplex-image png (loise:size opts) opts))

(defun simplex-image (png end-point opts)
  (simplex-image png #(1 1) end-point opts))

(defun simplex-image (png start-point end-point opts)
  (image png #'loise-simplex:point/4 start-point end-point opts))

(defun image (png noise-type)
  (image png noise-type (options)))

(defun image (png noise-type opts)
  (case noise-type
    ('perlin (perlin-image png opts))
    ('simplex (simplex-image png opts))))

(defun write-image (filename noise-type opts)
  (let* ((`#(ok ,file) (file:open filename '(write)))
         (png (png:create `#m(size ,(loise:size opts)
                                   mode #(grayscale 8)
                                   file ,file)))
         (data (image png noise-type opts)))
    (png:close png)
    (file:close file)))

;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;; Supporting functions
;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

(defun image
  ((png point-func (= `#(,start-x ,start-y) start) (= `#(,end-x ,end-y) end) opts)
   (let ((scale-func (mref opts 'scale-func))
         (mult (mref opts 'multiplier))
         (graded? (mref opts 'graded?))
         (grades (mref opts 'grades))
         (value-range (mref opts 'value-range)))
     (list-comp ((<- y (lists:seq start-y end-y)))
       (png:append png `#(row ,(list_to_binary (row point-func
                                                    scale-func
                                                    y
                                                    start
                                                    end
                                                    mult
                                                    graded?
                                                    grades
                                                    value-range
                                                    opts))))))))

(defun row
  ((point-func scale-func y `#(,start-x ,_) `#(,end-x ,end-y) mult graded? grades value-range opts)
   (list-comp ((<- x (lists:seq start-x end-x)))
     (loise-data:cell point-func
                      scale-func
                      `(,x ,y)
                      `(,end-x ,end-y)
                      mult
                      graded?
                      grades
                      value-range
                      opts))))

;;; Example from https://github.com/yuce/png/blob/master/examples/grayscale_8.escript,
;;; converted to LFE, for use in the REPL:
;;; 
;;; (set width 800)
;;; (set height 400)
;;; 
;;; (set width 100)
;;; (set height 100)
;;; 
;;; (random:seed)
;;; 
;;; (set `#(ok ,file) (file:open "grayscale_8.png" '(write)))
;;; (set png (png:create `#m(size #(,width ,height) mode #(grayscale 8) file ,file)))
;;; 
;;; (catch 
;;;   (list-comp ((<- y (lists:seq 1 height)))
;;;     (let ((row (list-comp ((<- x (lists:seq 1 width)))
;;;                  (random:uniform (+ 1 (trunc (* 255 (/ (+ (/ x width) (/ y height)) 2))))))))
;;;       (png:append png `#(row ,(list_to_binary row))))))
;;; 
;;; (png:close png)
;;; (file:close file)
