(defmodule loise-png
  (export all))

(include-lib "include/options.lfe")

;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;; Options and Defaults
;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

(defun options ()
  (options '()))

(defun options (overrides)
  (let* ((opts (default-png-options overrides))
         (grades-count (loise-opts:grades-count opts))
         (grades (loise-util:make-gradations grades-count)))
    ;; `(#(grades ,(lists:map (lambda (x) (+ 1 x)) grades)))
    (++ `(#(grades ,grades))
        (loise-opts:update-perm-table opts))))

;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;; API
;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

(defun perlin-image (png opts)
  (perlin-image png (loise:size opts) opts))

(defun perlin-image (png end-point opts)
  (perlin-image #(1 1) end-point opts))

(defun perlin-image (png start-point end-point opts)
  (make-image png #'loise-perlin:point/4 start-point end-point opts))

(defun simplex-image (png opts)
  (simplex-image png (loise:size opts) opts))

(defun simplex-image (png end-point opts)
  (simplex-image png #(1 1) end-point opts))

(defun simplex-image (png start-point end-point opts)
  (make-image png #'loise-simplex:point/4 start-point end-point opts))

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

(defun generate-config (opts)
  'tbd)

(defun point-data (point-func point max mult grades opts)
  ;;(io:format "Point: ~p~nMax: ~p~nMultiplier: ~p~nGrades: ~p~n" (list point max mult grades))
  ;;(io:format "Args: ~p~n" (list (list point max mult opts)))
  (let* ((value (apply point-func (list point max mult opts)))
         (scaled (lutil-math:color-scale value (loise-opts:value-range opts))))
    (if (loise-opts:graded? opts)
      (lutil-math:get-closest scaled grades)
      scaled)))

(defun make-image
  ((png point-func (= `#(,start-x ,start-y) start) (= `#(,end-x ,end-y) end) opts)
   (let ((mult (loise-opts:multiplier opts))
         (grades (loise-opts:grades opts)))
     (list-comp ((<- y (lists:seq start-y end-y)))
       (let ((row (list-comp ((<- x (lists:seq start-x end-x)))
                    (round
                     (point-data point-func
                                 `(,x ,y)
                                 `(,end-x ,end-y)
                                 mult
                                 grades
                                 opts)))))
         (png:append png `#(row ,(list_to_binary row))))))))

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
