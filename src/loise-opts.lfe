(defmodule loise-opts
  (export all))

(defun get (opts)
  (maps:merge (loise-state:get) opts))

(defun get (key opts)
  (maps:get key (get opts) 'undefined))

(defun get (key opts default)
  (maps:get key (get opts) default))

(defun update-grades (opts)
  (case (loise-opts:get 'graded? opts)
    ('true (let* ((count (loise-opts:get 'grades-count opts))
                  (grades (loise-util:make-gradations count)))
             (mupd opts 'grades grades)))
    (_ opts)))

(defun update-sizes (opts)
  (let* ((w (get 'width opts))
         (h (get 'height opts))
         (dim `(,w ,h)))
    (clj:-> opts
            (mupd 'dim dim)
            (mupd 'size (list_to_tuple dim)))))

(defun update-colors (opts)
  (case (loise-opts:get 'graded? opts)
    ('true (let* ((grades (mref opts 'grades))
                  (ascii-map (loise-opts:get 'ascii-map opts))
                  (colors (loise-opts:get 'colors opts)))
             (if (or (== 'undefined ascii-map) (== 'undefined colors))
               opts
               (lists:zip grades
                          (lists:zip ascii-map colors)))))
    (_ opts)))

(defun update-scale-func (opts)
  (mupd opts
        'scale-func
        (loise-opts:get 'scale-func opts #'lutil-math:color-scale/2)))

(defun update-calculated-opts (opts)
  (loise-util:update-perm-table opts)
  (clj:-> opts
          (update-sizes)
          (update-grades)
          (update-colors)
          (update-scale-func)))

(defun value-range (opts)
  ;; Update as we support more types of noise ...
  (case (loise-opts:get 'noise opts)
    ('perlin (loise-perlin:value-range))
    ('simplex (loise-simplex:value-range))))
