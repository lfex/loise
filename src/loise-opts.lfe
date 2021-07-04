(defmodule loise-opts
  (export all))

(defun trigger-opts ()
  "The set of options returned by this function impact calculated values of
  other options."
  (sets:from_list '(width
                    height
                    grades-count
                    graded?
                    ascii-map
                    grades
                    colors)))

(defun triggers-intersection (opts)
  (sets:intersection (sets:from_list (maps:keys opts))
                     (trigger-opts)))

(defun empty-triggers? (opts)
  (sets:is_empty
   (triggers-intersection opts)))

(defun triggers? (opts)
  (not (empty-triggers? opts)))

(defun trigger-update?
  (((= #m() map)) (when (== (map_size map) 0))
   'false)
  (('())
   'false)
  ((opts) (when (is_map opts))
   (triggers? opts))
  ((_)
   'false))

(defun match-list
  (('())
   'false))

(defun tu
  ((())
   'false))

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
               (mupd opts
                     'color-map
                     (lists:zip grades
                                (lists:zip ascii-map colors))))))
    (_ opts)))

(defun update-scale-func (opts)
  (let* ((val (loise-opts:get 'scale-func opts))
         (checked (case val
                    ('undefined  #'lutil-math:color-scale/2)
                    (_ val))))
  (mupd opts
        'scale-func
        checked)))

(defun update-calculated (opts)
  (loise-util:update-perm-table opts)
  (clj:-> opts
          (update-sizes)
          (update-grades)
          (update-colors)
          (update-scale-func)))

(defun maybe-update (opts)
  (if (trigger-update? opts)
    (update-calculated opts)
    opts))

(defun value-range (opts)
  ;; Update as we support more types of noise ...
  (case (maps:get 'value-range opts 'undefined)
    ('undefined (case (loise-opts:get 'noise opts)
                  ('perlin (loise-perlin:value-range))
                  ('simplex (loise-simplex:value-range))))
    (val val)))