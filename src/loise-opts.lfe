(defmodule loise-opts
  (export all))

(include-lib "include/options.lfe")

(defun noise (opts)
  (proplists:get_value 'noise opts))

(defun width (opts)
  (proplists:get_value 'width opts))

(defun height (opts)
  (proplists:get_value 'height opts))

(defun dimensions (opts)
  `(,(width opts) ,(height opts)))

(defun size (opts)
  (list_to_tuple (dimensions opts)))

(defun scale-func (opts)
  (proplists:get_value 'scale-func opts #'lutil-math:color-scale/2))

(defun grades (opts)
  (proplists:get_value 'grades opts))

(defun grades-count (opts)
  (proplists:get_value 'grades-count opts))

(defun graded? (opts)
  (proplists:get_value 'graded? opts))

(defun multiplier (opts)
  (proplists:get_value 'multiplier opts))

(defun get-png-mode (opts)
  (proplists:get_value 'png-mode opts))

(defun colors (opts)
  (proplists:get_value 'colors opts))

(defun ascii-map (opts)
  (proplists:get_value 'ascii-map opts))

(defun color-map (opts)
  (lists:zip (grades opts)
             (lists:zip (ascii-map opts)
                        (colors opts))))

(defun output-format (opts)
  (proplists:get_value 'output-format opts))

(defun data-format (opts)
  (proplists:get_value 'data-format opts))

(defun fade-factor (opts)
  (proplists:get_value 'fade-factor opts))

(defun fade-shift-1 (opts)
  (proplists:get_value 'fade-shift-1 opts))

(defun fade-shift-2 (opts)
  (proplists:get_value 'fade-shift-2 opts))

(defun mix-shift (opts)
  (proplists:get_value 'mix-shift opts))

(defun seed (opts)
  (proplists:get_value 'seed opts))

(defun random? (opts)
  (proplists:get_value 'random? opts))

(defun color? (opts)
  (proplists:get_value 'color opts))

(defun grad-modulus (opts)
  (proplists:get_value 'grad-modulus opts))

(defun grad-matrix (opts)
  (proplists:get_value 'grad-matrix opts))

(defun skew-factor (opts)
  (proplists:get_value 'skew-factor opts) (default-skew-factor))

(defun unskew-factor (opts)
  (proplists:get_value 'unskew-factor opts) (default-unskew-factor))

(defun simplex-scale (opts)
  (proplists:get_value 'simplex-scale opts))

(defun perm-table (opts)
  (proplists:get_value 'perm-table opts))

(defun update-perm-table (opts)
  "If 'random' is enabled (has a 'true' value), then don't use the default
  permutation table, but rather generate a new one."
  (case (random? opts)
    ('true
     (let ((state (rand:seed_s 'exsss (loise-util:seed-tuple (seed opts)))))
      (++ `(#(perm-table ,(loise-util:random-permutation-table state))) opts)))
    (_ opts)))

(defun value-range (opts)
  ;; Update as we support more types of noise ...
  (case (loise-opts:noise opts)
    ('perlin (loise-perlin:value-range))
    ('simplex (loise-simplex:value-range))))
