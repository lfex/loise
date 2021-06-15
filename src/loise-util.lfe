(defmodule loise-util
  (export all))

(defun version ()
  (version 'loise))

(defun version (app-name)
  (application:load app-name)
  (case (application:get_key app-name 'vsn)
    (`#(ok ,vsn) vsn)
    (default default)))

(defun version-arch ()
  `#(architecture ,(erlang:system_info 'system_architecture)))

(defun version+name (app-name)
  `#(,app-name ,(version app-name)))

(defun versions-rebar ()
  `(,(version+name 'rebar)
    ,(version+name 'rebar3_lfe)))

(defun versions-langs ()
  `(,(version+name 'lfe)
    #(erlang ,(erlang:system_info 'otp_release))
    #(emulator ,(erlang:system_info 'version))
    #(driver ,(erlang:system_info 'driver_version))))

(defun versions (bkend)
  (lists:append `((,(version+name 'loise))
                  ,(versions-langs)
                  ,(versions-rebar)
                  (,(version-arch)))))

(defun int-list->str (data)
  (lists:flatten
    (lists:map #'integer_to_list/1 data)))

(defun seed-tuple
  ((int) (when (is_integer int))
    `#(,int 0 0))
  ((`(,int))
    `#(,int 0 0))
  ((`(,int-1 ,int-2))
    `#(,int-1 ,int-2 0))
  ((`(,int-1 ,int-2 ,int-3))
    `#(,int-1 ,int-2 ,int-3)))

(defun random-permutation-table (state)
  (element 1
           (lists:foldl
            (match-lambda ((_ `#(,acc ,st1))
                           (let ((`#(,num ,st2) (rand:uniform_s 256 st1)))
                             `#(,(cons num acc) ,st2))))
            `#(() ,state)
            (lists:seq 1 512))))

(defun update-perm-table-options (opts)
  "If 'random' is enabled (has a 'true' value), then don't use the default
  permutation table, but rather generate a new one."
  (case (proplists:get_value 'random opts)
    ('true
     (let ((state (rand:seed_s 'exsss (seed-tuple (proplists:get_value 'seed opts)))))
      (++ `(#(perm-table ,(random-permutation-table state))) opts)))
    (_ opts)))

(defun get-color-map (opts)
  (lists:zip
    (proplists:get_value 'ascii-map opts)
    (proplists:get_value 'colors opts)))

(defun get-dimensions (opts)
  `(,(proplists:get_value 'width opts)
    ,(proplists:get_value 'height opts)))

(defun index (data position)
  "A list-based version of element-index."
  (lists:nth (+ 1 position) data))

(defun dot (grad x y z)
   (+ (* (index grad 0) x)
      (* (index grad 1) y)
      (* (index grad 2) z)))

(defun make-gradations
  "The number 'count' passed in this function represents the total number of
  gradations we expect to get back. The 'lutil-math:get-gradations' function
  expects a different parameter: 'divisions'. In other words, 'Tell me how many
  divisions you want in the given range.' These two parameters differ by one.

  Loise uses the same color range that Erlang's egd does: 0 to 255."
  ((count) (when (< count 2))
   #(error "there must be two or more gradations"))
  ((count)
   (lutil-math:get-gradations '(0 255) (- count 1))))

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

(defun mix (a b t options)
  (+ (* (- (proplists:get_value 'mix-shift options) t) a) (* t b)))

(defun fade (t options)
  (* t t t
    (+ (proplists:get_value 'fade-shift-2 options)
       (* t (- (* t (proplists:get_value 'fade-factor options))
               (proplists:get_value 'fade-shift-1 options))))))

(defun get-gradient-index (a b c options)
  (let ((perm (proplists:get_value 'perm-table options))
        (modulus (proplists:get_value 'grad-modulus options)))
    (rem (clj:->> (loise-util:index perm c)
                  (+ b)
                  (loise-util:index perm)
                  (+ a)
                  (loise-util:index perm))
         modulus)))

(defun get-noise-contribution (g x y z options)
  (loise-util:dot
    (loise-util:index (proplists:get_value 'grad-matrix options) g)
    x y z))

(defun corner-contribution (g x y z options)
  (let* ((t (- 0.5 (* x x) (* y y) (* z z)))
         (t^2 (* t t)))
    (if (< t 0)
      0.0
      (* t^2 t^2 (loise-util:dot
                   (loise-util:index (proplists:get_value 'grad-matrix options) g)
                   x y z)))))
