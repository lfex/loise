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

(defun mix (a b t opts)
  (+ (* (- (loise-opts:mix-shift opts) t) a) (* t b)))

(defun fade (t opts)
  (* t t t
    (+ (loise-opts:fade-shift-2 opts)
       (* t (- (* t (loise-opts:fade-factor opts))
               (loise-opts:fade-shift-1 opts))))))

(defun get-gradient-index (a b c opts)
  (let ((perm (loise-opts:perm-table opts))
        (modulus (loise-opts:grad-modulus opts)))
    (rem (clj:->> (loise-util:index perm c)
                  (+ b)
                  (loise-util:index perm)
                  (+ a)
                  (loise-util:index perm))
         modulus)))

(defun get-noise-contribution (g x y z opts)
  (dot
    (loise-util:index (loise-opts:grad-matrix opts) g)
    x y z))

(defun corner-contribution (g x y z opts)
  (let* ((t (- 0.5 (* x x) (* y y) (* z z)))
         (t^2 (* t t)))
    (if (< t 0)
      0.0
      (* t^2 t^2 (dot
                   (index (loise-opts:grad-matrix opts) g)
                   x y z)))))

(defun colorize (name text opts)
  (if (loise-opts:color? opts)
    (call 'color name text)
    text))
