(defmodule loise-traver-sup
  (beehaviour supervisor)
  ;; supervisor implementation
  (export
   (start_link 0)
   (stop 0))
  ;; callback implementation
  (export
   (init 1)))

;;; ----------------
;;; config functions
;;; ----------------

(defun SERVER () (MODULE))
(defun supervisor-opts () '())
(defun sup-flags ()
  `#M(strategy simple_one_for_one
      intensity 0
      period 1))

;;; -------------------------
;;; supervisor implementation
;;; -------------------------

(defun start_link ()
  (supervisor:start_link `#(local ,(SERVER))
                         (MODULE)
                         (supervisor-opts)))

(defun stop ()
  (gen_server:call (SERVER) 'stop))

;;; -----------------------
;;; callback implementation
;;; -----------------------

(defun init (args)
  `#(ok #(,(sup-flags)
          ,(child args))))

;;; -----------------
;;; private functions
;;; -----------------

(defun child (args)
  `(#m(id traversal_worker
       start #(loise-traver-work start_link ,args)
       shutdown brutal_kill
       restart temporary
       type worker)))
