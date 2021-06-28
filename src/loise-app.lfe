(defmodule loise-app
  (behaviour application)
  ;; app implementation
  (export
   (start 2)
   (stop 0)))

;;; --------------------------
;;; application implementation
;;; --------------------------

(defun start (_type _args)
  (logger:info "Starting loise application ...")
  (loise-sup:start_link))

(defun stop ()
  (loise-sup:stop)
  'ok)
