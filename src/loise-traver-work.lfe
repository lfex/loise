(defmodule loise-traver-work
  (behaviour gen_server)
  ;; gen_server implementation
  (export
   (start_link 3)
   (stop 0))
  ;; callback implementation
  (export
   (init 1)
   (handle_call 3)
   (handle_cast 2)
   (handle_info 2)
   (terminate 2)
   (code_change 3))
  ;; server API
  (export
   (echo 2)
   (get 1) (get 2) (get 3)
   (ping 1)))

;;; ----------------
;;; config functions
;;; ----------------

(defun SERVER () (MODULE))
(defun genserver-opts () '())
(defun unknown-command () #(error "Unknown command."))

;;; -------------------------
;;; gen_server implementation
;;; -------------------------

(defun start_link (id matrix-data opts)
  (let ((worker-name (list_to_atom (++ "traversal-for-" (atom_to_list id)))))
    (gen_server:start_link `#(local ,id)
                           (MODULE)
                           `#m(matrix ,matrix-data
                               start ,(maps:get 'start opts 'undefined)
                               end ,(maps:get 'end opts 'undefined)
                               type ,(maps:get 'type opts 'undefined)
                               count ,(maps:get 'count opts 'undefined)
                               duration ,(maps:get 'duration opts 'undefined))
                           (genserver-opts))))

(defun stop ()
  (gen_server:call (SERVER) '#(stop)))

;;; -----------------------
;;; callback implementation
;;; -----------------------

(defun init (state)
  `#(ok ,state))

(defun handle_cast
  ((_msg state)
   `#(noreply ,state)))

(defun handle_call
  ((`#(echo ,msg) _from state)
   `#(reply ,msg ,state))
  (('#(state get) _from state)
   `#(reply ,state ,state))
  ((`#(state get ,key) _from state)
   `#(reply ,(maps:get key state 'undefined) ,state))
  ((`#(state get layer ,name) _from state)
   ;; Note: attemtping to use clj:get-in instead of the nested map calls
   ;; results in a dialyzer error not being able to read Erlang data from
   ;; clj.beam ... depsite there being other calls in loise that use that
   ;; module (?)
   `#(reply ,(maps:get name (mref state 'layers) 'undefined) ,state))
  ((`#(state get ,key ,default) _from state)
   `#(reply ,(maps:get key state default) ,state))
  ((`#(ping) _from state)
   `#(reply pong ,state))
  (('#(stop) _from state)
   `#(stop shutdown ok ,state))
  ((_ _from state)
   `#(reply ,(unknown-command) ,state)))

(defun handle_info
  ((`#(EXIT ,_from normal) state)
   `#(noreply ,state))
  ((`#(EXIT ,pid ,reason) state)
   (io:format "Process ~p exited! (Reason: ~p)~n" `(,pid ,reason))
   `#(noreply ,state))
  ((_msg state)
   `#(noreply ,state)))

(defun terminate (_reason _state)
  'ok)

(defun code_change (_old-version state _extra)
  `#(ok ,state))

;;; --------------
;;; our server API
;;; --------------

(defun echo (pid msg)
  (gen_server:call pid `#(echo ,msg)))

(defun get (pid)
  (gen_server:call pid '#(state get)))

(defun get (pid key)
  (gen_server:call pid `#(state get ,key)))

(defun get (pid key default)
  (gen_server:call pid `#(state get ,key ,default)))

(defun ping (pid)
  (gen_server:call pid '#(ping)))
