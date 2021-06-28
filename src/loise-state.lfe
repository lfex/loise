(defmodule loise-state
  (behaviour gen_server)
  ;; gen_server implementation
  (export
   (start_link 0)
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
   (echo 1)
   (get 0) (get 1) (get 2)
   (pid 0)
   (ping 0)
   (set 2)))

;;; ----------------
;;; config functions
;;; ----------------

(defun SERVER () (MODULE))
(defun genserver-opts () '())
(defun unknown-command () #(error "Unknown command."))

(defun initial-state ()
  `#m(base-opts ,(loise-defaults:base-options)
      grad-matrix ,(loise-defaults:gradient-matrix)
      output-opts ,(loise-defaults:output-options)
      perm-table ,(loise-defaults:permutation-table)
      version ,(loise-util:version)
      versions ,(loise-util:versions)))

;;; -------------------------
;;; gen_server implementation
;;; -------------------------

(defun start_link ()
  (gen_server:start_link `#(local ,(SERVER))
                         (MODULE)
                         (initial-state)
                         (genserver-opts)))

(defun stop ()
  (gen_server:call (SERVER) '#(stop)))

;;; -----------------------
;;; callback implementation
;;; -----------------------

(defun init (state)
  `#(ok ,state))

(defun handle_cast
  ((`#(state set ,key ,val) _from state)
   `#(noreply ,(mref state key val)))
  ((_msg state)
   `#(noreply ,state)))

(defun handle_call
  ((`#(echo ,msg) _from state)
   `#(reply ,msg ,state))
  ((`#(ping) _from state)
   `#(reply pong ,state))
  (('#(stop) _from state)
   `#(stop shutdown ok ,state))
  (('#(state get) _from state)
   `#(reply ,state ,state))
  ((`#(state get ,key) _from state)
   `#(reply ,(maps:get key state 'undefined) ,state))
  ((`#(state get ,key ,default) _from state)
   `#(reply ,(maps:get key state default) ,state))
  ((message _from state)
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

(defun echo (msg)
  (gen_server:call (SERVER) `#(echo ,msg)))

(defun get ()
  (gen_server:call (SERVER) '#(state get)))

(defun get (key)
  (gen_server:call (SERVER) `#(state get ,key)))

(defun get (key default)
  (gen_server:call (SERVER) `#(state get ,key ,default)))

(defun pid ()
  (erlang:whereis (SERVER)))

(defun ping ()
  (gen_server:call (SERVER) '#(ping)))

(defun set (key value)
  (gen_server:cast (SERVER) `#(state set ,key ,value)))
