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
   (set 2)
   (get-layer 1) (get-layers 0)
   (set-layer 2)
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
  `#m(ascii-opts undefined
      base-opts ,(loise-defaults:base-options)
      data-opts undefined
      grad-matrix ,(loise-defaults:gradient-matrix)
      layers #m()
      output-opts ,(loise-defaults:output-options)
      perm-table ,(loise-defaults:permutation-table)
      png-opts undefined
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
  ((`#(state set ,key ,val) state)
   `#(noreply ,(maps:put key val state)))
  ((`#(state set layer ,name ,data) state)
   (let ((layers (mref state 'layers)))
     `#(noreply ,(mupd state 'layers (mupd layers name data)))))
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
  ((`#(state get layer ,name) _from state)
   `#(reply ,(clj:get-in state `(layers ,name)) ,state))
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

(defun set-layer (name data)
  (gen_server:cast (SERVER) `#(state set layer ,name ,data)))

(defun get-layer (name)
  (gen_server:call (SERVER) `#(state get layer ,name)))

(defun get-layers ()
  (loise-state:get 'layers))
