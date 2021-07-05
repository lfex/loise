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
   (set-layer 1) (set-layer 2) (set-layer 3)
   (render-ascii 1)
   (render-image 2)
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
     `#(noreply ,(mupd state 'layers (mset layers name data)))))
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
  ((`#(state get layer ,name) _from state)
   ;; Note: attemtping to use clj:get-in instead of the nested map calls
   ;; results in a dialyzer error not being able to read Erlang data from
   ;; clj.beam ... depsite there being other calls in loise that use that
   ;; module (?)
   `#(reply ,(maps:get name (mref state 'layers) 'undefined) ,state))
  ((`#(state get ,key ,default) _from state)
   `#(reply ,(maps:get key state default) ,state))
  ((`#(state render ascii ,layer-name) _from state)
   ;; XXX Depends upon: https://github.com/lfex/loise/issues/55
   ;; XXX pull data from state
   ;; XXX convert it to ASCII format or insert it into the appropriate point
   ;;     of the ASCII-processing pipeline
   `#(reply not-implemented ,state))
  ((`#(state render image ,layer-name ,filename) _from state)
   ;; XXX Depends upon: https://github.com/lfex/loise/issues/55
   ;; XXX pull data from state
   ;; XXX convert it to ASCII format or insert it into the appropriate point
   ;;     of the ASCII-processing pipeline
   `#(reply not-implemented ,state))
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

(defun set-layer (name)
  (set-layer name #m()))

(defun set-layer (name overrides)
  (set-layer name (loise:data overrides) overrides))

(defun set-layer (name data _)
  (gen_server:cast (SERVER) `#(state set layer ,name ,data)))

(defun get-layer (name)
  (gen_server:call (SERVER) `#(state get layer ,name)))

(defun get-layers ()
  (loise-state:get 'layers))

(defun render-image (layer-name filename)
  (gen_server:call (SERVER) `#(state render image ,layer-name ,filename)))

(defun render-ascii (layer-name)
  (io:format "~p~n"
             (list (gen_server:call (SERVER) `#(state render ascii ,layer-name)))))
