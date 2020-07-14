(defmodule {{name}}-app
  (behaviour gen_server)
  (export
    ;; app implementation
    (start 2)
    (stop 0)))

;;; --------------------------
;;; application implementation
;;; --------------------------

(defun start (_type _args)
  (logger:set_application_level '{{name}} 'all)
  (logger:info "Starting {{name}} application ...")
  ({{name}}-sup:start_link))

(defun stop ()
  ({{name}}-sup:stop)
  'ok)
