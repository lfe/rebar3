(defmodule process
  (export (main 1)))

(defun main (args)
  "Process command line arguments"
  (lfe_io:format "Processing ~p arguments~n" (list (length args)))

  (case args
    ((list input output)
     (process-file input output))
    (_
     (show-usage)))

  0)  ; Exit code

(defun process-file (input output)
  (lfe_io:format "Reading from: ~s~n" (list input))
  (lfe_io:format "Writing to: ~s~n" (list output))
  ;; Your processing logic here
  'ok)

(defun show-usage ()
  (lfe_io:format "Usage: rebar3 lfe run -- INPUT OUTPUT~n"))
