(defun main (args)
  (io:format "Running script '~s' with args ~p ...~n" `(,script-name ,args))
  (io:format "~p~n" `(,({{name}}:my-fun))))

(defun main ()
  (main script-args))

(main)
