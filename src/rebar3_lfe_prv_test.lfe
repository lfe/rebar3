(defmodule rebar3_lfe_prv_test
  (behaviour provider)
  (export all))

(defun namespace () 'lfe)
(defun provider-name () 'test)
(defun short-desc () "The LFE rebar3 test plugin.")
(defun deps () '(#(default app_discovery)
                 #(default compile)
                 #(lfe compile)
                 #(default lock)))

;;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;; Public API
;;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

(defun init (state)
  (let* ((opts `(#(name ,(provider-name))
                 #(module ,(MODULE))
                 #(namespace ,(namespace))
                 #(opts (#(test-type #\t "test-type" atom
                           ,(++ "Type of test to run. Valid types are: "
                                (format-test-types)
                                ". If no type is provided, '"
                                (format-default-type)
                                "' is assumed."))))
                 #(deps ,(deps))
                 #(example "rebar3 lfe test")
                 #(short_desc ,(short-desc))
                 #(desc ,(info (short-desc)))
                 #(bare true)))
         (provider (providers:create opts)))
    `#(ok ,(rebar_state:add_provider state provider))))

(defun do (state)
  (rebar_api:debug "Paths: ~n~p" `(,(code:get_path)))
  (rebar_api:info "Running test framework ...\n" '())

  (case (effective-test-type state)
    (`all
      (ltest-runner:all))
    (`unit
      (ltest-runner:unit))
    (`system
      (ltest-runner:system))
    (`integration
      (ltest-runner:integration))
    (type
      (rebar_api:error "Unknown test-type value: ~p" `(,type)))
    (_
      (rebar_api:error "Unknown option(s) or argument(s)." '())))
  `#(ok ,state))

(defun format_error (reason)
  (io_lib:format "~p" `(,reason)))

;;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;; Internal functions
;;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

(defun info (desc)
  (io_lib:format
    (++ "~n~s~n~n"
        "Run the unit, system, and integration tests "
        "for the project.~n"
        "~n")
    `(,desc)))

(defun default-type () 'all)

(defun format-default-type ()
  (atom_to_list (default-type)))

(defun legal-test-types ()
  "Get the list of allowed test types."
  `(unit system integration ,(default-type)))

(defun format-test-types ()
  "Format the list of allowed types in a manner suitable for inclusion in help
  text."
  (string:join (lists:map #'atom_to_list/1 (legal-test-types)) ", "))

(defun effective-test-type (state)
  "Calculate effective test type based on command parsed args, rebar.config and
  default test-type value"
  (let* ((cmd-args (element 1 (rebar_state:command_parsed_args state)))
         (state-args (rebar_state:get state (provider-name) []))
         (type-from-cmd (proplists:get_value 'test-type cmd-args))
         (type-from-state (proplists:get_value 'test-type state-args)))
    (if (=:= 'undefined type-from-cmd)
        (if (=:= 'undefined type-from-state)
            (default-type)
            type-from-state)
        type-from-cmd)))
