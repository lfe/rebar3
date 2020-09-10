(defmodule {{name}}-tests
  (behaviour ltest-unit))

(include-lib "ltest/include/ltest-macros.lfe")

;;; -----------
;;; library API
;;; -----------

(deftest my-fun
  (is-equal 'hellow-orld ({{name}}:my-fun)))
