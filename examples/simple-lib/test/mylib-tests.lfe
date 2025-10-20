(defmodule mylib-tests
  (behaviour ltest-unit))

(include-lib "ltest/include/ltest-macros.lfe")

(deftest add-positive
  (is-equal 5 (mylib:add 2 3)))

(deftest add-negative
  (is-equal -1 (mylib:add 2 -3)))

(deftest multiply-positive
  (is-equal 6 (mylib:multiply 2 3)))

(deftest multiply-zero
  (is-equal 0 (mylib:multiply 5 0)))
