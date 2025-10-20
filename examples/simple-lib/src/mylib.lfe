(defmodule mylib
  (export (add 2)
          (multiply 2)
          (greet 1)))

(defun add (a b)
  "Add two numbers"
  (+ a b))

(defun multiply (a b)
  "Multiply two numbers"
  (* a b))

(defun greet (name)
  "Greet someone"
  (io:format "Hello, ~s!~n" (list name)))
