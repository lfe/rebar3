# LFE Formatting Gallery

> A catalog of every formatting situation `rebar3 lfe format` handles. Each entry
> shows the **canonical formatted output** the formatter produces for a
> representative example. Every code fence is filled by Common-Tooling
> verification: the example is compiled with LFE (must compile) and then run
> through `r3lfe_formatter:format/1`; the fence holds the exact formatter output.
>
> Status: fences are **empty pending CC** (see `cc-prompt-gallery.md`). The number
> on each entry is its stable ID — the matching input lives in the CC prompt.
>
> Conventions demonstrated: 80-col width, 2-space indent, align-under-first-arg
> for plain calls, the `lfe-indent.el` special-form table, def-forms always break
> (except no-arg constants), `let`/`case`/`cond`/maps always break, comments
> preserved.

---

## §1 — Atoms & literals

### 1. Symbol / atom
A bare symbol is emitted verbatim.

```lfe
foo-bar
```

### 2. Integers in several bases
Decimal, binary (`#b`), octal (`#o`), hex (`#x`), explicit radix (`#Nr`).

```lfe
(list 1234 #b1010 #o377 #xC0FFE #2r1010)
```

### 3. Floating-point number
Standard float literals, including exponent form.

```lfe
(list 1.0 -1.5 1.111e-10)
```

### 4. List string with escapes
A `"..."` string; escapes preserved verbatim.

```lfe
"line one\n\"quoted\" and \t tab"
```

### 5. Binary string
`#"..."` binary-string syntax.

```lfe
#"binary string"
```

### 6. Triple-quoted string
Multi-line `"""..."""`, verbatim content (including embedded quotes).

```lfe
"""
Line one
Line "two"
"""
```

### 7. Character literal
`#\a` and the hex form `#\x...;`.
Note: `#\ ` is the LFE space char literal (`#\space` is not valid LFE syntax).

```lfe
(list #\a #\  #\x1f42d;)
```

### 8. Bar-quoted symbol
`|symbol with spaces|`, including an escaped bar.

```lfe
(list |symbol with spaces| |a\|b|)
```

---

## §2 — Function calls

### 9. Flat call that fits
A call short enough to stay on one line.

```lfe
(+ 1 2 3)
```

### 10. Call that breaks (align under first arg)
A call too wide for 80 cols; continuation args align under the first argument.

```lfe
(some-function-with-a-longish-name argument-one
                                   argument-two
                                   argument-three
                                   argument-four)
```

### 11. Nested calls (mixed flat / broken)
An outer call that breaks containing inner calls that stay flat.

```lfe
(outer (inner-a 1 2)
       (inner-b 3 4)
       (a-much-longer-inner-call-that-forces-a-break x y z))
```

### 12. Module-qualified call
`(mod:func ...)` and the `(: mod func ...)` form.

```lfe
(: lists map (lambda (x) (* x x)) the-list)
```

### 13. Empty list / empty call
`()` and a no-arg call.

```lfe
(list () (no-args))
```

---

## §3 — Quoting & prefixes

### 14. Quote
`'x` and `'(a b c)` — prefix glued to its target.

```lfe
'(a b c)
```

### 15. Quasiquote
`` `(a ,b ,@c) `` — quasiquote with unquotes inside.

```lfe
`(a ,b ,@c)
```

### 16. Unquote / unquote-splicing
`,x` and `,@xs` — glued prefixes.

```lfe
`(list ,x ,@xs)
```

### 17. Function reference
`#'name/arity` — the deprecated fun-ref syntax, prefix glued.

```lfe
(lists:map #'double/1 xs)
```

---

## §4 — Special forms

### 18. if
`(if test then else)` — specform N=1 (test on the head line).

```lfe
(if (> x 0) x (- x))
```

### 19. case (always breaks)
`(case expr clause...)` — clauses one per line at +2.

```lfe
(case x
  (1 'one)
  (2 'two)
  (_ 'other))
```

### 20. cond (always breaks, aligned)
`(cond (test body)...)` — clauses aligned under the first clause.

```lfe
(cond ((> x 0) 'pos)
      ((< x 0) 'neg)
      ('true 'zero))
```

### 21. cond with `?=`
A `cond` clause using the `(?= pat expr)` match test.

```lfe
(cond ((?= `#(ok ,val) result) val)
      ('true 'error))
```

### 22. receive with after
`(receive clause... (after timeout ...))`.

```lfe
(receive
  ((tuple 'ok msg) msg)
  ((tuple 'error reason) (error reason))
  (after 5000 (error timeout)))
```

### 23. try / catch / after
A `try` with `case`, `catch`, and `after` sections.

```lfe
(try (foo x)
  (case ((tuple 'ok v) v))
  (catch ((tuple 'error reason) (error reason)))
  (after (cleanup)))
```

### 24. lambda
`(lambda (args) body)` — specform N=1.

```lfe
(lambda (x y) (+ x y))
```

### 25. match-lambda
`(match-lambda (pat body)...)` — specform N=0.

```lfe
(match-lambda ((x) (when (> x 0)) x) ((x) (- x)))
```

### 26. progn
`(progn expr...)` — N=0, body one per line.

```lfe
(progn (foo) (bar) (baz))
```

### 27. maybe with else
`(maybe ... (else ...))`.

```lfe
(maybe
  (tuple 'ok x)
  ?=
  (let ((y (+ x 1)))
    y)
  (else ((tuple 'error r) r)))
```

---

## §5 — let family

### 28. let, single binding (always breaks)
`(let ((x 1)) body)` — breaks even though it fits; binding stays compact.

```lfe
(let ((x 1))
  (+ x 2))
```

### 29. let, multiple bindings
Bindings one per line, aligned under the first binding.

```lfe
(let ((x 1)
      (y 2)
      (z 3))
  (+ x y z))
```

### 30. let*
Sequential bindings, one per line.

```lfe
(let* ((x 1)
       (y (+ x 1))
       (z (+ y 1)))
  (* x y z))
```

### 31. flet / fletrec (NOT forced)
Local functions — flat-if-fits (scope note: not in the always-break set).

```lfe
(flet ((double (x) (* 2 x)) (square (x) (* x x))) (double (square 3)))
```

---

## §6 — def-forms

### 32. defun with args (always breaks body)
Signature on the head line; body at +2, even when it would fit.

```lfe
(defun double (x)
  (* 2 x))
```

### 33. defun constant (no args, flat if fits)
`(defun +my-pi+ () 3.14)` — the constant idiom stays on one line.

```lfe
(defun +my-pi+ () 3.14159)
```

### 34. defun with docstring
Docstring on its own line at +2, before the body.

```lfe
(defun greet (name)
  "Greet the given name."
  (++ "Hello, " name))
```

### 35. defun match-clause form
`(defun name (clause)...)` — name on head line, clauses at +2.

```lfe
(defun fact
  ((0) 1)
  ((n) (* n (fact (- n 1)))))
```

### 36. defun match-clause with guard
Pattern and `(when ...)` guard share one line; body below at align column.
Wide clauses trigger the guard path; small clauses stay flat.

```lfe
(defun factorial
  ((0 accumulator) accumulator)
  ((number accumulator) (when (> number 0))
   (factorial (- number 1) (* number accumulator))))
```

### 37. defmacro
A macro definition (signature and/or match-clause).

```lfe
(defmacro my-and
  (() 'true)
  ((e) e)
  ((e . rest) `(if ,e (my-and ,@rest) 'false)))
```

### 38. defmodule with export
Wide `export`: keyword alone on its line, items at `C+2` (specform N=0).
Short `export` that fits within 80 cols stays flat.

```lfe
(defmodule my-module
  (export
    (ackermann 2)
    (factorial 1)
    (factorial 2)
    (large-prime-number? 1)
    (small-prime-number? 1)))
```

### 39. defmodule with import
Wide `import`: keyword alone, `(from …)`/`(rename …)` at `C+2`.

```lfe
(defmodule my-module
  (import
    (from lists (map 2) (filter 2) (foldl 3) (foldr 3))
    (rename io ((format 2) fmt))))
```

### 40. defrecord
`(defrecord name field...)` — always breaks, fields at +2.

```lfe
(defrecord person
  name
  age
  email)
```

### 41. defstruct
`(defstruct field...)`.

```lfe
(defstruct name
  age
  email)
```

---

## §7 — Data structures & comprehensions

### 42. Tuple (flat and broken)
`#(a b c)` flat; a wide tuple breaks with elements aligned under the first.

```lfe
#(a-very-long-element-name
  another-very-long-element-name
  yet-another-long-name-x)
```

### 43. Map (always breaks to pairs)
`#m(k v ...)` — one key-value pair per line.

```lfe
#m(key1 val1
   key2 val2
   key3 val3)
```

### 44. Binary with segments
`#b((x (size 16)) ...)` segment syntax.

```lfe
#b((x (size 16)) (y (size 8) (type unsigned)) z)
```

### 45. Record make / match / update
`(make-name ...)`, `(match-name ...)`, `(update-name ...)`.

```lfe
(make-person name "Alice" age 30 email "alice@example.com")
```

### 46. List comprehension
`(lc ((<- x list) guard) expr)`.

```lfe
(lc ((<- x '(1 2 3 4 5)) (> x 2)) (* x x))
```

### 47. Binary comprehension
`(bc ((<= seg binary) test) bitstring-expr)`.

```lfe
(bc ((<= seg my-binary) (> (size seg) 0)) seg)
```

---

## §8 — Comments

### 48. Leading own-line comment
A `;;` comment above a form (leading trivia).

```lfe
;; Compute the double of x.
(defun double (x)
  (* 2 x))
```

### 49. Trailing comment
An end-of-line comment after a form; one space before `;`.

```lfe
(defun double (x)
  (* 2 x)) ; double it
```

### 50. Block comment
`#| ... |#`, possibly multi-line, verbatim.

```lfe
(defun foo (x)
  #| this is a block
  comment |#
  x)
```

### 51. Dangling comment before close
A comment on its own line before a closing paren.

```lfe
(list 1
      2
      3
  ;; end of list
)
```

### 52. Comment between `(` and head
Head-leading comment — opener stands alone, all children at +2.

```lfe
(
  ;; c
  alpha
  beta)
```

### 53. Head trailing comment
A trailing comment on the head symbol — args fall to the body.

```lfe
(foo ;; trailing
  bar
  baz)
```

### 54. Section comment levels
`;;;;` / `;;;` / `;;` / `;` used at their conventional scopes.

```lfe
;;;; Section header
;;; Subsection
;; Note
; inline
(foo)
```

### 55. Blank line preservation
A single blank line kept between top-level forms (runs collapsed to one).

```lfe
(foo)

(bar)
```

---

## §9 — Width, nesting & guards

### 56. Form exceeding 80 columns
A form that overflows and must break.

```lfe
(some-long-function-name argument-one
                         argument-two
                         argument-three
                         argument-four-x)
```

### 57. Deeply nested form
Several levels of nesting, each indented consistently.

```lfe
(defun outer (x)
  (let ((y (inner-a x)))
    (let ((z (inner-b y)))
      (result x y z))))
```

### 58. Long argument list
A call whose arguments wrap, aligned under the first.

```lfe
(lists:foldl (lambda (acc x) (+ acc x)) 0 (lists:seq 1 100))
```

### 59. Guards with multiple tests
A `when` guard combining several tests.

```lfe
(defun in-range (x min max)
  (when (>= x min) (<= x max))
  x)
```

### 60. eval-when-compile
`(eval-when-compile (defun helper ...))`.

```lfe
(eval-when-compile
  (defun helper (x)
    (* x 2)))
```
