# LFE Formatting Gallery

> A catalog of every formatting situation `rebar3 lfe format` handles. Each entry
> shows the **canonical formatted output** the formatter produces for a
> representative example. Every code fence is filled by Common-Tooling
> verification: the example is compiled with LFE (must compile) and then run
> through `r3lfe_formatter:format/1`; the fence holds the exact formatter output.
>
> Status: generated & verified against the 0.5.5 formatter (2026-06-25).
>
> Conventions (v2): 80-col width, 2-space indent. Model: **knowledge-gated** —
> canonical special forms use the `lfe-indent.el` table; unknown/data forms use
> break-preserving layout. Always-break set: `if`/`let`/`let*`/`case`/`cond`/
> `progn`/`receive`/`try`/`maybe`/maps/def-bodies. **try** uses full symmetry:
> `try` alone, body at +2, each section keyword alone at +2, section contents at
> +4. **export/import**: always one-per-line at `C+OpenLen` (+1 under the
> keyword), alphabetically sorted by name then arity (suppressed when any entry
> has a comment). **import nested**: `(from M …)`/`(rename M …)` keyword+module
> on the head line, entries at +1. Head comment on the opener line; cons-dot kept
> glued.

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
An outer call that breaks containing inner calls; the third inner call is wide
enough to itself break.

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
`#'name/arity` — the fun-ref syntax, prefix glued.

```lfe
(lists:map #'double/1 xs)
```

---

## §4 — Special forms

### 18. if (always breaks)
`(if test then else)` — `if` is in the always-break set; test on the head
line, then/else at +2.

```lfe
(if (> x 0)
  'positive
  'non-positive)
```

### 19. case (always breaks)
`(case expr clause...)` — clauses one per line at +2.

```lfe
(case x
  (1 'one)
  (2 'two)
  (_ 'many))
```

### 20. cond (always breaks, aligned)
`(cond (test body)...)` — clauses aligned under the first clause.

```lfe
(cond ((< x 0) 'neg)
      ((=:= x 0) 'zero)
      ('true 'pos))
```

### 21. cond with `?=`
A `cond` clause using the `(?= pat expr)` match test.

```lfe
(cond ((?= (tuple 'ok v) (fetch k)) v)
      ('true 'none))
```

### 22. receive with after
`(receive clause... (after timeout ...))`.

```lfe
(receive
  ((tuple 'msg m)
   (handle m))
  (after 1000 'timeout))
```

### 23. try / catch / after (full symmetry)
`try` alone on the first line; body and each section keyword at +2; section
contents at +4 via `render_clause`.

```lfe
(try
  (risky)
  (case
    ((tuple 'ok v) v))
  (catch
    ((tuple _ r _)
     (log r)))
  (after
    (cleanup)))
```

### 24. lambda
`(lambda (args) body)` — specform N=1.

```lfe
(lambda (x y) (+ x y))
```

### 25. match-lambda
`(match-lambda (pat body)...)` — specform N=0; always breaks.

```lfe
(match-lambda
  ((0) 'zero)
  ((n) n))
```

### 26. progn
`(progn expr...)` — N=0, body one per line.

```lfe
(progn
  (step-one)
  (step-two)
  (step-three))
```

### 27. maybe with else
`(maybe ... (else ...))`.

```lfe
(maybe
  (?= (tuple 'ok a) (fa))
  (?= (tuple 'ok b) (fb))
  (+ a b)
  (else (('error) 'failed)))
```

---

## §5 — let family

### 28. let, single binding (always breaks)
`(let ((x 1)) body)` — breaks even though it fits; binding stays compact.

```lfe
(let ((x 1))
  (+ x 1))
```

### 29. let, multiple bindings
First bindings fit on the binding head line; overflow bindings align under the
first.

```lfe
(let ((x 1) (y 2)
            (z 3))
  (+ x y z))
```

### 30. let*
Sequential bindings: first bindings on the head line, overflow aligned under
the first.

```lfe
(let* ((low 1) (high 2)
               (sum (+ low high)))
  sum)
```

### 31. flet / fletrec (flat-if-fits)
`flet` is not in the always-break set: the whole form stays flat when it fits.
When a local binding's body overflows, it renders defun-like (§3.6 — see #61).

```lfe
(flet ((double (n) (* 2 n))) (double 21))
```

---

## §6 — def-forms

### 32. defun with args (always breaks body)
Signature on the head line; body at +2, even when it would fit.

```lfe
(defun factorial (n)
  (* n (factorial (- n 1))))
```

### 33. defun constant (no args, flat if fits)
`(defun +my-pi+ () 3.14)` — the constant idiom stays on one line.

```lfe
(defun +my-pi+ () 3.14)
```

### 34. defun with docstring
Docstring on its own line at +2, before the body.

```lfe
(defun square (x)
  "Return the square of X."
  (* x x))
```

### 35. defun match-clause form
`(defun name clause...)` — name on head line, clauses at +2.

```lfe
(defun ack
  ((0 n)
   (+ n 1))
  ((m 0)
   (ack (- m 1) 1))
  ((m n)
   (ack (- m 1) (ack m (- n 1)))))
```

### 36. defun match-clause with guard
Pattern and `(when ...)` guard share one line; body below at the clause indent.

```lfe
(defun fact
  ((0 acc) acc)
  ((n acc) (when (> n 0))
   (fact (- n 1) (* n acc))))
```

### 37. defmacro
A simple macro definition; body at +2.

```lfe
(defmacro double (x)
  `(* 2 ,x))
```

### 38. defmodule with export
`export` entries always one-per-line at `C+OpenLen` (+1 under the keyword),
alphabetically sorted by name then arity.

```lfe
(defmodule maths
  (export
   (ackermann 2)
   (factorial 1)
   (square 1)))
```

### 39. defmodule with import
`import` entries nested: `(from M …)`/`(rename M …)` keyword+module on the
head line, entries one-per-line at +1, sorted within each clause; clause order
preserved.

```lfe
(defmodule client
  (export
   (run 0))
  (import
   (from lists
    (filter 2)
    (map 2))
   (rename maths
    ((factorial 1) fact))))
```

### 40. defrecord
`(defrecord name field...)` — always breaks, fields at +2.

```lfe
(defrecord person
  (name "")
  (age 0)
  email)
```

### 41. defstruct
`(defstruct field...)` — always breaks, fields with defaults at +2.

```lfe
(defstruct (name "")
  (age 0))
```

---

## §7 — Data structures & comprehensions

### 42. Tuple (flat and broken)
`#(ok 42)` stays flat; a wide `tuple` form breaks with elements aligned under
the first argument.

```lfe
(list #(ok 42)
      (tuple 'data
             (longish-value-one)
             (longish-value-two)
             (third-value-to-overflow)))
```

### 43. Map (always breaks to pairs)
`#m(k v ...)` — always breaks; one key-value pair per line.

```lfe
#m(alpha 1
   beta 2
   gamma 3)
```

### 44. Binary with segments
`#b((x (size 16)) ...)` segment syntax; flat when it fits.

```lfe
#b((42 (size 16)) (1.0 float (size 32)))
```

### 45. Record make / update
`(make-name ...)`, `(update-name ...)` — flat when they fit.

```lfe
(list (make-person name "Robert" age 54) (update-person p age 55))
```

### 46. List comprehension
`(lc ((<- x list) guard) expr)` — flat when it fits.

```lfe
(lc ((<- x (lists:seq 1 10)) (== 0 (rem x 2))) (* x x))
```

### 47. Binary comprehension
`(bc ((<= seg binary) test) bitstring-expr)` — breaks when wide.

```lfe
(bc ((<= (binary (f float (size 32))) bin) (> f 1.0))
  (binary (f float (size 64))))
```

---

## §8 — Comments

### 48. Leading own-line comment
A `;;` comment above a form (leading trivia).

```lfe
;; compute the answer
(defun answer () 42)
```

### 49. Trailing comment
An end-of-line comment after a form; one space before `;`.

```lfe
(defun answer () 42) ; the answer
```

### 50. Block comment
`#| ... |#` at the top level, verbatim.

```lfe
#| module-level note |#
(defun f () 'ok)
```

### 51. Trailing comment on element before close
A trailing comment on a list element; the close paren falls to its own line.

```lfe
(list a
      b ;; note before close
      )
```

### 52. Comment between `(` and head
Head-leading comment — opener stands alone, all children at +2.

```lfe
(
  ;; head note
  list
  a
  b
  c)
```

### 53. Head trailing comment
A trailing comment on the head symbol; args fall to `C+2` on a single line.

```lfe
(list ; the items
  a b c)
```

### 54. Section comment levels
`;;;;` (file), `;;;` (section), `;;` (code), `;` (inline) at their conventional
scopes.

```lfe
;;;; File header
;;; Section
;; code note
(defun f () ; inline
  'ok)
```

### 55. Blank line preservation
A single blank line is kept between top-level forms; runs of multiple blank
lines are collapsed to one.

```lfe
(defun foo () 'foo)

(defun bar () 'bar)

(defun baz () 'baz)
```

### 56. Wide call (align under first arg)
A wide call that cannot fit on one line; arguments align under the first.

```lfe
(register-handler 'an-event
                  (lambda (e) (process-the-event e))
                  #(priority high)
                  (with-options foo bar baz quux))
```

---

## §9 — Width, nesting & guards

### 57. Deeply nested form (fits flat)
Several levels of nesting; the whole thing fits within 80 cols and stays flat.

```lfe
(a (b (c (d (e (f-with-a-long-tail one two three four five six))))))
```

### 58. Long argument list
A call whose arguments break; continuation args align under the first.

```lfe
(do-something first-argument
              second-argument
              (lambda (x) (frob x))
              fourth-argument
              last-argument)
```

### 59. Guards with match-clause
`(when ...)` guard on the pattern line; body at clause indent.

```lfe
(defun classify
  ((n) (when (andalso (> n 0) (< n 10)))
   'small)
  ((n) 'large))
```

### 60. eval-when-compile
`(eval-when-compile (defun helper ...))`.

```lfe
(eval-when-compile
  (defun helper (x)
    (* x x)))
```

---

## §10 — A7 behaviors

### 61. Wide flet → defun-like break
When a `flet` local binding's body overflows, it renders defun-like: name +
args on the head line, body at `C+OpenLen+1`.

```lfe
(flet ((long-function-name (a b c)
         (some-complex-body-call a b c)))
  (long-function-name 1 2 3))
```

### 62. Cons-dot / improper list
A dotted pair `(a . b)` keeps the dot glued to its neighbours; `(cons 1 2)`
stays flat.

```lfe
(list (cons 1 2) '(a . b))
```

### 63. Commented export — sort suppressed
When any export entry has a comment (leading or trailing), sorting is suppressed
so the developer-annotated order is preserved verbatim.

```lfe
(defmodule m
  (export
   (z 0) ; last
   (a 0)))
```
