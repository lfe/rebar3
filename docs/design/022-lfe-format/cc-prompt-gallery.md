# 022 · Formatting gallery — fill the fences (CC task)

> Target: Sonnet 4.6 + `erlang-guidelines`. Populate every empty fence in
> `formatting-gallery.md` with **real, verified formatter output**. **Stop and
> report** if blocked.
>
> **Output discipline (critical — this is large):** work **one section (§) per
> turn**, write results straight into `formatting-gallery.md` with Edit, don't
> paste the file back, keep prose terse. There are 60 entries across 9 sections —
> do **not** attempt them all in one turn.

## 0. Orientation

1. `CLAUDE.md` (`warnings_as_errors` ON); `erlang-guidelines` (`11-anti-patterns`).
2. `formatting-gallery.md` — the doc you are filling. Each entry has a numbered
   heading and one empty ```lfe fence. Fence **N** receives the formatted output
   of **input N** below.
3. `src/r3lfe_formatter.erl` — `format/1` is the formatter you run.

## 1. Workflow per entry (do all three)

For each numbered input below:

1. **Verify it is valid LFE.** Compile it: if the input is a complete top-level
   form (`defmodule`/`defun`/`defrecord`/…), compile it inside a minimal module;
   if it is an expression or fragment, wrap it — e.g.
   `(defmodule g (export (e 0))) (defun e () <input>)` — and compile that with
   the LFE compiler. If an input does **not** compile, minimally adjust it to a
   valid LFE equivalent that demonstrates the **same situation**, and note the
   change in your report. (For fragments that can't be wrapped, at minimum verify
   they read via `lfe_io:read_string/1`.)
2. **Format the construct of interest** (the input as given, *not* the wrapper):
   `{ok, IO} = r3lfe_formatter:format(<<"…input…">>)`, take
   `unicode:characters_to_binary(IO)`.
3. **Write that exact output** into fence N of `formatting-gallery.md`. Then
   confirm idempotency for it (`format(output) == output`); if it isn't a fixed
   point, that's a bug — stop and report it rather than papering over.

Verify a quick **token-preservation** check per entry too (the formatted output
re-lexes to the same significant tokens as the input). If any entry reveals a
formatter bug, **stop and report** — do not edit the formatter from this task.

## 2. The inputs (fence number → example)

Inputs are written compactly / unformatted on purpose; the formatter does the
work. Strings are shown as the LFE source to format.

**§1 Atoms & literals**
1. `foo-bar`
2. `(list 1234 #b1010 #o377 #xC0FFE #2r1010)`
3. `(list 1.0 -1.5 1.111e-10)`
4. `"line one\n\"quoted\" and \t tab"`
5. `#"binary string"`
6. (triple-quoted, multi-line):
   ```
   """
   Line one
   Line "two"
   """
   ```
7. `(list #\a #\space #\x1f42d;)`
8. `(list |symbol with spaces| |a\|b|)`

**§2 Function calls**
9. `(+ 1 2 3)`
10. `(some-function-with-a-longish-name argument-one argument-two argument-three argument-four)`
11. `(outer (inner-a 1 2) (inner-b 3 4) (a-much-longer-inner-call-that-forces-a-break x y z))`
12. `(: lists map (lambda (x) (* x x)) the-list)`
13. `(list () (no-args))`

**§3 Quoting & prefixes**
14. `'(a b c)`
15. `` `(a ,b ,@c) ``
16. `` `(list ,x ,@xs) ``
17. `(lists:map #'double/1 xs)`

**§4 Special forms**
18. `(if (> x 0) 'positive 'non-positive)`
19. `(case x (1 'one) (2 'two) (_ 'many))`
20. `(cond ((< x 0) 'neg) ((=:= x 0) 'zero) ('true 'pos))`
21. `(cond ((?= (tuple 'ok v) (fetch k)) v) ('true 'none))`
22. `(receive ((tuple 'msg m) (handle m)) (after 1000 'timeout))`
23. `(try (risky) (case ((tuple 'ok v) v)) (catch ((tuple _ r _) (log r))) (after (cleanup)))`
24. `(lambda (x y) (+ x y))`
25. `(match-lambda ((0) 'zero) ((n) n))`
26. `(progn (step-one) (step-two) (step-three))`
27. `(maybe (?= (tuple 'ok a) (fa)) (?= (tuple 'ok b) (fb)) (+ a b) (else (('error) 'failed)))`

**§5 let family**
28. `(let ((x 1)) (+ x 1))`
29. `(let ((x 1) (y 2) (z 3)) (+ x y z))`
30. `(let* ((low 1) (high 2) (sum (+ low high))) sum)`
31. `(flet ((double (n) (* 2 n))) (double 21))`

**§6 def-forms**
32. `(defun factorial (n) (* n (factorial (- n 1))))`
33. `(defun +my-pi+ () 3.14)`
34. `(defun square (x) "Return the square of X." (* x x))`
35. `(defun ack ((0 n) (+ n 1)) ((m 0) (ack (- m 1) 1)) ((m n) (ack (- m 1) (ack m (- n 1)))))`
36. `(defun fact ((0 acc) acc) ((n acc) (when (> n 0)) (fact (- n 1) (* n acc))))`
37. `` (defmacro double (x) `(* 2 ,x)) ``
38. `(defmodule maths (export (factorial 1) (ackermann 2) (square 1)))`
39. `(defmodule client (export (run 0)) (import (from lists (map 2) (filter 2)) (rename maths ((factorial 1) fact))))`
40. `(defrecord person (name "") (age 0) email)`
41. `(defstruct (name "") (age 0))`

**§7 Data structures & comprehensions**
42. `(list #(ok 42) (tuple 'data (longish-value-one) (longish-value-two) (third-value-to-overflow)))`
43. `#m(alpha 1 beta 2 gamma 3)`
44. `#b((42 (size 16)) (1.0 float (size 32)))`
45. `(list (make-person name "Robert" age 54) (update-person p age 55))`
46. `(lc ((<- x (lists:seq 1 10)) (== 0 (rem x 2))) (* x x))`
47. `(bc ((<= (binary (f float (size 32))) bin) (> f 1.0)) (binary (f float (size 64))))`

**§8 Comments**
48. `;; compute the answer`  ⏎  `(defun answer () 42)`
49. `(defun answer () 42) ; the answer`
50. `#| module-level note |#`  ⏎  `(defun f () 'ok)`
51. `(list a b ;; note before close` ⏎ `)`
52. `(;; head note` ⏎ `list a b c)`
53. `(list ; the items` ⏎ `a b c)`
54. a small module showing `;;;;` (file), `;;;` (section), `;;` (code), `;` (inline)
55. two top-level `defun`s separated by one blank line, plus a 3-blank run to collapse
56. `(register-handler 'an-event (lambda (e) (process-the-event e)) #(priority high) (with-options foo bar baz quux))`

**§9 Width, nesting & guards**
57. `(a (b (c (d (e (f-with-a-long-tail one two three four five six))))))`
58. `(do-something first-argument second-argument (lambda (x) (frob x)) fourth-argument last-argument)`
59. `(defun classify ((n) (when (andalso (> n 0) (< n 10))) 'small) ((n) 'large))`
60. `(eval-when-compile (defun helper (x) (* x x)))`

## 3. Report against this ledger

```
Formatting gallery
[ ] §1–§9 fences all filled with verified formatter output (state per-section done)
[ ] every input compiled (or wrapped+compiled; fragments read-verified); list any
    inputs you had to adjust to compile, with the change
[ ] each entry is a format/1 fixed point (idempotent) and token-preserving
[ ] any formatter bug surfaced → reported (NOT fixed from this task)
[ ] full existing suite still green (you only edited the gallery doc + ran the
    formatter; no src changes)
[ ] deviations / adjustments named — or "none"
```

Work one § per turn. After §9, the gallery is a compile-verified, idempotent
showcase of the formatter's output.
