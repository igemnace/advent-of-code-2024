#lang racket

#|
Day 3 Mull It Over

Part 1:

Input looks like this:

xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5)))

There are a lot of garbage characters, but all we want to do is extract mul(X,Y)
statements out of it.

This is actually why I chose Racket -- I want to write a parser (and Lisp came
to mind; otherwise I'd reach for a general purpose language that I already
know...  boring!), and Racket looked like it was the most approachable Scheme
while also supporting building DSLs as a use case.

That said, prior reading doesn't seem to point me toward any of the define-syntax
class of macros, so let's just implement a simple parser on our own for now.

Algorithm is simple:

(Parser)
1. Scan the string left to right. Keep tokens as state.
2. If we find a match for a valid mul(X,Y) statement, parse that into mul
   instruction.
(Eval)
3. Once we find all valid instructions, run through them 1-by-1 and eval
   their results. Keep a sum count.

I'm cheating here a bit -- I've already seen Part 2, and I know we'll have do()
and don't() directives, so separate parser and eval steps will help.

I'm skipping a separate lexer step cuz token -> AST is gonna be pretty simple
(pretty much just "mul(X,Y)" -> '(mul X Y))
|#

;; PARSE
(define (parse-instruction match)
  (cond
    [(string-prefix? (first match) "mul")
      ; '(mul X Y)
      (cons 'mul (map string->number (rest match)))]
    [else '()]))

(define (parse-instructions str)
  ; NOTE: Thank you, regexp-quote! Pretty easy to inspect in the REPL how
  ; certain characters have to be quoted
  ; regexp-match* is our lexer :P
  (let ([matches (regexp-match* #rx"mul\\(([0-9]+),([0-9]+)\\)" str #:match-select values)])
    (map parse-instruction matches)))

;; EVAL
(define (eval-instruction instruction)
  (cond
    ; mul: simply multiply
    [(eq? (car instruction) 'mul) (foldl * 1 (cdr instruction))]))

;; use an accumulating recursive function
;; acc is just the count
(define (eval-instructions-acc acc instructions)
  (cond
    [(empty? instructions) acc]
    [#t (let ([result (eval-instruction (car instructions))])
          ; simply sum the results as we go on
          (eval-instructions-acc (+ acc result) (cdr instructions)))]))

(define (eval-instructions instructions)
  (eval-instructions-acc 0 instructions))

;; MAIN
;; functional core, imperative shell...
(call-with-input-file "./input" (lambda (in) (println (eval-instructions (parse-instructions (port->string in))))))
