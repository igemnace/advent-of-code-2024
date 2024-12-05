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

Part 2:

There is now a do() instruction that enables mul(), and a don't() instruction
that disables it. So, for example,

don't()mul(1,1)do()

would actually print 0. Only the most recent do()/don't() instruction applies,
so I don't need to think about nesting or closing.

This can easily be done by extending the parse+eval interpreter to simply
understand those two new instructions. Amendments to the algorithm above:

2. We now also parse do() and don't() into their own instructions.
3.
  - Apart from a sum count, we keep a "do" flag. We only eval a mul as a
    product if "do" is true; otherwise it just evals to 0.
  - We eval a do by toggling the "do" flag, and inversely for don't
|#

;; PARSE
(define (parse-instruction match)
  (let ([op (car match)])
    (cond
      ; '(mul X Y)
      [(string-prefix? (car match) "mul")
       (cons 'mul (map string->number (rest match)))]
      ; '(do)
      [(equal? (car match) "do()")
       '(do)]
      ; '(dont)
      [(equal? (car match) "don't()")
       '(dont)]
      [else '()])))

(define (parse-instructions str)
  ; NOTE: Thank you, regexp-quote! Pretty easy to inspect in the REPL how
  ; certain characters have to be quoted
  ; regexp-match* is our lexer :P
  (let ([matches (regexp-match* #rx"mul\\(([0-9]+),([0-9]+)\\)|do\\(\\)|don't\\(\\)" str #:match-select values)])
    (map parse-instruction matches)))

;; EVAL
; return both count and the do flag
(define (eval-instruction do? instruction)
  (let ([op (car instruction)]
        [args (cdr instruction)])
    (cond
      ; mul: multiply only if do flag is true
      [(eq? op 'mul)
       (if do?
           (values (foldl * 1 args) do?)
           (values 0 do?))]
      ; do: set do flag to true
      [(eq? op 'do)
       (values 0 #t)]
      ; dont: set do flag to false
      [(eq? op 'dont)
       (values 0 #f)])))

;; use an accumulating recursive function
;; acc is a pair of count and the do flag
(define (eval-instructions-acc acc instructions)
  (let ([count (car acc)]
        [do? (cdr acc)])
    (cond
      [(empty? instructions) count]
      [#t (let-values ([(c d?) (eval-instruction do? (car instructions))])
            ; simply sum the results as we go on, and thread the do flag
            (eval-instructions-acc (cons (+ count c) d?) (cdr instructions)))])))

(define (eval-instructions instructions)
  (eval-instructions-acc '(0 #t) instructions))

;; MAIN
;; functional core, imperative shell...
(call-with-input-file "./input" (lambda (in) (println (eval-instructions (parse-instructions (port->string in))))))
