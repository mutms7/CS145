#lang racket

(require "lambda-interp.rkt")
 
(define (curry-sexpr sexp)
  (cond
    [(pair? sexp)
     (match sexp
       [(list (or 'λ 'lambda) params body)
        ((lambda (recur)
           (recur recur (if (pair? params) params (list params)) (curry-sexpr body)))
         (lambda (recur ps b)
           (if (null? ps)
               (list 'λ '() b)
               (list 'λ (list (first ps))
                     (if (null? (rest ps)) b (recur recur (rest ps) b))))))]
       [lst
        ((lambda (items)
           (if (null? (rest items))
               (first items)
               ((lambda (f)
                  (f f (first items) (rest items)))
                (lambda (self acc rest-items)
                  (if (null? rest-items)
                      acc
                      (self self (list acc (first rest-items)) (rest rest-items)))))))
         (map curry-sexpr (if (pair? lst) lst (list lst))))])]
    [else sexp]))

(define (curry sexp)
  (parse (curry-sexpr sexp)))

(define (lambda-print ast)
  (match ast
    [(App fst snd) (list (lambda-print fst) (lambda-print snd))]
    [(Abs par bdy) (list 'λ (list par) (lambda-print bdy))]
    [(Var id) id]))



(define (uncurried-eval exp) (lambda-print (lambda-interp (curry exp))))
