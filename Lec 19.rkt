#lang racket
(define-struct baexp (op arg1 arg2))

(define t1 (list + 5 10))



(define-struct Var (id))
(define-struct App (fst snd))
(define-struct Abs (par bdy))

(define (parse sexp)
  (match sexp
      ;; Abs
      [`(λ (,x) ,y) (Abs x (parse y))]
    ;; App
    [`(,x ,y) (App x (parse y))]
    ;; Var
    [x (Var x)])
  )

(parse '((λ(x) (λ(y) x)) (λ(t) t)))