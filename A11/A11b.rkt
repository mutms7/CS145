#lang racket

(require "lambda-interp.rkt")

(define (curry x)
  (cond
    [(not (list? x)) x]
    [(empty? (rest x)) (first x)]
    [(or (equal? (first x) 'lambda) (equal? (first x) 'λ))
     ;; only one variable
     (if (empty? (rest (second x)))
         (list
          'λ
          (list (first (second x)))
          (curry (third x)))

         ;; multiple variables
         (curry (list
                 'λ
                 (reverse (rest (reverse (second x))))
                 (list 'λ (list (first (reverse (second x)))) (curry (third x))))))]
    [else
     (if (empty? (rest (rest x))) ;; only two elements
         (list (curry (first x)) (curry (second x)))
         ;; 3+ elements
         (list

          
          
          (curry (reverse (rest (reverse x))))

          ;; last element
          (curry (first (reverse x)))))]
    ))
    
(define (myparse sexp)
  (match sexp
    [`(λ (,x) ,y) (Abs x (myparse y))]
    [`(lambda (,x) ,y) (Abs x (myparse y))]
    [`(λ ,x ,y) (Abs x (myparse y))]
    [`(lambda ,x ,y) (Abs x (myparse y))]
    [`(,x ,y) (App (myparse x) (myparse y))]
    [x
     (Var x)]))

(define (uncurried-eval ast)
  (lambda-interp (myparse (curry ast)))
  )
