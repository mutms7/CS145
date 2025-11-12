#lang racket
(require "generate.rkt")

;; 1: split array
;; 2: sort left list sort right list
;; 3: merge lists



;; my-sort: (listof X) (X X -> Bool) -> (listof X)

(define (my-sort l comp)
  (generate
   (list l comp empty)
   (λ(x) true)
   (λ(x) (list empty empty empty))
   (λ(x) (myhelp (first x) (second x)))
   ))

;; my-help: (listof X) (X X -> Bool) -> (listof X)
(define (myhelp l comp)
  (generate
   (list l comp empty)
   (λ(x) true)
   (λ(x) (list empty empty empty))
   (λ(x) (if (< (length (first x)) 2) (first x) (my-tail (my-split (first x) (floor (/ (length (first x)) 2))) (second x))))
     ))

;; my-tail: (listof (listof X)) (X X -> Bool) -> (listof X)

(define (my-tail lisoflis comp)
  (generate
   (list (first lisoflis) (second lisoflis) comp)
   (λ(x) true)
   (λ(x) (list empty empty empty))
   (λ(x) (my-merge (myhelp (first x) (third x)) (myhelp (second x) (third x)) (third x)))
   ))

;; my-split: (listof X) Nat -> (listof (listof X))
;; (my-split (list 1 2 4 5) 2) -> (list (list 1 2) (list 4 5))
(define (my-split l n)
  (generate
   (list n l empty)
   (λ(x) (= (first x) 0))
   (λ(x) (list
          (sub1 (first x))
          (rest (second x))
          (cons (first (second x)) (third x))
          ))
   (λ(x) (list (reverse (third x)) (second x)))
   ))


;; my-merge: (listof X) (listof X) -> (listof X)
;; ordered ordered -> ordered
(define (my-merge l1 l2 comp)
  (generate
   (list l1 l2 comp empty)
   (λ(x) (and (empty? (first x)) (empty? (second x))))
   (λ(x) (cond
           [(empty? (first x))
            (list
             (first x)
             (rest (second x))
             (third x)
             (cons (first (second x)) (fourth x))
             )]
           [(empty? (second x))
            (list
             (rest (first x))
             (second x)
             (third x)
             (cons (first (first x)) (fourth x))
             )]
           [(not (comp (first (first x)) (first (second x))))
            (list
             (first x)
             (rest (second x))
             (third x)
             (cons (first (second x)) (fourth x))
             )]
           [else
            (list
             (rest (first x))
             (second x)
             (third x)
             (cons (first (first x)) (fourth x))
             )]
           )
     )
   (λ(x) (reverse (fourth x)))
   ))


 