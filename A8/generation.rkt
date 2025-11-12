#lang racket

(require "generate.rkt")

;; prime?: Nat -> Bool
(define (prime? n)
  (generate
   (list 2 n)
   (λ(x) (or
          (< (second x) 2)
          (> (first x) (sqrt (second x)))
          (= 0 (modulo (second x) (first x)))
          ))
   (λ(x) (list (+ 1 (first x)) (second x)))
   (λ(x) (cond
           [(< (second x) 2) false]
           [(> (first x) (sqrt (second x))) true]
           [(= 0 (modulo (second x) (first x))) false]
           ))
   ))


;; my-build-list: Nat (Nat -> X) -> (listof X)
(define (my-build-list n f)
  (generate
   (list (sub1 n) f empty)
   (λ(x) (= (first x) -1))
   (λ(x) (list
          (sub1 (first x))
          (second x)
          (cons ((second x) (first x)) (third x))))
   (λ(x) (third x))
   ))


;; my-foldl: (X Y -> Y) Y (listof X) -> Y
(define (my-foldl f z l)
  (generate
   (list f z l)
   (λ(x) (empty? (third x)))
   (λ(x) (list
          (first x)
          ((first x) (first (third x)) (second x))
          (rest (third x))))
   (λ(x) (second x))
   ))


;; my-insert: X (listof X) (X X -> Bool) -> (listof X)
(define (my-insert e l comp)
  (generate
   (list e l empty true comp)
   (λ(x) (empty? (second x)))
   (λ(x) (if (and (fourth x) (not ((fifth x) (first (second x)) (first x))))
             (list
              (first x)
              (rest (second x))
              (cons (first (second x)) (cons (first x) (third x)))
              false
              (fifth x)
              )
             (list
              (first x)
              (rest (second x))
              (cons (first (second x)) (third x))
              (fourth x)
              (fifth x)
              )
             ))
     (λ(x) (if (fourth x)
       (reverse (cons (first x) (third x)))
       (reverse (third x))))
     ))


;; my-insertion-sort: (listof X) (X X -> Bool) -> (listof X)
(define (my-insertion-sort l comp)
  (generate
   (list l empty)
   (λ(x) (empty? (first x)))
   (λ(x) (list
          (rest (first x))
          (my-insert (first (first x)) (second x) comp)))
   (λ(x) (second x))
     ))