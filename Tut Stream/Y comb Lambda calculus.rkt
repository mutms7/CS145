#lang racket

;;ALL PROVIDED IN LAMBDA.RKT
(provide
 True
 False
 If
 Or
 And
 Not
 Cons
 First
 Rest
 Empty
 Empty?
 Y ;; fixed point combinator
 Z ;; Zero as binary
 Z? ;;Test for Z
 ADD1 
 ADD
 ;;;;
 True?
 TAN
 NAT
 )

(require "lambda.rkt")

(define Append
  (λ (lst1 lst2) (If (Empty? lst1) lst2
                     (Cons (First lst1) (Append (Rest lst1 lst2))))))
;; not pure lambda calc because there is recursion, so use Y combinator

(define Append
  (Y
   (λ (rec)
     (λ (lst1 ls2) (If (Empty? lst1) lst2
                       (Cons (First lst1) (rec (Rest lst1 lst2))))))))
;; (Y f) returns something that expects the same number of parameters as append
;; f expects the recursive self

;; GENERAL Y COMBINATOR ADVICE
;;write code recursively first, then add the Y combinator, wrap λ with rec, and replace the recursive calls with rec


   (define l1 (Cons 3 (Cons 2)))
   (define l2 (Cons 1 (Cons 4)))

   (First (append l1 l2))