#lang lazy
(require "Lambda.rkt")

(define (MUL a b)
  (MULh (removeZ a) (removeZ b))
  )

(define (MULh a b) ;;101 0011
  ;; a is zero; return b
  (If (Z? a) Z
      ;; first a is false; multiply rest of a with 2*b
      (If (Not (First a)) (MUL (Rest a) (Cons False b))
          ;; first a is true; add b with
          (ADD b (MUL (Rest a) (Cons False b)))
          )))

(define (removeZ a)
  (makeZ a (trailZ a Z) a)
  )

(define (trailZ a zeros)
  (If (Z? a) zeros
      (If (First a) (trailZ (Rest a) Z)
          (trailZ (Rest a) (Cons False zeros))
           )))

(define (makeZ a zeros total)
  (If (Z? total) Z
      (If (Not (Z? zeros)) (makeZ a (Rest zeros) (Rest total))
          (Cons (First a) (makeZ (Rest a) zeros (Rest total)))
           )))

