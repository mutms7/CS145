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

;; karatsuba
(define (halve a midlen)
  (If (Z? a) midlen
      (halveh (Rest a) (Cons True midlen))))

(define (halveh a midlen)
  (If (Z? a) midlen
      (halve (Rest a) midlen)))

(define (front a midlen)
  (If (Z? midlen) Z
     (Cons (First a) (front (Rest a) (Rest midlen)))
           ))

(define (back a midlen)
  (If (Z? a) Z
      (If (Z? midlen) (Cons (First a) (back (Rest a) midlen))
          (back (Rest a) (Rest midlen)))
           ))

(define (shift a amt)
  (If (Z? amt) a
      (shift (Cons False a) (Rest amt))
           ))

