#lang lazy
(require "Lambda.rkt")

(define (MUL a b)
  (MULf (removeZ a) (removeZ b) (halve (removeZ a) Z))
  )

(define (MULf a b ah)
  (MULp
   (removeZ (front a ah))
   (removeZ (back a ah))
   (removeZ (front a ah))
   (removeZ (back a ah))
   ah))

(define (MULp a b c d m)
  (MULd (MULt (ADD a b) (ADD c d)) (MULt a c) (MULt b d) m))

(define (MULd big fro bac m)
  (ADD (shift (shift bac m) m)
       (ADD (shift (removeZ (SUB (removeZ (SUB big bac)) fro)) m)
            fro)))


(define (SUB1 x)
   (If (Z? x) Z
       (If (First x) (Cons False (Rest x))
           (Cons True (SUB1 (Rest x))))))

(define (SUB a b)
  (If (Z? a) Z
   (If (Z? b) a
       ;; first b is false; first digit is first a
    (If (Not (First b)) (Cons (First a) (SUB (Rest a) (Rest b)))
        ;; first a is true; first digit is not first b
     (If (First a) (Cons (Not (First b)) (SUB (Rest a) (Rest b)))
         ;; first a is false, first b is true, then true and sub1 to rest
       (Cons True (SUB1 (removeZ (SUB (Rest a) (Rest b))))))))))
   

(define (MULt a b)
  (If (Z? (removeZ b)) Z
  (MULh (removeZ a) (removeZ b)))
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
  (If (Z? amt)  (Cons False a)
      (shift (Cons False a) (Rest amt))
           ))

