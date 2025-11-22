#lang lazy

(define True (λ (yes) (λ (no) yes)))
(define False (λ (yes) (λ (no) no)))

(define (Or a b) (If a True b))
(define (And a b) (If a b False))
(define (Not a) (If a False True))

;; and
(lambda(a b)((a b)(lambda(x)(lambda(y)y))))