#lang racket

(provide drawcard scratch)

(define-struct crd (sec total))

(define (drawcard n)
  (make-crd (add1 (random n)) n))

(define (scratch card)
  (if (crd? card)
  (list (crd-sec card) (crd-total card))
  'fraud))