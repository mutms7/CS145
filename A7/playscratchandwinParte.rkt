#lang racket

(require "scratchandwin.rkt")
(require "avl-cs145.rkt")

(provide playgame)

;;playgame: Nat -> (listof crd)
(define (playgame n)
  (playgamehelp n empty empty)
  )

;;playgamehelp: Nat avl (listof crd) -> (listof crd)
(define (playgamehelp n avl result)
  (local [(define cd (drawcard n))
          (define a (insertavl avl (first (scratch cd))))]
  (cond
    [(= (sizeavl avl) n) result]
    [(= (sizeavl a) (sizeavl avl)) (playgamehelp n a result)]
    [else (playgamehelp n a (cons cd result))]
    )
    )
  )
