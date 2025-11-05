#lang racket
(require "scratchandwin.rkt")



(provide playgame)

;; 
(define (playgame n)
  (playgamehelp n empty n)
  )

(define (playgamehelp n lstsofar total)
  (local [(define cd (drawcard n))]
  (cond [(= total 0) lstsofar]
        [(not (foldr (λ(c rest) (or (= (first (scratch c)) (first (scratch cd))) rest)) false lstsofar)) (playgamehelp n (cons cd lstsofar) (sub1 total))]
        [else (playgamehelp n lstsofar total)]
        )))

  