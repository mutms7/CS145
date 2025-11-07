#lang racket

(require "scratchandwin.rkt")

(provide collect-prize)


;; collect-prize: (listof crd) -> 'prize or 'fraud
(define (collect-prize cardlist)
  (cond
    [(empty? cardlist) 'prize]
    [(not (list? cardlist)) 'fraud]
    [(equal? (scratch (first cardlist)) 'fraud) 'fraud]
    [(not (lenandtype cardlist 0 (second (scratch (first cardlist))))) 'fraud] 
    [else (collecthelp (sort (crdl->numl cardlist) <) (build-list (second (scratch (first cardlist))) add1))]
    )
  )

;; lenandtype: (listof crd) Nat Nat -> bool
(define (lenandtype cardlist at target)
  (cond
    [(and (empty? cardlist) (= at target)) true]
    [(or (empty? cardlist) (= at target)) false]
    [(equal? (scratch (first cardlist)) 'fraud) false]
    [(not (equal? (second (scratch (first cardlist))) target)) false]
    [else (lenandtype (rest cardlist) (add1 at) target)]
    )
  )

(define (crdl->numl cardlist)
  (cond [(empty? cardlist) empty]
  [else (cons (first (scratch (first cardlist))) (crdl->numl (rest cardlist)))]))
  

(define (collecthelp cardlist idenlist)
  (cond
    [(empty? cardlist) 'prize]
    [(not (= (first cardlist) (first idenlist)))  'fraud]
    [else (collecthelp (rest cardlist) (rest idenlist))]
    )
  )






