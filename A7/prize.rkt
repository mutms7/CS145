#lang racket

(require "scratchandwin.rkt")

(provide collect-prize)


;; collect-prize: (listof crd) -> 'prize or 'fraud
(define (collect-prize cardlist)
  (cond
    [(empty? cardlist) 'prize]
    [(not (list? cardlist)) 'fraud]
    [(equal? (scratch (first cardlist)) 'fraud) 'fraud]
    [(not (= (length cardlist) (second (scratch (first cardlist))))) 'fraud]
    [(not (iscards cardlist)) 'fraud]
    [(not (isN cardlist (second (scratch (first cardlist))))) 'fraud]
    [else (collect-prize1 (sort (clst->nlst cardlist) <) (second (scratch (first cardlist))))]
    )
  )

;; clst->nlst: (listof crd) -> (listof Nat)
(define (clst->nlst cdlst)
  (cond [(empty? cdlst) empty]
        [else (cons (first (scratch (first cdlst))) (clst->nlst (rest cdlst)))]))

;; iscards: (listof crd) -> bool
(define (iscards cardlist)
  (foldr (λ(x rest) (and (list? (scratch x)) rest)) true cardlist))

;; isN: (listof crd) Nat -> bool
(define (isN cardlist max)
  (foldr (λ(x rest) (and (= max (second (scratch x))) rest)) true cardlist))

;; (listof crd) -> 'prize or 'fraud
(define (collect-prize1 cardlist max)
  (cond
    [(empty? cardlist) 'prize]
    [(and (not (= (length cardlist) 1)) (empty? (second cardlist))) 'prize]
    [(and (not (= (length cardlist) 1)) (= (first cardlist) (second cardlist)))  'fraud]
    [else (collect-prize1 (rest cardlist) max)]
    )
  )



  


