#lang racket

(provide generate)

(define (generate initial done? step final)
  (local
    [(define (gen initial)
      (cond 
        [(done? initial) (final initial)]
        [else  (gen (step initial))]))]
  (gen initial)))