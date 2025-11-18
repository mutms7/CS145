#lang racket

(require "stream.rkt" "total-order.rkt" "ordered-set.rkt")

(define (queue s)
  (stream-generate (list s os-empty)
                   (λ (state) (os-empty? (first state)))
                   (λ (state) (local [(define ins (first (stream-first (first state))))]
                                (cond [(symbol=? ins 'pop) (list (stream-rest (first state))
                                                                 (os-difference (second state) (os-singleton (os-op (second state))))

                                       
                   (λ (state) (local [(define ins (first (stream-first (first state))))]
                               (cond [(symbol=? ins 'pop) (string-append "popped: " ) (number->string (first (to-unhide (os-op (second state)))))]
                                     [...]
                                     )))
                   ))

