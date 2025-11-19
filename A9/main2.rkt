#lang racket

(require "ordered-set.rkt")
(require "stream.rkt")

(define (count s) (stream-generate
                   ;; initial
                   (list s os-empty)

                   ;; final
                   (λ (x) (stream-empty? (first x)))

                   ;; step
                   (λ (x) (cond
                            ;; has the number been seen before?
                            [(and (not (equal? (second (os-before (second x) (list (stream-first (first x)) +inf.0))) +inf.0))
                                  ;; is the found element actually the set element we are checking?
                                  (equal? (stream-first (first x)) (first (os-before (second x) (list (stream-first (first x)) +inf.0)))))
                             (list (stream-rest (first x))
                                   ;; merge the new entry with the set with the incremented count
                                   (os-union (os-singleton (list (stream-first (first x)) (add1 (second (os-before (second x) (list (stream-first (first x)) +inf.0))))))
                                             ;; get rid of the old instance in the set
                                             (os-difference (second x) (os-singleton (os-before (second x) (list (stream-first (first x)) +inf.0))))))]
                            ;; not seen before, so just put in that first instance
                            [else (list (stream-rest (first x))
                                        (os-union (os-singleton (list (stream-first (first x)) 1)) (second x)))]))

                   ;; final
                   (λ (x) (cond  
                            [(and (not (equal? (second (os-before (second x) (list (stream-first (first x)) +inf.0))) +inf.0))
                                  (equal? (stream-first (first x)) (first (os-before (second x) (list (stream-first (first x)) +inf.0)))))
                             ;; same idea as before
                             (os-before (second x) (list  (stream-first (first x)) +inf.0) )]
                            ;; not in the set, put one element
                            [else (list (stream-first (first x)) 0)]))))
