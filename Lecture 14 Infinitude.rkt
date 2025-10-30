#lang racket

(require "stream.rkt")

(provide fibseq fibfirst fibrest)

(define (my-if test true-part false-part)
  (if test (true-part) (false-part)))

(define (why?) ((λ(x) (x x)) (λ(x) (x x))))


(define fibseq
  (stream-generate
   (make-f 0 1)
   (λ(state) false)
   (λ(state) (make-f (f-nxt state) (+ (f-cur state) f-nxt state)))
   (λ(state) (f-cur state))))
