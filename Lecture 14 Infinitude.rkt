#lang racket

(require "stream.rkt")

(provide fibseq fibfirst fibrest)

(define (my-if test true-part false-part)
  (if test (true-part) (false-part)))

(define (why?) ((λ(x) (x x)) (λ(x) (x x))))


