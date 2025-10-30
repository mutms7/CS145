#lang racket

(require "stream.rkt")


(define (my-if test true-part false-part)
  (if test (true-part) (false-part)))

(define (why?) ((λ(x) (x x)) (λ(x) (x x))))

(define s (list->stream (build-list 100000 add1)))
(stream-first s)
(stream-first (stream-rest s))