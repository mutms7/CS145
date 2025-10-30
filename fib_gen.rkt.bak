#lang racket

(require "fibstream.rkt")

(fibfirst fibseq)
(fibfirst (fibrest fibseq))
(fibfirst (fibrest (fibrest fibseq)))

(define (my-list-ref lst i my-first my-rest)
  (cond
    [(zero? i) (my-first lst)]
    [else (my-list-ref (my-rest lst) (sub1 i) my-first my-rest)]))

(my-list-ref (build-list 1000 add1) 500 first rest)
(my-list-ref fibseq 500 fibfirst fibrest)