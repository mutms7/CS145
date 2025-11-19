#lang racket
(require "stream.rkt")

;; stream-map: (int -> x) (streamof int) -> (streamof x)
(define (stream-map fn s)
  (stream-generate s
                   (λ(x) (empty? x))
                   (λ(x) (stream-rest x))
                   (λ(x) (fn (stream-first x)))))