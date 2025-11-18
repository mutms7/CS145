#lang lazy
(require "IOStream.rkt")

(define fibs
  (Gen
   0 ;; current
   1 ;; next fib
   (lambda (this next cont)
     (cont next ;; next is new current
           (+ this next) ;; new next
           (list this)))));; output current