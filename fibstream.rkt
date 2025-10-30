
#lang racket

(require "stream.rkt")

(provide fibseq fibfirst fibrest)

(define-struct f (cur nxt))

(define fibseq
  (stream-generate
   (make-f 0 1)  ;; initial value of state
   (lambda (state) false) ;; when to quit
   (lambda (state) (make-f (f-nxt state) (+ (f-cur state) (f-nxt state))))
   (lambda (state) (f-cur state))))

(define fibfirst stream-first)
(define fibrest stream-rest)