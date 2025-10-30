#lang racket

;; Stream implementation v1.2
;; Copyright 2010 Gordon V. Cormack
;; Copyright 2024 Troy Vasiga
;; Permission is granted for student use in CS145
;; Publication or other use prohibited

(provide stream-first stream-rest stream-cons 
  stream-empty stream-empty? stream-generate list->stream
  stream->list)

(define-struct stream-pair (this next))
(define (stream-first s) (stream-pair-this s))
(define (stream-rest s) ((stream-pair-next s)))
(define (stream-cons e s) (make-stream-pair e (lambda () s)))
(define stream-empty empty)
(define stream-empty? empty?)

(define (stream-generate initial done? step final)
  (cond
    [(done? initial) empty]
    [else (make-stream-pair
           (final initial)
           (lambda ()
             (stream-generate
              (step initial)
              done?
              step
              final)))]))

(define (list->stream l) (foldr stream-cons stream-empty l))

(define (stream->list s)
  (if (stream-empty? s) empty (cons (stream-first s) (stream->list (stream-rest s)))))