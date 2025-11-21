#lang racket
(define-struct baexp (op arg1 arg2))

(define t1 (list + 5 10))

(define (parse exp)
  ;; a number
  ;; (op arg1 arg2)
  (match exp
    [(list op arg1 arg2) (make-baexp op
                                     (parse arg1)
                                     (parse arg2))]
    [num num]))