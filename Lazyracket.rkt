#lang lazy

(define (my-if test true-part false-part)
  (if test true-part false-part))

(define lst (list 1 2 3 4))
lst
(first lst)
(rest lst)
(define lst2 (cons 1 (cons 2 (cons 3 empty))))
lst2
(first lst2)
(rest lst2)
(rest (rest lst2))
