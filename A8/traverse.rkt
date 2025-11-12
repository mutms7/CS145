#lang racket
(require "btree.rkt" "generate.rkt")

;; travers : bt -> (listof X)
(define (traverse t)
  (generate
   (list (list t) empty)
   (λ(x) (empty? (first x)))
   (λ(x)
     (cond
       [(and (empty? (bt-left (first (first x)))) (empty? (bt-right (first (first x)))))
        (list (rest (first x))
              (cons (bt-dec (first (first x))) (second x)))
        ]
         
       [(empty? (bt-left (first (first x)))) 
        (list (cons
               (make-bt empty (bt-dec (first (first x))) empty)
               (cons
                (bt-right (first (first x)))
                (rest (first x))))
              (second x))
        ]
       [(empty? (bt-right (first (first x))))
        (list (cons
               (bt-left (first (first x)))
               (cons
                (make-bt empty (bt-dec (first (first x))) empty)
                (rest (first x))))
              (second x))
        ]
           
       [else
        (list (cons
               (bt-left (first (first x)))
               (cons
                (make-bt empty (bt-dec (first (first x))) empty)
                (cons
                 (bt-right (first (first x)))
                 (rest (first x)))))
              (second x))

        ]
       )
       
     )
   (λ(x) (reverse (second x)))
   ))