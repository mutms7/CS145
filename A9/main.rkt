#lang racket

(require "ordered-set.rkt" "stream.rkt")

;; count: (streamof Num) -> (streamof (list Num int))
(define (count s)
  (stream-generate
   (list s os-empty)
   (λ(x) (stream-empty? (first x)))
   (λ(x) (local
           [(define str (first x))
            (define avl (second x))
            (define curr (stream-first str))
            (define rofstr (stream-rest str))
            (define aftr (os-after avl (list curr -inf.0)))
            ]
           ;; already exists
           (if (and 
                (not (equal? -inf.0 (second aftr)))
                (equal? curr (first aftr)))
               (list rofstr
                     ;; add new +1
                     (os-union (os-singleton (list curr (add1 (second aftr))))
                                             ;; remove original
                                             (os-difference avl (os-singleton aftr))))
               ;; not exist
               (list rofstr
                     ;; add with 1
                     (os-union avl (os-singleton (list curr 1))))
               )))
   ;; return first value with number of previous occurances
   (λ(x) (local
           [(define str (first x))
            (define avl (second x))
            (define curr (stream-first str))
            (define rofstr (stream-rest str))
            (define aftr (os-after avl (list curr -inf.0)))
            ]
           ;; already exists
           (if (and 
                (not (equal? -inf.0 (second aftr)))
                (equal? curr (first aftr)))
               ;;return element
               aftr
               ;; not exist
               (list curr 0))))
   ))
               
               
               
           
               