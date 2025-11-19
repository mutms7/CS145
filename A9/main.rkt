#lang racket
(require "stream.rkt" "ordered-set.rkt")

;; count: (streamof int) -> (streamof (list int Nat))
;; initial = (stream-first x)
;; step = (intersection (stream-first x))

(define (os-d s t)
  (os-difference (os-union s t) (os-intersection s t)))

(define (count s)
  (stream-generate (list s os-empty)
                   (λ(x) true)
                   (λ(x) (list (stream-rest s)
                     (local
                       [(define str (first x))
                        (define avl (second x))
                        ]
                     (if (os-member (os-singleton (list (stream-first str) 0)) avl)
                         (os-intersection (os-singleton (list (stream-first str)
                                                (os-op (os-d
                                 (os-d
                                  avl
                                  (os-singleton (list (stream-first str) 0)))
                                 avl))))
                                          (os-d avl (os-singleton (list (stream-first str) 0))))
                         (os-intersection (os-singleton (list (stream-first str) 0)) avl)
                       )
                       )))
                   (λ(x) (second (os-d
                                 (os-d
                                  (second x)
                                  (os-singleton (list (stream-first (first x)) 0)))
                                 (second x))))
                   ))