#lang racket
(require "stream.rkt" "ordered-set.rkt")

;; count: (streamof int) -> (streamof (list int Nat))
;; initial = (stream-first x)
;; step = (intersection (stream-first x))

(define (os-d s t)
  (os-difference (os-union s t) (os-intersection s t)))

(define (count s)
  (stream-generate (list s empty)
                   (λ(x) (empty? x))
                   (λ(x)
                     (local
                       [(define str (first x))
                        (define avl (second x))
                        ]
                     (if (os-member (os-singleton (list (stream-first str) 0)) avl)
                         (list (stream-rest s)
                     (os-intersection (os-singleton (list (stream-first str)
                                                (os-op (os-d
                                 (os-d
                                  avl
                                  (os-singleton (list (stream-first str) 0)))
                                 avl))))
                                          (os-d avl (os-singleton (list (stream-first str) 0)))))
                         (list (stream-rest s)
                     (os-intersection (os-singleton (list (stream-first str) 0)) avl))
                       )
                       )

                     )
                   (λ(x) (list (stream-first (first x)) (second (os-op (os-d
                                 (os-d
                                  (second x)
                                  (os-singleton (list (stream-first (first x)) 0)))
                                 (second x))))))
                   ))

(define x (list (list->stream '(3 9 3 4 9 3 7)) empty))



(list (stream-first (first x)) (second (os-op (os-d
                                 (os-d
                                  (second x)
                                  (os-singleton (list (stream-first (first x)) 0)))
                                 (second x)))))