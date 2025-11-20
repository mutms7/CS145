#lang racket
(require "ordered-set.rkt" "stream.rkt")
(provide scoreboard)

;; scoreboard: (streamof (list String Nat)) -> (streamof (list String Nat Nat String))
(define (scoreboard s)
  (stream-generate
   (list s os-empty)
   (λ(x) (stream-empty? (first x)))
   (λ(x)
     (local
       [(define str (first x))
        (define avl (second x))
        (define curr (stream-first str))
        (define name (first curr))
        (define change (second curr))
        (define rofstr (stream-rest str))
        (define aftr (os-after avl (list name -inf.0)))
        ]
       ;; already exists
       (if (and 
            (not (equal? -inf.0 (second aftr)))
            (string-ci=? name (first aftr)))
           (list rofstr
                 ;; add new + change
                 (os-union (os-singleton (list name (+ change (second aftr))))
                           ;; remove original
                           (os-difference avl (os-singleton aftr))))
           ;; not exist
           (list rofstr
                 ;; add with 1
                 (os-union avl (os-singleton (list name change))))
           )))
   (λ(x) (local
           [(define str (first x))
            (define avl (second x))
            (define curr (stream-first str))
            (define name (first curr))
            (define change (second curr))
            (define rofstr (stream-rest str))
            (define aftr (os-after avl (list name -inf.0)))
            ]
           ;; already exists
           (if (and 
                (not (equal? -inf.0 (second aftr)))
                (string-ci=? name (first aftr)))
               ;;return element
                (list name change (+ change (second aftr)) (leaderboard (os-union (os-singleton (list name (+ change (second aftr))))
                           ;; remove original
                           (os-difference avl (os-singleton aftr)))))
               ;; not exist
               (list name change change (leaderboard (os-union avl (os-singleton (list name change)))))
                     )))))
   

(define (leaderboard a)
  (stream-generate
   a
   ;;empty
   (λ(x) (empty? x))
   ;; remove 1st place
   (λ(x) (os-difference x (os-singleton (os-op x))))
   ;; return 1st place
   (λ(x) (first (os-op x)))
   ))