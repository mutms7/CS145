#lang lazy

;; Leaderboard WC

(require "Gen.rkt")

(define Nats ;; a stream of all non-negative integers in ascending order; e.g.,
  ;;   (outstream (take 5 Nats)) ;; outputs 0 1 2 3 4
  (Gen
   0
   0
   (λ (inp state cont)
     (cont 0 (add1 state) (list state)))
   ))


(define (Kill k s) ;; remove all elements divisible by k from stream s; e.g.,
  ;;   (outstream (Kill 3 (take 5 Nats))) ;; outputs 1 2 4
  ;;   (outstream (take 5 (Kill 2 Nats))) ;; outputs 1 3 5 7 9 
  ;;   (outstream (Kill 5 '(10 15 20 31))) ;; outputs 31
  (Gen
   s
   0
   (lambda (stream state cont)
     (cond
       [(empty? stream) empty]
       [(= (remainder (first stream) k) 0)
        (cont
         (rest stream)  ;; disfirstd head of list
         0
         empty)]
       [else
        (cont
         (rest stream)  ;; disfirstd head of list
         0
         (list (first stream)))]))
   ))


(define prim ;; a stream of 2 3 4 5 6 
  (Gen
   0
   2
   (λ (inp state cont)
     (cont 0 (add1 state) (list state)))
   ))


(define primes ;; outputs 2 3 5 7 11
  (Gen
   0 
   prim 
   (lambda (stream state cont) 
     (cont
      0  
      (Kill (first state) state) ;; check next num
      (list (first state)))
     )))

(define primodd ;; a stream of 3 5 7 9 11 13 15
  (Gen
   0
   3
   (λ (inp state cont)
     (cont 0 (+ 2 state) (list state)))
   ))

(define primesodd ;; outputs 3 5 7 11
  (Gen
   0 
   primodd 
   (lambda (stream state cont) 
     (cont
      0  
      (Kill (first state) state) ;; check next num
      (list (first state)))
     )))

(define twinprimes ;; a stream of all pairs [list with 2 elements] of twin prime numbers; e.g.,
  ;;   (outstream (take 5 twinprimes)) ;; outputs (3 5) (5 7) (11 13) (17 19) (29 31) ;; outputs 2 3 5 7 11
  (Gen
   0 
   primesodd 
   (lambda (stream state cont) 
     (cond
       [(= 2 (- (second state) (first state)))
        (cont
         0  
         (Kill (first state) state) ;; check next num
         (list (list (first state) (second state))))]
       [else
        (cont
         0  
         (Kill (first state) state) ;; check next num
         empty)]
       )
     )))