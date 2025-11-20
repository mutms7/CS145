#lang racket

;; Total Order ADT used by Ordered Set ADT

;; Must provide:
;;   a membership predicate
;;   a total order
;;   min and max functions with identities
;;   user-defined associative operator and identity
;;
;;   any other operations required by the user
;;       e.g. to-hide and to-unhide

(provide to? to< to<= to> to> to>= to= to!= to-min to-min-ident 
         to-max to-max-ident to-op to-op-ident to-hide to-unhide)

;; For this example, we use a number hidden in a struct

   (define-struct to-rep (hidden count))
   (define (to-unhide x) (list (to-rep-hidden x) (to-rep-count)))
   (define (to-hide x) (make-to-rep (first x) (second x)))

;; membership predicate

   (define (to? x) (not (empty? x)))

;; defining relation must be total, reflexive, transitive
   (define (to<= a b) (if (= (first a) (first b))
                          (<= (second a) (second b))
                          (<= (first a) (first b))))

;; derived relations
   (define (to> a b) (not (to<= a b)))
   (define (to= a b) (and (to<= a b) (to<= b a)))
   (define (to>= a b) (to<= b a))
   (define (to< a b) (to> b a))
   (define (to!= a b) (not (to= a b)))

;; min/max functions and identities
   
   (define (to-min a b) (if (to< a b) a b))
   (define to-min-ident (list +inf.0 +inf.0))

   (define (to-max a b) (if (to< a b) b a))
   (define to-max-ident (list -inf.0 -inf.0))

;; user-defined associative operator and identity

;; for this example we just add the secret numbers

   (define (to-op a b) 0)
   (define to-op-ident 0)