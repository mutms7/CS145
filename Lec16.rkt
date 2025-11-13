#lang lazy

(define True (λ(x y) x)) ;; uncurried form.  In curried form: (λ(x) (λ(y) x))
(define False (λ(x y) y))
(define Display (λ(b) (b 'true 'false)))

(define If (λ(b t f) (b t f)))

(define Cons (λ(a b) (λ (s) (s a b))))
(define First (λ(c) (c True)))
(define Rest (λ(c) (c False)))
(define Empty (λ(c) True))
(define Empty? (λ(c) (c (λ(x y) False))))

(define x (Cons True (Cons False (Cons True Empty))))
(define y (Cons True (Cons True (Cons True Empty))))

(define And-list
  (λ(lst) (If (Empty? lst)
              True
              (If (First lst) (And-list (Rest lst)) False))))


;;; don't use recursion, but let's use multi-parameter functions

(define And-list-helper
  (λ(self lst)  (If (Empty? lst)
                    True
                    (If (First lst) (self self (Rest lst)) False))))
(define And-list1 (λ(lst) (And-list-helper And-list-helper lst)))

;; get real with λ-calculus

(define And-curry-helper 
   (λ(self) (λ (lst)  (If (Empty? lst)
                    True
                    (If (First lst) ((self self) (Rest lst)) False)))))
(define And-list-curry
  (And-curry-helper And-curry-helper))



;; beta-reduction
(define And-curry-no-helper
  ((λ(self) (λ (lst)  (If (Empty? lst)
                    True
                    (If (First lst) ((self self) (Rest lst)) False))))
   (λ(self) (λ (lst)  (If (Empty? lst)
                    True
                    (If (First lst) ((self self) (Rest lst)) False))))))

(define Or-curry-no-helper
  ((λ(self) (λ (lst)  (If (Empty? lst)
                    False
                    (If (First lst) ((self self) (Rest lst)) True))))
   (λ(self) (λ (lst)  (If (Empty? lst)
                    False
                    (If (First lst) ((self self) (Rest lst)) True))))))

;;

;; abstract away the And
(define Abstract-curry-no-helper
  (λ(f) ;;takes a function
    ((λ(self) (f (self self))) ;;makes a curry with that function
     (λ(self) (f (self self))))))

(define And-abstracted
  (Abstract-curry-no-helper ;; expects a function
   ;; calls a function that does a thing
   (λ(And-lst) ;; the argument is a function, that is applied into itself NOT RECURSION
     (λ(lst) (If (Empty? lst)
                 True
                 (If (First lst)
                     (And-lst (Rest lst))
                     False))))))

(define Or-abstracted
  (Abstract-curry-no-helper 
   (λ(Or-lst) 
     (λ(lst) (If (Empty? lst)
                 False
                 (If (First lst)
                     (Or-lst (Rest lst))
                     True))))))
 

;; Y = (λf.((λx.(f (x x))) (λx.(f (x x)))))
;; Y g = (λf.((λx.(f (x x))) (λx.(f (x x))))) g
;; =B> (λx.(g (x x))) (λx.(g (x x)))
;; =B> g (λx.(g (x x))) (λx.(g (x x)))
;; = g (Y g)


;;Numbers:
;; 0 = λf.λx.x
;; 1 = λf.λx.(f x)
;; 2 = λf.λx.(f (f x))

;; succ = λn.λf.λx.(n f x)
;; i.e. add1 in Racket
;; plus m n = m + n = f^(m+n) = f^m*f^n
;; = λm.λn.λf.λx.(m f (n f x))


;;Unary Lists:
