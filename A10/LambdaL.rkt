#lang lazy

(provide
 True
 False
 If
 Or
 And
 Not
 Cons
 First
 Rest
 Empty
 Empty?
 Y             ;; fixed point combinator
 Z             ;; Zero as binary Natural
 Z?            ;; Test for Z
 ADD1          ;; Add 1 to binary Natural
 ADD           ;; Adds binary Naturals
 ;;;; impure functions, only for input and output
 True?  ;; convert Boolean to 'yes or 'no
 TAN    ;; convert binary Natural number to Racket number
 NAT    ;; convert Racket number to binary Natural
 )


;; TRUE AND FALSE
(define True (λ (yes) (λ (no) yes)))
(define False (λ (yes) (λ (no) no)))
(define (True? b) ((b 'yes) 'no))

(define (If test thenpart elsepart)
  ((test thenpart) elsepart))


(define (Or a b) (If a True b))
(define (And a b) (If a b False))
(define (Not a) (If a False True))


;; LAMBDA WITH T/F
(define lIf (λ (test thenpart elsepart)
              ((test thenpart) elsepart)))

(define lOr
  (λ (test elsepart)
    ((test (λ (yes) (λ (no) yes))) elsepart))
  )

(define lAnd
  (λ (test thenpart)
    ((test thenpart) (λ (no) (λ (yes) yes))))
  )

(define lNot
  (λ (test)
    ((test (λ (no) (λ (yes) yes))) (λ (yes) (λ (no) yes)))))

(define lXor
  (λ (one two)
  ;; and
((λ (test thenpart)
    ((test thenpart) (λ (no) (λ (yes) yes))))
  ;;or
  ((λ (a1 a2)
    ((a1 (λ (yes) (λ (no) yes))) a2)) one two)

  ;;not
  ((λ (b1)
    ((b1 (λ (no) (λ (yes) yes))) (λ (yes) (λ (no) yes))))
  ((λ (c1 c2)
    ((c1 c2) (λ (no) (λ (yes) yes)))) one two))))
  )



;; LISTS
;;(define True (λ (yes) (λ (no) yes)))
;;(define False (λ (yes) (λ (no) no)))

(define (Cons first rest)
  (λ (selector) ((selector first) rest)))

(define (First pair) (pair True))
(define (Rest pair) (pair False))

(define Empty (λ (x) True))
(define (Empty? lst)
  (lst (λ (yes) (λ (no) False))))

(define Y
  (λ (f)
    ((λ (self) (f (self self)))
     (λ (self) (f (self self))))))

(define Z Empty)
(define Z? Empty?)



;;LAMBDA WITH LISTS
(define lTrue (λ (yes) (λ (no) yes)))
(define lFalse (λ (yes) (λ (no) no)))

(define lCons
  (λ(first rest) (λ (selector) ((selector first) rest))))

(define lFirst (λ(pair) (pair (λ (yes) (λ (no) yes)))))
(define lRest (λ(pair) (pair (λ (yes) (λ (no) no)))))

(define lEmpty (λ (x) (λ (yes) (λ (no) yes))))
(define lEmpty?
  (λ(lst) (lst (λ (yes) (λ (no) (λ (yes) (λ (no) no)))))))

(define lZ lEmpty)
(define lZ? lEmpty?)



(define lADD1
  ((λ (f)
    ((λ (self) (f (self self)))
     (λ (self) (f (self self)))))
   (λ (rec)
     (λ (x)
       ((((λ(lst) (lst (λ (yes) (λ (no) (λ (yes) (λ (no) no)))))) x)
         ((λ(first rest) (λ (selector) ((selector first) rest))) (λ (yes) (λ (no) yes)) (λ (x) (λ (yes) (λ (no) yes)))))
        ((((λ(pair) (pair (λ (yes) (λ (no) yes)))) x)
          ((λ(first rest) (λ (selector) ((selector first) rest))) (λ (yes) (λ (no) no)) (rec ((λ(pair) (pair (λ (yes) (λ (no) no)))) x))))
         ((λ(first rest) (λ (selector) ((selector first) rest))) (λ (yes) (λ (no) yes)) ((λ(pair) (pair (λ (yes) (λ (no) no)))) x))))))))

  

(define (ADD1 x)
  (If (Z? x)
      (Cons True Z)
      (If (First x) (Cons False (ADD1 (Rest x)))
          (Cons True (Rest x)))))

(define (ADD a b)
  (If (Z? a) b
      (If (Z? b) a
          (If (Not (First a)) (Cons (First b) (ADD (Rest a) (Rest b)))
              (If (Not (First b)) (Cons (First a) (ADD (Rest a) (Rest b)))
                  (Cons False (ADD1 (ADD (Rest a) (Rest b)))))))))

(define (TAN x)
  (If (Z? x) 0
      (If (First x)
          (add1 (* 2 (TAN (Rest x))))
          (* 2 (TAN (Rest x))))))

(define (NAT n)
  (if (zero? n) Z (ADD1 (NAT (sub1 n)))))

(define (x1 n)
  (define (x2 m)
    (if (< m 0) (void)
        (begin
          (printf "~a ~a ~a\n" n m (TAN (ADD (NAT n) (NAT m))))
          (x2 (sub1 m)))))
  (if (< n 0) (void)
      (begin (x2 n) (x1 (sub1 n)))))

;; Addition table to test ADD

;(x1 5)