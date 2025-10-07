;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |ZLecture 9 Lambda|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define L (list 1 5 10 15 20 18 12 -10))

(define double2 (lambda (x) (+ x x)))
(map (lambda (x) (+ x x)) (list -1 2 3))
(define (make-adder n) (lambda (x) (+ x n)))
(define make-adder2 (lambda (n) (lambda (x) (+ x n))))

;; map: (Any -> Any) (listof Any) -> (listof Any) NOT SPECIFIC ENOUGH
;; map: (X -> Y) (listof X) -> (listof Y) yessir

;; fliter: (X -> Bool) (listof X) -> (listof X)

;;make-adder: Num -> (Num -> Num)



;; foldr             [fold from the right]

(foldr (lambda (fir res-of-rest) (+ fir res-of-rest)) 0 L)

;; foldr: (X Y -> Y) Y (listof X) -> Y

(foldr (lambda (fir ror) (+ (string-length fir) ror))
       0
       (list "abc" "troy" "hello"))

;;function notation: (foldr f b (list e1, e2, ... en))
;; gives (f e1 (f e2 ... (f en-1 ( en b))...))

;;foldl        [fold from the left]

(define (sumlist L) (sumhelp L 0))
(define (sumhelp L acc)
  (cond
    [(empty? L) acc]
    [else (sumhelp (rest L) (+ (first L acc)))]
   ))

;; (foldl combine init L)

(foldl (lambda (fir ror) (+ (string-length fir) ror))
       0
       (list "abc" "troy" "hello"))

;; build-list: Nat (Nat-> X) -> (listof X)

;; sort: (listof X) (X X -> Bool) -> (listof X)
(sort L <)
(sort L >)
(sort (list "abc" "troy" "hello" "d") string<?)
(sort L (lambda (x y) (zero? (random 2))))
(sort L =)