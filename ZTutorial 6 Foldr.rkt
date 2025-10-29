;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |ZTutorial 6 Foldr|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;myfoldl: (X -> Y) Y (listof X) -> (listof Y)
(define (myfoldl f b lst) (foldr f b (reverse lst)))


;;mymap: (X -> Y) (listof X) -> (listof Y)
(define (mymap f lst) (foldr (λ (x y) (cons (f x) y)) empty lst))

;;myfilter: (X -> Bool) (listof X) -> (listof Y)
(define (myfilter f lst) (foldr (λ (x y) (if (f x) (cons x y) y)) empty lst))

#|

if f is O(g) and g is in O(h), then f is in O(h)

f is O(g) implies there exists an x1 for all c1>0 for which x>=x1 means f(x) < c1*g(x)
g is O(h) implies there exists an x2 for all c2>0 for which x>=x2 means g(x) < c2*h(x)

Pick c = c1*c2 and x0 = max(x1, x2)
Now for x >= x0,
f(n) < c1*g(x) < c1*c2*h(x) = c*h(x)

|#