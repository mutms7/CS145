#lang racket
  (provide c x0 findx)

  (define c 100)
  (define x0 10)

;; f(x)=x, g(x)=100*floor(x/10)

  ;; 
  ;; start with a true inequality, and maintain
#|
x = x
x = 1/100 (100x)
for all x >= 1,
floor(x/10) < 100x [for all x >= 10]
Therefore,
x < 1/100 (100*100*floor(x/10)) < 1/100 (100 (100x)) 
f(x) < 100 g(x) [for all x >= 10]


Therefore, (findx 100 10) must produce impossible,
and f(x) is O(g(x))
|#


  (define (findx c x0)
    (cond
      [(> x0 (* 100 c (floor (/ x0 10)))) x0]
      [(> (+ 1 x0) (* 100 c (floor (/ (+ 1 x0) 10)))) (+ 1 x0)]
      [(> (+ 2 x0) (* 100 c (floor (/ (+ 2 x0) 10)))) (+ 2 x0)]
      [(> (+ 3 x0) (* 100 c (floor (/ (+ 3 x0) 10)))) (+ 3 x0)]
      [(> (+ 4 x0) (* 100 c (floor (/ (+ 4 x0) 10)))) (+ 4 x0)]
      [(> (+ 5 x0) (* 100 c (floor (/ (+ 5 x0) 10)))) (+ 5 x0)]
      [(> (+ 6 x0) (* 100 c (floor (/ (+ 6 x0) 10)))) (+ 6 x0)]
      [(> (+ 7 x0) (* 100 c (floor (/ (+ 7 x0) 10)))) (+ 7 x0)]
      [(> (+ 8 x0) (* 100 c (floor (/ (+ 8 x0) 10)))) (+ 8 x0)]
      [(> (+ 9 x0) (* 100 c (floor (/ (+ 9 x0) 10)))) (+ 9 x0)]
      
      [else 'impossible]))

#|
f(x) > c*g(x)
x > c*(100*floor(x/10)) > c*(100*(x-10)/10)

2000 > 2001/200 * 200

|#



