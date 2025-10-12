#lang racket
  (provide c x0 findx)

  (define c 1/100)
  (define x0 1)

;; f(x)=x, g(x)=100*floor(x/10)

  ;; 
  ;; start with a true inequality, and maintain
#|
x = x
x = 1/100 (100x)
for all x >= 1,
x/100 < floor(x/10)
Therefore,
x < 1/100 (100 (x/100)) < 1/100 (100*floor(x/10))
f(x) < 1/100 * x < 1/100 g(x) [for all x >= 1]
f(x) < 1/100 * g(x) [for all x >= 1]

Therefore, (findx 1/100 1) must produce impossible,
and f(x) is O(g(x))
|#


  (define (findx c x0)
    (cond
      [(> x0 (* 100 c (floor (/ x 10)))) x0]
      [else 'impossible]))

#|
f(x) > c*g(x)
x > c*(100*floor(x/10)) > c*(100*(x-10)/10)



|#

