#lang racket
  (provide c x0 findx)

  (define c 3000001)
  (define x0 1)

;; f(x)=2*x+1, g(x)=x/1000000

  ;; 
  ;; start with a true inequality, and maintain
  ;;    x = x
  ;;    2x = 2000000(x/1000000)
  ;;    2x+1 = 2000000(x/1000000) + 1
  ;;    2x+1 = (2000000+1000000/x)(x/1000000)
  ;;    f(x) = (2000000+1000000/x)*g(x)
  ;;    f(x) < 3000001*g(x) [for all x >= 1]

  ;; Therefore, (findx 1 1) must produce impossible,
  ;; and f(x) is O(g(x))

  (define (findx c x0)
    (cond
      [(< c (+ 2000000 (/ 1000000 x0))) x0]
      [else 'impossible]))

#|
f(x) > c*g(x)
(2000000+1000000/x) > c
For x = 1,
3000001 > c
Otherwise, 
|#

