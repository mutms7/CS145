#lang racket


  (provide c x0 findx)

  (define c 3)
  (define x0 1)

 ;; we need to show f(x) ≤ c*g(x) for all x ≥ x0
  ;;   that is, there does not exist x >= x0 such that
  ;;   f(x) > c*g(x)
#|
f(x)=2^x, g(x)=2^(x-1)

2^x = 2*2^(x-1)
f(x) = 2 * g(x)
for f(x) > c*g(x):
2*g(x) > c*g(x)
2 > c

Therefore f(x) is O(g(x))
  |#


  (define (findx c x0)
    (cond
      [(> 2 c) x0]
      [else 'impossible]
      ))

#|

(findx 1/5 1)
2^x > 1/5*2^(x-1)
2 > 
True:
(findx 







|#


