#lang racket

  (provide c x0 findx)

  (define c 'none)
  (define x0 'none)

 ;; we need to show f(x) ≤ c*g(x) for all x ≥ x0
  ;;   that is, there does not exist x >= x0 such that
  ;;   f(x) > c*g(x)

  ;; 
  ;; start with a true inequality, and maintain
  ;;    x > sqrt(x)   [for all x >= 2]
  ;;    Prove that for all c > 0, there exists x >= x0 such that
;; f(x) > c*g(x)
;; x = sqrt(x)*sqrt(x)
;; for sufficiently large x,
;;  c < sqrt(x)
;; Therefore x > c*sqrt(x)
;; Therefore there are no c and x0 that produce impossible


  (define (findx c x0)
    (max (+ 1 (expt c 2)) x0))

#|
f(x) > c*g(x)
x > c*sqrt(x)
sqrt(x) > c
x > c^2



|#

