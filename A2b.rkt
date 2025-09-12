;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname A2b) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))


(define (compare-steps-k n) (+ 3 (* n 5)))
(define (compare-steps-l n) (+ 3 (* n 5)))
(define (compare-steps-m n) (+ 3 (* n 5)))
(define (compare-steps-o n) (+ 3 (* n 5)))
(define (step-count-t n) (+ 3 (* n 5)))

(define (fib-steps n)
  (if (> 2 n) 3 
      (* 3
         (fib-help (- n 2) 1 1))))
(define (fib-help n fn-2 fn-1)
  (if (= n 0) (+ fn-2 fn-1 2)
      (fib-help (- n 1) fn-1 (+ fn-2 fn-1 2)))
      ) 



(check-expect (fib-steps 0) 3) ;; 3 1
(check-expect (fib-steps 1) 3) ;; 0*3 + 3 1
(check-expect (fib-steps 2) 12) ;; 3*3 + 3 4
(check-expect (fib-steps 3) 21) ;; 6*3 + 3 7
(check-expect (fib-steps 4) 39) ;; 12*3 + 3 13
;; 66 21*3 + 3 22
;; 111 36*3 + 3 37
