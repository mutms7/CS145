;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname A2a) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define (step-count-k n) (+ 3 (* n 5)))
(define (step-count-l n) (if (= n 0) 3 (+ 6 (* 2 (step-count-l (sub1 n))))))
(define (step-count-m n) (if (= n 0) 3 (+ 3 (* (+ 1 (inexact->exact(log n 2))) 5))))
(define (step-count-n n) (if (< n 1) 3 (+ 3 (* (+ 1 (floor (inexact->exact(log n 2)))) 5))))
(define (step-count-o n) (if (< n 1) 3 (+ 6 (* 2 (step-count-o (quotient n 2))))))

(check-expect (step-count-n 100) 38)