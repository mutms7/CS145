;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname A2b) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define (step-count-k n) (+ 3 (* n 5)))
(define (step-count-l n) (if (= n 0) 3 (+ 6 (* 2 (step-count-l (sub1 n))))))
(define (step-count-m n) (if (= n 0) 3 (+ 3 (* (nhelper n) 5))))
(define (nhelper n) (if (= n 0) 0 (+ 1 (nhelper (quotient n 2)))))
(define (step-count-o n) (if (< n 1) 3 (+ 6 (* 2 (step-count-o (quotient n 2))))))


(define (compare-steps-k n) (cond
                              [(> (fib n) (step-count-k n) 'more)]
                              [(= (fib n) (step-count-k n) 'same)]
                              [(< (fib n) (step-count-k n) 'fewer)]
  ))
(define (compare-steps-l n) (cond
                              [(> (fib n) (step-count-l n) 'more)]
                              [(= (fib n) (step-count-l n) 'same)]
                              [(< (fib n) (step-count-l n) 'fewer)]
  ))
(define (compare-steps-m n) (cond
                              [(> (fib n) (step-count-m n) 'more)]
                              [(= (fib n) (step-count-m n) 'same)]
                              [(< (fib n) (step-count-m n) 'fewer)]
  ))
(define (compare-steps-o n) (cond
                              [(> (fib n) (step-count-o n) 'more)]
                              [(= (fib n) (step-count-o n) 'same)]
                              [(< (fib n) (step-count-o n) 'fewer)]
  ))

(define (step-count-t n) (+ 4 (* n 5)))

(define (fib-steps n)
  (if (> 2 n) 3 
      (* 3
         (fib-help (- n 2) 1 1))))
(define (fib-help n fn-2 fn-1)
  (if (= n 0) (+ fn-2 fn-1 2)
      (fib-help (- n 1) fn-1 (+ fn-2 fn-1 2)))
      ) 
