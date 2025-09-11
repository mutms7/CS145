;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |ZLecture 3 Fibonacci|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define (fib x)
  (cond
    [[zero? x] 0]
    [(= x 1) 1]
    [else (+ (fib (- x 1)) (fib (- x 2)))]
    )
  )
(define (fib2 x)
  (cond
    [[zero? x] 0]
    [(= x 1) 1]
    [else (fib-helper (- x 2) 0 1)]
    )
  )
(define (fib-helper n fn-2 fn-1)
  (cond
    [(zero? n) (+ fn-2 fn-1)]
    [else (fib-helper (sub1 n) fn-1 (+ fn-1 fn-2))]
  )
)
(fib2 10)

