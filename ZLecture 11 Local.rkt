;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |ZLecture 11 Local|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define s 1000)
(define t -3)
(define t_0 10)
(local
  [(define t 4)
   (define s (+ t t))
   (define (my-max a b) (max (add1 a) (add1 b)))]
  (my-max s t))
(local [(define x 1)] x)
(local [(define x 2000)] x)
(local
  [(define (my-length L)
     (cond
       [(empty? L) 0]
       [else (add1 (my-length (rest L)))]))
   ]
  (my-length (list 1 2 3 4 5))
  )


;; no check-expects for local functions

(define (triple x)
  (local
    ((define (double x) (+ x x)))
    (+ x (double x))))
(triple 10)

(define (dist p1 p2)
  (local
    [(define d-x (- (posn-x p1) (posn-x p2)))
    (define d-y (- (posn-y p1) (posn-y p2)))]

    (sqrt (+ (sqr d-x) (sqr d-y)))
    )
  )