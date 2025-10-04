;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname A5b) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(check-expect (evens (list 0 1 2 3 4 5 6)) (list 0 2 4 6))
(check-expect (evens (list 0 1 2 3 4 5)) (list 0 2 4))

;; evens: (listof any) -> (listof any)
(define (evens lst)
  (evensh lst 0)
  )

;; evensh: (listof any) nat -> (listof any)
(define (evensh lst parity)
  (cond
    [(empty? lst) empty]
    [(= parity 0) (cons (first lst) (evensh (rest lst) 1))]
    [(= parity 1) (evensh (rest lst) 0)]
    )
  )




(check-expect (evens (list 0)) (list 0))
(check-expect (evens empty) empty)

(check-expect (interleave (list 0 1 2 3 4 5 6) (list 0 1 2 3 4 5 6)) (list 0 0 1 1 2 2 3 3 4 4 5 5 6 6))
(check-expect (interleave (list 0 1 2 3 4 5) (list 5 4 3 2 1 0)) (list 0 5 1 4 2 3 3 2 4 1 5 0))

;; evens: (listof any) (listof any) -> (listof any)
(define (interleave l1 l2)
  (interleaveh l1 l2 0)
  )

;; evensh: (listof any) nat -> (listof any)
(define (interleaveh l1 l2 parity)
  (cond
    [(and (empty? l1) (empty? l2)) empty]
    [(empty? l1) (cons (first l2) (interleaveh l1 (rest l2) 0))]
    [(empty? l2) (cons (first l1) (interleaveh (rest l1) l2 0))]
    [(= parity 0) (cons (first l1) (interleaveh (rest l1) l2 1))]
    [(= parity 1) (cons (first l2) (interleaveh l1 (rest l2) 0))]
    )
  )


(check-expect (interleave (list 0) (list 1)) (list 0 1))
(check-expect (interleave (list 0 1 2 3) (list 0 1 2 3 4 5 6)) (list 0 0 1 1 2 2 3 3 4 5 6))