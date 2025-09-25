;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |ZLecture 6 Lists|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define L1 (cons 5 (cons 10 (cons 15 (cons 20 empty)))))

(first L1)
(rest L1)

(check-expect (nth L1 1) 5)
(check-expect (nth L1 2) 10)
(check-expect (nth L1 3) 15)
(check-expect (nth L1 4) 20)
(check-expect (nth L1 5) 'fuckyou)

(define (nth L n) ;;useless    i lied useful
  (cond
    [(= n 1) (first L)]
    [(empty? (rest L)) 'fuckyou] ;; should throw exception exceeds index
    [else (nth (rest L) (sub1 n))]
    )
  )

(define y (cons 4 empty))
(define z (cons y y))
(define z2 (cons (cons 5 empty) y)) ;; (cons (cons 5 empty) (cons 4 empty))
(define z3 (cons z2 z2))


(define Lalt (list 67 45 10))

(define t1 (cons 45 empty))
(define t2 (list 45 empty))
t1
t2

(define bigL2 (list 10 20 30 40 50 60 70 80 90))

(empty? z3) ;; constant runtime
(zero? (length z3)) ;; linear runtime NONO

#|
(define (my-list-ref L i) ;; all natural number functions
  (cond
    [(zero? i) ...]
    [else ... i ... (my-list-ref ... (sub1 i))]
    )
  )
|#

(define (my-list-ref L i) ;; all natural number functions
  (cond
    [(zero? i) (first L)]
    [else (my-list-ref (rest L) (sub1 i))]
    )
  )
(define l (list 1 2 3 4 5))
(check-expect (my-list-ref l 0) 1)
(check-expect (my-list-ref l 1) 2)
(check-expect (my-list-ref l 2) 3)
(check-expect (my-list-ref l 3) 4)
(check-expect (my-list-ref l 4) 5)

(check-expect (my-length empty) 0)
(check-expect (my-length l) 5)

(define (my-length L)
  (cond
    [(empty? L) 0]
    [else (add1 (my-length (rest L)))]
)
)

(check-expect (my-member? 3 l) true)
(check-expect (my-member? 33 l) false)

(define (my-member? n L)
  (cond
    [(empty? L) false]
    [(= (first L) n) true]
    [else (my-member? n (rest L))]
)
)

(define (my-append L1 L2)
  (cond
    [(empty? L1) false]
    [(= (first L) n) true]
    [else (my-member? n (rest L))]
)
)
