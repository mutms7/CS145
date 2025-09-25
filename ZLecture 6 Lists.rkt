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
(check-expect (nth L1 5) empty)

(define (nth L n) ;;useless    i lied useful
  (cond
    [(= n 1) (first L)]
    [(empty? (rest L)) 'fuck you] ;; should throw exception exceeds index
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

