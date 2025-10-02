;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |ZLecture 7|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(require racket/base)

(define (sumto n)
  (cond
    [(= n 0) 0]
    [else (+ n (sumto (sub1 n)))]
    )
  )

(define (sumtot n) (shelp n 0))

(define (shelp n accum)
  (if (= n 0) accum
      (shelp (sub1 n) (+ n accum))
      )
  )


;;biglist: Nat -> (listof Nat)
(define (biglist n)
  (cond
    [(zero? n) empty]
    [else (cons (random 10000) (biglist (sub1 n)))]))

;;biglist2: Nat -> (listof Nat) DONT DO THIS
(define (biglist2 n)
  (cond
    [(zero? n) empty]
    [else (append (list (random 10000)) (biglist2 (sub1 n)))]))



