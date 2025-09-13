;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname A2c) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define (coprime? x y) (divis (min x y) x y))
;; tests
(check-expect (coprime? 5 10) false)
(check-expect (coprime? 16 27) true)
(check-expect (coprime? 15 20) false)
(check-expect (coprime? 100 89) true)

(define (divis n i1 i2) (cond
                          [(= n 1) true]
                          [(and (= 0 (modulo i1 n)) (= 0 (modulo i2 n))) false]
                          [else (divis (- n 1) i1 i2)]
                          )
  )
;; tests
(check-expect (divis 5 5 10) false)
(check-expect (divis 16 16 27) true)
(check-expect (divis 15 15 20) false)
(check-expect (divis 89 100 89) true)