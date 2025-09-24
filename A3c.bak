;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname A3c) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
#|
(define (tree-height n)
  (if (= n 1) (make-node empty empty)
      (make-node (tree-create (- n 1)) (make-node empty empty))
  )
  )
|#

;; c
(define (tree-create-c n)
  (if (= n 1) (make-node empty empty)
      (make-node n (tree-create-c (- n 1)))
  )
  )

;;f
(check-expect (tree-height (tree-create-c 7)) 7)
(check-expect (tree-height (tree-create-c 1)) 1)



(define (tree-height n)
  (if (= n 1) (make-node empty empty)
      (make-node (tree-create (- n 1)) (make-node empty empty))
  )
  )




(check-expect (ismax 7) true)
(check-expect (ismax 1) true)
(check-expect (ismax 14) false)

(define (ismax n)
  (istwo (+ n 1))
  )

(define (istwo n)
  (cond
    [(= n 1) true]
    [(= (remainder n 2) 1) false]
    [else (istwo (/ n 2))]
    )
  )