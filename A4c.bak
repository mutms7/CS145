;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname A4c) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(check-expect (tree-count -1110) 0)
(check-expect (tree-count 1) 1)
(check-expect (tree-count 2) 2)
(check-expect (tree-count 3) 5)


(define (tree-count n)
  (cond [(< n 0) 0]
        [(= n 0) 1]
        [else (t-cycle 0 (- n 1))]
        )
  )

(define (t-cycle at max)
  (cond
    [(> at max) 0]
    [else (+ (t-cycle (add1 at) max)
             (*
              (tree-count at)
              (tree-count (- max at))
              )
             )]
    )
  )

(check-expect (tree-count 4) 14)
(check-expect (tree-count 5) 42)
(check-expect (tree-count 6) 132)