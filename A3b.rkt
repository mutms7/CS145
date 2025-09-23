;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname A3b) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct node (left right))


;; a
(check-expect (tree-similar? empty empty) true)
(check-expect (tree-similar? (make-node 1 2) (make-node 2 1)) false)

(define (tree-similar? t1 t2)
  (cond
    [(and (number? t1) (number? t2)) (= t1 t2)]
    [(and (not (and (node? t1) (node? t2))) (or (node? t1) (node? t2))) false]
    [(and (empty? t1) (empty? t2)) true]
    [(or (empty? t1) (empty? t2)) false]
    [else (and (tree-similar? (node-left t1) (node-left t2)) (tree-similar? (node-right t1) (node-right t2)))]
            )
  )

(check-expect (tree-similar? (make-node (make-node empty empty) empty) (make-node (make-node empty empty) empty)) true)
(check-expect (tree-similar? (make-node (make-node empty empty) empty) (make-node empty empty)) false)


(check-expect (tree-similar? (tree-create 2) (tree-create-c 2)) false)
(check-expect (tree-similar? (tree-create 3) (tree-create-d 3)) false)

;; change numbers to (make-node empty empty)

;; b
(define (tree-create n)
  (cond [(= n 1) (make-node empty empty)]
        [(= n 0) empty]
      [else (make-node (tree-create (- n 1)) empty)]
  )
  )
;; c
(define (tree-create-c n)
  (cond [(= n 1) (make-node empty empty)]
        [(= n 0) empty]
      [else (make-node empty (tree-create-c (- n 1)))]
  )
  )
;; d
(define (tree-create-d n)
  (cond [(= n 1) (make-node empty empty)]
        [(= n 0) empty]
      [else (make-node empty (tree-create (- n 1)))]
      )
  )

(check-expect (tree-similar? (tree-create 1) (tree-create-c 1)) true)
(check-expect (tree-similar? (tree-create 2) (tree-create-d 2)) false)

;; e
(check-expect (tree-count -1110) 0)
(check-expect (tree-count 1) 1)
(check-expect (tree-count 2) 2)
(check-expect (tree-count 3) 6)
(check-expect (tree-count 4) 18)


(define (tree-count n)
  (cond [(< n 0) 0]
        [(= n 0) 1]
  (t-cycle 0 (- n 1) 0)
  )
  )

(define (t-cycle at max count)
  (cond
    [(> at max) count]
    [(and (= 0 at) (= 0 max)) 1]
    [else (+
           (t-cycle (add1 at) max count)
           (t-cycle 0 (sub1 at) count)
           (t-cycle 0 (- max at 1) count)
           )]
    )
  )

(check-expect (tree-count 2) 2)



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