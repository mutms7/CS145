;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname A3c) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct node (left right))
;; (node-left z)
;; (node-right z)
#|
(define (tree-height n)
  (if (= n 1) (make-node empty empty)
      (make-node (tree-create (- n 1)) (make-node empty empty))
  )
  )
|#

;; c
(define (tree-create-c n)
  (cond [(= n 1) (make-node empty empty)]
        [(= n 0) empty]
      [else (make-node empty (tree-create-c (- n 1)))]
  )
  )
;;f
(check-expect (tree-height (tree-create-c 7)) 7)
(check-expect (tree-height (tree-create-c 1)) 1)


(define (tree-height n)
  (if (empty? n) 0
      (+ 1 (max (tree-height (node-left n))
                (tree-height (node-right n))
                )
         )
      
  )
  )

;; g

(check-expect (tree-height (tree-create-max 7)) 7)
(check-expect (tree-height (tree-create-max 1)) 1)


(define (tree-create-max n)
  (cond [(= n 1) (make-node empty empty)]
        [(= n 0) empty]
      [else (make-node empty (tree-create-max (- n 1)))]
  )
  )


;; h

(check-expect (tree-height (tree-create-min 7)) 3)
(check-expect (tree-height (tree-create-min 2)) 2)
(check-expect (tree-height (tree-create-min 31)) 5)
(check-expect (tree-height (tree-create-min 3)) 2)



(define (tree-create-min n)
  (cond
    [(= n 0) empty]
    [(= n 1) (make-node empty empty)]
    [else (make-node (tree-create-min (floor (/ (- n 1) 2)))
                 (tree-create-min (ceiling (/ (- n 1) 2)))
                 )]
  )
  )

(check-expect (tree-count-max -1) 0)
(check-expect (tree-count-max 0) 1)
(check-expect (tree-count-max 1) 1)
(check-expect (tree-count-max 3) 4)
(check-expect (tree-count-max 5) 16)



;; i
(define (tree-count-max n)
  (cond [(< n 0) 0]
        [(= n 0) 1]
  [else (expt 2 (- n 1))]
  )
  )

;; j
#|
(check-expect (tree-count-min 1) 1)
(check-expect (tree-count-min 2) 2)
(check-expect (tree-count-min 3) 1)
(check-expect (tree-count-min 4) 6)
(check-expect (tree-count-min 5) 6)
(check-expect (tree-count-min 6) 4)
(check-expect (tree-count-min 7) 1)
(check-expect (tree-count-min 8) 94)
(check-expect (tree-count-min 9) 114)
|#


(define (tree-count-min n)
  (tree-count-minh n (tree-height (tree-create-min n)))
  )

(define (tree-count-minh n h)
  (cond [(< n 0) 0]
        [(= n 0) 1]
        [else (t-cycle (tmax n) n h)]
        ;; n = 2: t-cycle 1 2 2
        ;; n = 6: t-cycle 3 6 3
        ;; n = 7: t-cycle 7 7 3
        ;; n = 19: t-cycle 15 19 5
        )
  )
#|
(check-expect (t-cycle 1 2 2) 2)
(check-expect (t-cycle 3 6 3) 4)
(check-expect (t-cycle 7 7 3) 1)
|#


(define (t-cycle at max h)
  (cond
    [(> at max) 0]
    [(= h 0) 0]
    [(and (= max 0) (= h 1)) 1]
    [else (+
           (*
             (tree-count-minh (sub1 at) (sub1 h))
             (tree-count-minh (- max at) (sub1 h))
           )
           (t-cycle (add1 at) max h)
           )
           
           ]
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

(check-expect (tmax 3) 3)
(check-expect (tmax 6) 3)
(check-expect (tmax 7) 7)


(define (tmax n)
  (- (expt 2 (- (tdivide (+ n 1)) 1)) 1)
  )


(define (tdivide n)
  (cond
    [(< n 1) 0]
    [(= n 1) 1]
    [else (+ 1 (tdivide (/ n 2)))]
    )
  )
  