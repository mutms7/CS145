;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname A4b) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct node (left right key))
#|
(define (tree-grow-min n)
  (cond
    [(empty? n) (make-node empty empty)]
    [(> (tree-size (node-left n)) (tree-size (node-right n)))
       (make-node
        (node-left n)
        (tree-grow-min
         (node-right n)))
       ]
    [else
     (make-node
        (tree-grow-min
         (node-left n))
        (node-right n)
        )
       ]
    )
  )
|#

(define (size h)
  (cond [(empty? h) 0]
        [else
         (+ 1
            (size (node-left h))
            (size (node-right h))
            )
         ]
        )
  )

;;(check-expect (bst (make-node (empty empty 0)) 0)


;; 1
(check-expect (insert-bst (make-node empty empty 0) 0)
                          (make-node empty empty 0))
(check-expect (insert-bst empty 1)
                          (make-node empty empty 1))


(define (insert-bst t n)
  (cond [(empty? t) (make-node empty empty n)]
        [(> n (node-key t))
         (make-node (node-left t) (insert-bst (node-right t) n) (node-key t))
         ]
        [(< n (node-key t))
         (make-node (insert-bst (node-left t) n) (node-right t)  (node-key t))
         ]
        [else
         (make-node (node-left t) (node-right t)  (node-key t))
         ]
        )
  )

(check-expect (insert-bst (make-node (make-node empty empty 1) (make-node empty empty 10) 5) 4)
                          (make-node (make-node empty (make-node empty empty 4) 1) (make-node empty empty 10) 5))
(check-expect (insert-bst (make-node (make-node empty empty 1) (make-node empty empty 10) 5) 11)
                          (make-node (make-node empty empty 1) (make-node empty (make-node empty empty 11) 10) 5))





;; 2
(check-expect (delete-bst (make-node empty empty 0) 0)
                          empty)
(check-expect (delete-bst (make-node empty empty 1) 0)
                          (make-node empty empty 1))


(define (delete-bst t n)
  (cond [(empty? t) empty]
        [(> n (node-key t))
         (make-node (node-left t) (delete-bst (node-right t) n) (node-key t))
         ]
        [(< n (node-key t))
         (make-node (delete-bst (node-left t) n) (node-right t)  (node-key t))
         ]
        [else ;; = n nodekey t (must remove)
         (cond 
           [(and (empty? (node-left t)) (empty? (node-right t))) empty]
           [(empty? (node-left t)) (make-node (node-left (node-right t)) (node-right (node-right t)) (node-key (node-right t)))]
           [(empty? (node-right t)) (make-node (node-left (node-left t)) (node-right (node-left t)) (node-key (node-left t)))]
           [else
            (make-node (delete-bst (node-left t) (maxbst (node-left t))) (node-right t)  (maxbst (node-left t)))
           ]
           )
         ]
        )
  )

(define (maxbst t)
  (cond
    [(and (empty? (node-left t)) (empty? (node-right t))) (node-key t)]
    [else (maxbst (node-right t))]
  )
  )

(define (minbst t)
  (cond
    [(and (empty? (node-left t)) (empty? (node-right t))) (node-key t)]
    [else (minbst (node-left t))]
  )
  )

(check-expect (delete-bst (make-node (make-node empty empty 1) (make-node empty empty 10) 5) 10)
                          (make-node (make-node empty empty 1) empty 5))
(check-expect (delete-bst (make-node (make-node empty (make-node empty empty 4) 1) (make-node empty empty 10) 5) 5)
                          (make-node (make-node empty empty 1) (make-node empty empty 10) 4))


;; 3
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