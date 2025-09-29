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
           [(empty? (node-left t)) (node-right t)]
           [(empty? (node-right t)) (node-left t)]
           [(> (size (node-left t)) (size (node-right t)))
            (make-node (delete-bst (node-left t) (maxbst (node-left t))) (node-right t)  (maxbst (node-left t)))
           ]
           [else
            (make-node (node-left t) (delete-bst (node-right t) (minbst (node-right t))) (minbst (node-right t)))
           ]
           )
         ]
        )
  )

(define (maxbst t)
  (cond
    
    [(empty? (node-right t)) (node-key t)]
    [else (maxbst (node-right t))]
  )
  )

(define (minbst t)
  (cond
    
    [(empty? (node-left t)) (node-key t)]
    [else (minbst (node-left t))]
  )
  )

(check-expect (delete-bst (make-node (make-node empty empty 1) (make-node empty empty 10) 5) 10)
                          (make-node (make-node empty empty 1) empty 5))
(check-expect (delete-bst (make-node (make-node empty (make-node empty empty 4) 1) (make-node empty empty 10) 5) 5)
                          (make-node (make-node empty empty 1) (make-node empty empty 10) 4))
(check-expect (delete-bst (make-node (make-node empty empty 1) (make-node empty empty 10) 5) 5)
                          (make-node (make-node empty empty 1) empty 10))
(check-expect (delete-bst (make-node empty (make-node empty empty 10) 5) 5)
                          (make-node empty empty 10))
(check-expect (delete-bst (make-node (make-node empty empty 5) empty 10) 10)
                          (make-node empty empty 5))

;; 3

(check-expect (combine-bst empty empty) empty)
(check-expect (combine-bst (make-node empty empty 1) (make-node empty empty 2))
                          (make-node empty (make-node empty empty 2) 1))



(define (inrecurse t n)
  (cond [(empty? t) n]
        [(empty? n) t]
        [(and (empty? (node-left n)) (empty? (node-right n))) (insert-bst t (node-key n))]
        [else (insert-bst (inrecurse (inrecurse t (node-left n)) (node-right n)) (node-key n))]
        )
  )

(define (combine-bst t n)
  (cond [(empty? t) n]
        [(empty? n) t]
        [else (inrecurse t n)]
        )
  )

(check-expect
(combine-bst
 (make-node (make-node (make-node empty empty 1) empty 2) empty 3)
 (make-node empty (make-node empty (make-node empty empty 6) 5) 4)
 )
(make-node (make-node (make-node '() '() 1) '() 2) (make-node (make-node (make-node '() '() 4) '() 5) '() 6) 3)
)

(check-expect
(combine-bst
 (make-node (make-node empty empty 10) (make-node empty empty 20) 15)
 (make-node (make-node empty empty 12) (make-node empty empty 17) 16)
 )
(make-node (make-node '() (make-node '() '() 12) 10) (make-node (make-node (make-node '() '() 16) '() 17) '() 20) 15)
)
