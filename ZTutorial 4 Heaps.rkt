;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |ZTutorial 4 Heaps|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct node (l r k))

(define (insert1 h n)
  ;;optimized and inserting larger value replaces larger value and inserts smaller value into smaller subtree
  ;;creates balanced heap
  (cond
    [(empty? h) (make-node empty empty n)]
    [(> n (node-k h)) (if (< (size (node-l h)) (size (node-r h)))
                             (make-node
                              (insert (node-l h) (node-k h))
                              (node-r h)
                              n
                              )
                             (make-node
                              (node-l h)
                              (insert (node-r h) (node-k h))
                              n
                              )
                             )]
    [else
     (if (< (size (node-l h)) (size (node-r h)))
                             (make-node
                              (insert (node-l h) n)
                              (node-r h)
                              (node-k h)
                              )
                             (make-node
                              (node-l h)
                              (insert (node-r h) n)
                              (node-k h)
                              )
                             )
     ]
  )
  )


(define (size h)
  (cond [(empty? h) 0]
        [else
         (+ 1
            (size (node-l h))
            (size (node-r h))
            )
         ]
        )
  )

(define (insert H n) ;;ugly
  ;; creates a diagonal line
  (cond
    [(empty? H) (make-node empty empty n)]
    [(> n (node-k H))
     (make-node
      H
      empty
      n
      )
     ]
    [else (make-node
           (insert (node-l H) n)
           (node-r H)
           (node-k H))]
  )
  )

(define (remov h)
  (cond [(and (empty? (node-l h)) (empty? (node-r h)))
         empty]
        [(empty? (node-r h)) (make-node
                            (remov (node-l h))
                            empty
                            (node-k (node-l h)))]
        [(empty? (node-l h)) (make-node
                            empty
                            (remov (node-r h))
                            (node-k (node-r h)))]
        [(< (node-k (node-l h)) (node-k (node-r h))) (make-node (node-l h) (remov (node-r h)) (node-k (node-r h)))]
        [else (make-node (remov (node-l h)) (node-r h)  (node-k (node-l h)))]

        )
  )

(define (lst->heap lst)
  (cond [(empty? lst) empty]
        [else (insert (lst->heap (rest lst)) (first lst))]
        )
  )

(define (heap->lst h)
  (cond [(empty? h) empty]
        [else (cons (node-k h) (heap->lst (remov h)))]
        )
  )

;;ADT dequeue implemented with only structs
;;pushfront
;;pushback
;;popfront
;;popback
;;peekfront
;;peekback


;;tree-mirror
;;write a function that takes an undecorated binary tree and mirrors it about its rootnode

;;bst-closest nodes
;;write a function that takes a bst and finds the minimal difference between two keys

(remov (insert1 (insert1 (insert1 (insert1 (insert1 (insert1 (insert1 empty 9) 12) 4) 33) 5) 1) 19))
