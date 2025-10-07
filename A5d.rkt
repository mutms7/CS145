;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname A5d) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
 (define-struct node (left right key))
   ;; A BST is either:
   ;;  * empty, or 
   ;;  * (make-node BST BST Int) such that:
   ;;       - all the keys in left are less than key
   ;;       - all the keys in right are greater than key
   ;;       - left and right are both BSTs

(check-expect (bst->list (make-node (make-node empty empty 1) (make-node empty empty 10) 5)) (list 1 5 10))
(check-expect (bst->list empty) empty)


;; bst->list: bst -> list
(define (bst->list t)
    (bst->listh t empty)
  )

;; bst->list: bst list -> list
(define (bst->listh t list)
  (cond
    [(empty? t) empty]
    [(and (empty? (node-left t)) (empty? (node-right t))) (cons (node-key t) list)]
    [(empty? (node-left t)) (cons (node-key t) (bst->listh (node-right t) list))]
    [(empty? (node-right t)) (bst->listh (node-left t) (cons (node-key t) list))]
    [else (bst->listh (node-left t) (cons (node-key t) (bst->listh (node-right t) list)))]
  )
  )

(check-expect (bst->list (make-node (make-node '() (make-node '() '() 12) 10) (make-node (make-node (make-node '() '() 16) '() 17) '() 20) 15)) (list 10 12 15 16 17 20))
(check-expect (bst->list (make-node (make-node (make-node (make-node (make-node '() '() 10) '() 11) '() 12) '() 13) '() 14)) (list 10 11 12 13 14))