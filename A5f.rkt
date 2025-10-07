;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname A5f) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct node (left right key))
   ;; A BST is either:
   ;;  * empty, or 
   ;;  * (make-node BST BST Int) such that:
   ;;       - all the keys in left are less than key
   ;;       - all the keys in right are greater than key
   ;;       - left and right are both BSTs



;; take: (listof any) nat -> (listof any)

(check-expect (take (list 1 2 3 4 5 6) 3) (list 1 2 3))
(check-expect (take (list 1 2 3 4 5 6) 6) (list 1 2 3 4 5 6))


(define (take lst n)
  (cond
    [(= n 0) empty]
    [(empty? lst) empty]
    [(empty? (first lst)) empty]
    [else (cons (first lst) (take (rest lst) (sub1 n)))]
                      
    )
  )

;; drop: (listof any) nat -> (listof any)

(check-expect (drop (list 1 2 3 4 5 6) 1) (list 2 3 4 5 6))
(check-expect (drop (list 1 2 3 4 5 6) 6) empty)


(define (drop lst n)
  (cond
    [(= n 0) lst]
    [else (drop (rest lst) (sub1 n))]
    )
  )


;; list->balanced: list -> bst
(define (list->balanced lst)
  (cond
    [(empty? lst) empty]
    [(= (length lst) 1) (make-node empty empty (first lst))]
    [else (make-node
           (list->balanced (take lst (floor (/ (sub1 (length lst)) 2))))
           (list->balanced (drop lst (+ 1 (floor (/ (sub1 (length lst)) 2)))))
           (list-ref lst (floor (/ (sub1 (length lst)) 2)))
           )]
  )
  )




;; mergesort: (listof Nat) -> (listof Nat)
(define (mergesort lst)
  (cond
    [(empty? lst) empty]
    [(= (length lst) 1) lst]
    [(= (length lst) 2) (mergeh (cons (first lst) empty) (rest lst))]
    [else (mergeh
           (mergesort (take lst (floor (/ (sub1 (length lst)) 2))))
           (mergesort (drop lst (floor (/ (sub1 (length lst)) 2)))))
           ]
  )
  )

(check-expect (mergeh (list 1 2 3) (list 4 5 6)) (list 1 2 3 4 5 6))
(check-expect (mergeh (list 2 4 5) (list 1 3 6)) (list 1 2 3 4 5 6))
(check-expect (mergeh (list 0) (list 1 3 6)) (list 0 1 3 6))
(check-expect (mergeh (list 2 4 5) (list 0)) (list 0 2 4 5))



;; mergeh: (listof Nat) (listof Nat) -> (listof Nat)
(define (mergeh l1 l2)
  (cond
    [(and (empty? l1) (empty? l2)) empty]
    [(empty? l1) (cons (first l2) (mergeh l1 (rest l2)))]
    [(empty? l2) (cons (first l1) (mergeh (rest l1) l2))]
    [(> (first l1) (first l2)) (cons (first l2) (mergeh l1 (rest l2)))]
    [else (cons (first l1) (mergeh (rest l1) l2))]
   )
  )
                             





(check-expect (rand->balanced (list 10 5 1)) (make-node (make-node empty empty 1) (make-node empty empty 10) 5))
(check-expect (rand->balanced empty) empty)

;; rand->balanced: list -> bst
(define (rand->balanced lst)
  (list->balanced (mergesort lst))
  )

(check-expect (rand->balanced (list 16 17 20 10 12 15 )) (make-node
 (make-node
  '()
  (make-node '() '() 12)
  10)
 (make-node
  (make-node '() '() 16)
  (make-node '() '() 20)
  17)
 15))
(check-expect (rand->balanced (list 10 11 12 13 14)) (make-node
 (make-node
  '()
  (make-node '() '() 11)
  10)
 (make-node
  '()
  (make-node '() '() 14)
  13)
 12) )
