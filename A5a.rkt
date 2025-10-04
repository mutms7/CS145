;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname A5a) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

(check-expect (take (list 1 2 3 4 5 6 7 8 9 10) 10) (list 1 2 3 4 5 6 7 8 9 10))
(check-expect (take (list 1 2 3 4 5 6) 0) empty)



;; drop: (listof any) nat -> (listof any)

(check-expect (drop (list 1 2 3 4 5 6) 1) (list 2 3 4 5 6))
(check-expect (drop (list 1 2 3 4 5 6) 6) empty)


(define (drop lst n)
  (cond
    [(= n 0) lst]
    [else (drop (rest lst) (sub1 n))]
    )
  )

(check-expect (drop (list 1 2 3 4 5 6 7 8 9 10) 5) (list 6 7 8 9 10))
(check-expect (drop (list 1 2 3 4 5 6) 0) (list 1 2 3 4 5 6))