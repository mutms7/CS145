;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname A6f) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require "avl-cs145.rkt")
#|
  (define x (insertavl (insertavl (insertavl empty 10) 20) 30))
   (sizeavl x)                                          ;; 3
   (listavl x)                                          ;; (list 10 20 30)
   (node-key x)                                         ;; one of:  10, 20, 30
   (sizeavl (node-left x))                              ;; one of: 0, 1, 2
   (+ (sizeavl (node-left x)) (sizeavl (node-right x))) ;; 2
   (listavl (deleteavl x 20))                           ;; (list 10 30)
|#


(define-struct set (avl))





 ;; emptyset              ;; the empty set
(define emptyset (make-set empty))

 ;; (emptyset? s)         ;; returns true if set s is empty; false otherwise

(check-expect (emptyset? (make-set empty)) true)
(check-expect (emptyset? (make-set (insertavl empty 5))) false)

;; emptyset?: set -> bool
(define (emptyset? s)
  (cond
    [(empty? (set-avl s)) true]
    [else false]
    )
  )
;; O(1) as no recursive calls






 ;; (singleton n)         ;; a set containing the number n
(check-expect (listavl (set-avl (singleton 5))) (listavl (set-avl (make-set (insertavl empty 5)))))
(check-expect (listavl (set-avl (singleton 1))) (listavl (set-avl (make-set (insertavl empty 1)))))


;; singleton: Nat -> set
(define (singleton s)
  (make-set (insertavl empty s))
  )
;; O(1) as no recursive calls






 ;; (union s1 s2)         ;; a set containing the union of sets s1 and s2

(check-expect (listavl (set-avl (union (make-set empty) (make-set (insertavl empty 5))))) (listavl (set-avl (make-set (insertavl empty 5)))))

(check-expect (listavl (set-avl (union (make-set (insertavl empty 5)) (make-set (insertavl empty 5))))) (listavl (set-avl (make-set (insertavl empty 5)))))
(check-expect (listavl (set-avl (union (make-set (insertavl (insertavl empty 4) 5))
                     (make-set (insertavl (insertavl empty 19) 7)))))
              (listavl (set-avl (make-set (insertavl (insertavl (insertavl (insertavl empty 19) 7) 4) 5))))
              )

;; union: set set -> set
(define (union s1 s2)
  (cond
    [(> (sizeavl (set-avl s1)) (sizeavl (set-avl s2))) (make-set (foldr (lambda (fir res-of-rest) (insertavl res-of-rest fir)) (set-avl s1) (listavl (set-avl s2))))] 
    [else (make-set (foldr (lambda (fir res-of-rest) (insertavl res-of-rest fir)) (set-avl s2) (listavl (set-avl s1))))]
    )
  )
;;(listavl (set-avl empty))
;; O(N * log(M)) as the insertavl function of log(M) time is called N times, M is the size of larger set and N is size of smaller set






 ;; (intersection s1 s2)  ;; a set containing the intersection of sets s1 and s2

(check-expect (listavl (set-avl (intersection (make-set empty) (make-set (insertavl empty 5))))) (listavl (set-avl (make-set empty))))

(check-expect (listavl (set-avl (intersection (make-set (insertavl empty 5)) (make-set (insertavl empty 5))))) (listavl (set-avl (make-set (insertavl empty 5)))))
(check-expect (listavl (set-avl (intersection (make-set (insertavl (insertavl empty 4) 5))
                     (make-set (insertavl (insertavl empty 19) 5)))))
              (listavl (set-avl (make-set (insertavl empty 5))))
              )

;; intersection: set set -> set
(define (intersection s1 s2)
  (make-set (interhelp (listavl (set-avl s1)) (listavl (set-avl s2)) empty))
  )
;; interhelp is O(N*log(M)) and listavl is O(N), so intersection is O(N*log(M))


(check-expect (listavl (interhelp (list 1 2 4) (list 4 5 8) empty)) (listavl (insertavl empty 4)) )
(check-expect (listavl (interhelp (list 1 2 4) (list 1 2 4) empty)) (listavl (insertavl (insertavl (insertavl empty 1) 2) 4)))

;;interhelp: (listof Nat) (listof Nat) avl -> avl
(define (interhelp l1 l2 av)
  (cond
    [(empty? l1) av]
    [(empty? l2) av]
    [(> (first l1) (first l2)) (interhelp l1 (rest l2) av)]
    [(< (first l1) (first l2)) (interhelp (rest l1) l2 av)]
    [else (interhelp (rest l1) (rest l2) (insertavl av (first l1)))]
  )
  )
;; O(N * log(M)) as the insertavl function of log(M) time is called at most N times and the intersection of s1 and s2
;; is at largest the minimum size of s1 and s2. 








 ;; (difference s1 s2)    ;; a set containing elements in s1 but not in s2



(check-expect (listavl (set-avl (difference (make-set empty) (make-set (insertavl empty 5))))) (listavl (set-avl (make-set empty))))

(check-expect (listavl (set-avl (difference (make-set (insertavl empty 5)) (make-set (insertavl empty 5))))) (listavl (set-avl (make-set empty))))
(check-expect (listavl (set-avl (difference (make-set (insertavl (insertavl empty 4) 5))
                     (make-set (insertavl (insertavl empty 19) 5)))))
              (listavl (set-avl (make-set (insertavl empty 4))))
              )

;; difference: set set -> set
(define (difference s1 s2)
 (make-set (foldr (lambda (fir res-of-rest) (deleteavl res-of-rest fir)) (set-avl s1) (listavl (set-avl (intersection s1 s2))))) 
  )
;; O(N * log(M)) as the deleteavl function of log(M) time is called at most N times and the intersection of s1 and s2
;; is at largest the minimum size of s1 and s2. 





 ;; (size s)              ;; the number of elements in set s

(check-expect (size (make-set empty)) 0)
(check-expect (size (make-set (insertavl (insertavl (insertavl (insertavl empty 1) 5) 3) 2))) 4)

;; size: set -> Nat
(define (size s)
  (cond
    [(empty? (set-avl s)) 0]
    [else (sizeavl (set-avl s))]
    )
  )
;; O(1) as no recursive calls, sizeavl is O(1)




 ;; (nth s i)             ;; the ith element of s.
                        ;; (nth s 0) is the smallest element
                        ;; (nth s 1) is the second smallest element
                        ;; (nth s (- (size s) 1) is the largest element 

(check-expect (nth (make-set (insertavl empty 5)) 0) 5)
(check-expect (nth (make-set (insertavl (insertavl (insertavl empty 1) 2) 3)) 1) 2)
(check-expect (nth (make-set (insertavl (insertavl (insertavl empty 1) 2) 3)) 2) 3)
(check-expect (nth (make-set (insertavl (insertavl (insertavl empty 1) 2) 3)) 0) 1)
(check-expect (nth (make-set (insertavl (insertavl (insertavl (insertavl (insertavl (insertavl empty 1) 2) 3) 4) 5) 6)) 4) 5)


;; nth: set Nat -> Nat
(define (nth s i)
  (nhelp (set-avl s) i)
  )

;; nhelp: avl Nat Nat -> Nat
(define (nhelp av i)
  (cond
    [(= (sizeavl av) 0) 'error]
    [(= (sizeavl av) 1) (node-key av)]
    [(= i (sizeavl (node-left av))) (node-key av)]
    [(< i (sizeavl (node-left av))) (nhelp (node-left av) i)]
    [(> i (sizeavl (node-left av))) (nhelp (node-right av) (- i (sizeavl (node-left av)) 1))]
  )
  )

;; O(log N) as it is O(h) where h is the height of the avl, and the O(h) of an avl is always O(log N)

