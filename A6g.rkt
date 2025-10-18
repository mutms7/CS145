;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname A6g) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; 1



(check-expect (map2argfn (list + - * / list) (list 3 2))
              (list 5 1 6 1.5 (list 3 2)))
(check-expect (map2argfn empty (list 1 0))
              empty)

;;map2argfn: (listof (Num Num -> X)) (listof Num) -> (listof X)
(define (map2argfn fns two)
  (foldr (lambda (function rst) (cons ((lambda (fn n1 n2) (fn n1 n2)) function (first two) (first (rest two))) rst)) empty fns) 
  )







;; 2

(check-expect (intersection (list 1 'boo true 19 22) (list 22 false 'boo 'hi 19))
              (list 'boo 19 22))
(check-expect (intersection (list 1) empty)
              empty)


;;intersection: (listof Any) (listof Any) -> (listof Any)
(define (intersection list1 list2)
  (foldr
   (lambda (firstl1 rest)
  (cond
    [(foldr (lambda (fir lst) (or (equal? fir firstl1) lst)) false list2) (cons firstl1 rest)]
    [else rest]
  )
     )
  empty
  list1)
  
  )





;; 3



(check-expect (remove-duplicates (list 1 2 3 3 4 2 3 1 3))
              (list 1 2 3 4))
(check-expect (remove-duplicates empty)
              empty)


;;remove-duplicates: (listof Num) -> (listof Num)
(define (remove-duplicates lst)
  (reverse
   ((lambda (reversed) 
   (foldr
   (lambda (firstl1 rest)
  (cond
    [(foldr (lambda (fir lst) (or (equal? fir firstl1) lst)) false rest) rest]
    [else (cons firstl1 rest)]
  )
     )
  empty
  reversed)
      ) (reverse lst)))
  )

;; 4


(check-expect (subseq (list 'a 'b 'c 'd 'e 'f) 3 4)
              (list 'd))
(check-expect (subseq (list 'a 'b 'c 'd 'e 'f) 2 4)
              (list 'c 'd))
(check-expect (subseq (list 'a 'b 'c 'd 'e 'f) 1 1000)
              (list 'b 'c 'd 'e 'f))
(check-expect (subseq (list 'a 'b 'c 'd 'e 'f) 0 4)
              (list 'a 'b 'c 'd))
(check-expect (subseq (list 'a 'b 'c 'd 'e 'f) 5 4)
              empty)
(check-expect (subseq (list 'a 'b 'c 'd) 2 400)
              (list 'c 'd))


;;subseq : (listof Num) Nat Nat -> (listof Num)
(define (subseq lst i j)
  
  
  (if (>= i j) empty
  (reverse
  ((lambda (l lnth2)
     (foldr
      (lambda (fir res1)
        (cond ;; 6 4 c d e f f e d c
          #|
          f e d c
          
          0 1 2 3
          2 2 2 2
          |#
          [(>= (- lnth2 (+ (length res1) 1)) (- (length lst) j)) (cons fir res1)]
          [else res1]
          )
        )
      empty
      l
      )
     )
   (reverse 
  ((lambda (l lnth)
     (foldr
      (lambda (fir res)
        (cond ;; 6 1 >= 1 && 1 < 4
          [(>= (- lnth (+ (length res) 1)) i) (cons fir res)]
          [else res]
          )
        )
      empty
      l
      )
     )
   lst
   (length lst))
  )
   (length (reverse 
  ((lambda (l lnth)
     (foldr
      (lambda (fir res)
        (cond ;; 6 1 >= 1 && 1 < 4
          [(>= (- lnth (+ (length res) 1)) i) (cons fir res)]
          [else res]
          )
        )
      empty
      l
      )
     )
   lst
   (length lst))
  )))
)

  )
  
  )