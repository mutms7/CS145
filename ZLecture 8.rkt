;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |ZLecture 8|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require racket/base)

(define (biglist n)
  (cond
    [(zero? n) empty]
    [else (cons (random 10000) (biglist (sub1 n)))]
    )
  )

(define (biglist2 n)
  (cond
    [(zero? n) empty]
    [else (append (list (random 10000)) (biglist (sub1 n)))]
    )
  )

(define (biglist3 n)
  (cond
    [(zero? n) empty]
    [else (append (biglist (sub1 n)) (list (random 10000)))]
    )
  )

(define (my-append L1 L2)
  (cond
    [(empty? L1) L2]
    [else (cons (first L1) (my-append (rest L1) L2))]
    )
  )

(define (my-reverse L)
  (cond
    [(empty? L) empty]
    [else (append (my-reverse (rest L)) (list (first L)))]))


(define-struct node (left right key))

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

(define (height h)
  (cond [(empty? h) 0]
        [else
         (+ 1
            (max (size (node-left h))
            (size (node-right h)))
            )
         ]
        )
  )

(define (bigtree n)
  (cond
    [(zero? n) empty]
    [else (insert-bst (bigtree (sub1 n)) (random 10000))]))



(define (map f L)
  (cond
    [(empty? L) empty]
    [else (cons (f (first L)) (map f (rest L)))]))

(define (filter pred? L)
  (cond
    [(empty? L) empty]
    [(pred? (first L))
     (cons (first L) (filter pred? (rest L)))]
    [else (filter pred? (rest L))]
    )
  )

(define (double x) (+ x x))
(double 3)

