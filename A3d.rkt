;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname A3d) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct node (left right))

(define (tree-similar? t1 t2)
  (cond
    [(and (number? t1) (number? t2)) (= t1 t2)]
    [(and (not (and (node? t1) (node? t2))) (or (node? t1) (node? t2))) false]
    [(and (empty? t1) (empty? t2)) true]
    [(or (empty? t1) (empty? t2)) false]
    [else (and (tree-similar? (node-left t1) (node-left t2)) (tree-similar? (node-right t1) (node-right t2)))]
            )
  )

(define (tree-create-min n)
  (cond
    [(= n 0) empty]
    [(= n 1) (make-node empty empty)]
    [else (make-node (tree-create-min (floor (/ (- n 1) 2)))
                 (tree-create-min (ceiling (/ (- n 1) 2)))
                 )]
  )
  )


(define (tree-size t)
  (if (empty? t) 0 (+ 1 (tree-size (node-left t)) (tree-size (node-right t))
                      )
      )
  )

;; 1

(check-expect (tree-similar? (tree-grow-min (tree-create-min 6)) (tree-create-min 7)) true)
(check-expect (tree-similar? (tree-grow-min (tree-create-min 2)) (tree-create-min 3)) true)
(check-expect (tree-similar? (tree-grow-min (tree-create-min 0)) (tree-create-min 1)) true)
(check-expect (tree-similar? (tree-grow-min (tree-create-min 14)) (tree-create-min 15)) true)


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


;; b

(check-expect (tree-similar? (tree-shrink-min (tree-create-min 1)) (tree-create-min 0)) true)
(check-expect (tree-similar? (tree-shrink-min (tree-create-min 1)) (tree-create-min 0)) true)
(check-expect (tree-similar? (tree-shrink-min (tree-create-min 1)) (tree-create-min 0)) true)
(check-expect (tree-similar? (tree-shrink-min (tree-create-min 1)) (tree-create-min 0)) true)
(check-expect (tree-similar? (tree-shrink-min (tree-create-min 1)) (tree-create-min 0)) true)



(define (tree-height n)
  (if (empty? n) 0
      (+ 1 (max (tree-height (node-left n))
                (tree-height (node-right n))
                )
         )
      
  )
  )

(define (tree-shrink-min n)
  (cond
    [(empty? n) empty]
    [(and (empty? (node-right n)) (empty? (node-left n))) empty]
    [(> (tree-height (node-left n)) (tree-height (node-right n)))
     (make-node
        (tree-shrink-min
         (node-left n))
        (node-right n)
        )
     
       ]
    [else
     (make-node
        (node-left n)
        (tree-shrink-min
         (node-right n)))
       ]
    )
  )
