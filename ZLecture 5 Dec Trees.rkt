;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |ZLecture 5 Dec Trees|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct node (size height left right))
;;(node-size t)
;;(node-height t)
;;(node-left t)
;;(node-right t)


(define (height t) (if (empty? t) 0 (node-height t)))

(define (merge t1 t2)
  (make-node
   (+ (node-size t1) (node-size t2) 1)
   (add1 (max (node-height t1) (node-height t2)))
   t1
   t2
   )
  )


(define-struct bst (key l r))
  ;;(bst-key t)
   ;;(bst-l t)
   ;;(bst-r t)

(define (find t n)
  (cond
    [(empty? t) false]
    [(= (bst-key t) n) true]
    [(> (bst-key t) n) (find (bst-l t) n)]
    [(< (bst-key t) n) (find (bst-r t) n)]
    )
  )

  (find 
        (make-bst 16
                  (make-bst 8
                            (make-bst 4 empty empty)
                            (make-bst 12 empty empty))
                  (make-bst 24
                            (make-bst 20 empty empty)
                            (make-bst 28 empty empty))
                  )
        15
        
        )