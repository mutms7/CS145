;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |ZTutorial 5 Lambda Tail Recursion|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define (make-node l r k)
  (lambda (selector) (cond [(symbol=? selector 'l) l]
                           [(symbol=? selector 'r) r]
                           [(symbol=? selector 'k) k])))
(define (node-l n) (n 'l))
(define (node-r n) (n 'r))
(define (node-k n) (n 'k))


(define (is-even? n)
  (cond [(zero? n) true]
        [else (make-bounce is-odd? (list (sub1 n)))]))

(define (is-odd? n)
  (cond [(zero? n) false]
        [else (make-bounce is-even? (list (sub1 n)))]))

(define-struct bounce (fn param))

(define (trampoline b)
  (cond [(not (bounce? b)) b]
        [else (trampoline (apply (bounce-fn b) (bounce-param b)))]))

(trampoline (make-bounce is-even? (list 19)))

;;zippers and trampolining for recursive function tree-count