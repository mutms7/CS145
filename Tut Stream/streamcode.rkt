#lang lazy
(define-struct stream-pair (this next))



;;stream:
;; (final initial)
;; (final (step initial))
;; (final (step (step initial))
;; each element gets a final function call

(define stream-first stream-pair-this)
(define (stream-rest s) ((stream-pair-next s)))
(define (stream-cons e s) (make-stream-pair e (λ () s)))

;;initial is our state
(define (stream-generate initial done? step final) ;; similar to generate
  (cond [(done? initial) empty]
        [else (make-stream-pair (final initial)
                                (λ ()
                                (stream-generate (step initial)
                                                 done?
                                                 step
                                                 final)))]))

;; in regular racket, we are cooked
(stream-generate 0 (λ(x) false) add1 (λ(x) x))