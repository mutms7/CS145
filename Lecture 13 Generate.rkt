;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Lecture 13 Generate|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define initial (list 10 0))
(define (done? lst) (zero? (first lst)))
(define (step lst) (list (sub1 (first lst))
                         (+ (first lst) (second lst))))
(define (final lst) (second lst))


(define (sumto n acc)
  (cond
    [(zero? n) acc]
    [else (sumto (sub1 n) (+ n acc))]))

(define (sumto2 lst)
  (cond
    [(done? lst) (final lst)]
    [else (sumto2 (step lst))]))


(define (newsumto n)
  (local
    [(define initial (list n 0))
     (define (sumto2 initial)
       (cond
         [(done? initial) (final initial)]
         [else (sumto2 (step initial))]))]
     (sumto2 initial)))


;; generate: (listof X)
;;           ((listof X) -> Bool)
;;           ((listof X) -> (listof X))
;;           ((listof X) -> Y)
(define (generate initial done? step final)
  (cond
    [(done? initial) (final initial)]
    [else (generate (step initial) done? step final)]))

(define (sumto-gen n)
  (generate
   ;; initial
   (list n 0)
   ;; done?
   (λ (lst) (zero? (first lst)))
   ;; step
   (λ (lst) (list (sub1 (first lst))
                  (+ (first lst) (second lst))))
   ;; final
   (λ (lst) (second lst))
   ))

;; factorial using generate

(define (factorial n)
  (generate
   (list n 1)
   (λ(lst) (zero? (first lst)))
   (λ(lst) (list (sub1 (first lst))
                  (* (first lst) (second lst))))
   (λ(lst) (second lst))
     ))

(check-expect (fib 0) 0)
(check-expect (fib 1) 1)
(check-expect (fib 3) 2)
(check-expect (fib 5) 5)



(define (fib n)
  (generate
   (list n 0 1)
   (λ(lst) (zero? (first lst)))
   (λ(lst) (list (sub1 (first lst))
                (third lst)
                (+ (second lst) (third lst))))
   (λ(lst) (second lst))))


(check-expect (numfib 0) 1)
(check-expect (numfib 1) 3)
(check-expect (numfib 2) 4)
(check-expect (numfib 55) 11)


;; want to find the number of fibonacci numbers
;; less than or equal to n
(define (numfib n)
  (generate
   ;; init
   (list 0 1 0) ;; cur-fib next-fib count
   ;; done?
   (λ(lst) (> (first lst) n))
   ;; step
   (λ(lst) (list
                (second lst) ;; cur-fib becomes next-fib
                (+ (first lst) (second lst)) ;; next-fib is cur + next
                (add1 (third lst)) ;; increase count
                ))
   ;; final
   (λ(lst) (third lst))))



;; want to make a list of fibonacci numbers
;; less than or equal to n
(define (listfib n)
  (generate
   ;; init
   (list 1 0) ;; cur-fib next-fib count
   ;; done?
   (λ(lst) (> (first lst) n))
   ;; step
   (λ(lst) (cons (+ (first lst) (second lst)) lst) ;; cur-fib becomes next-fib
                 ;; next-fib is cur + next
                )
   ;; final
   (λ(lst) (reverse (rest lst)))))


(define (nthfib n s)
  (cond
    [(zero? n





  
