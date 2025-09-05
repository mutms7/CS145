;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname A1b) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define (collinear a x b y c z)
  (and
   (not(or (and (= (- x y) 0) (not (= (- y z) 0))) (and (= (- y z) 0) (not (= (- x y) 0))))) ;; one 0 one real = false
   (or 
    (and (= (- x y) 0) (= (- y z) 0));; both 0 = true
    (= (/ (- a b) (- x y)) (/ (- b c) (- y z)))))) ;; slopes same = true


  (collinear 1 5 2 5 3 3)     ;; should produce true
  (collinear 2 10 6 50 3 20)  ;; should produce true
  (collinear 1 10 5 49 2 20)  ;; should produce false