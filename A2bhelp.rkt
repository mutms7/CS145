;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname A2bhelp) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
 (define (nextfib a b n) (if (= n 0) a (nextfib b (+ a b) (- n 1))))
   (define (fib1 n) (nextfib 0 1 n))
(fib1 100)