;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname A2ahelp) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
  (define (m n) (if (= n 0) 1 (* n (m (quotient n 2)))))
(m 100) ;; 3 8 13131818
      ;; 0 1 2 3 4 5

