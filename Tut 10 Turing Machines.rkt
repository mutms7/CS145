#lang lazy

(define True (λ (yes) (λ (no) yes)))
(define False (λ (yes) (λ (no) no)))
(define (True? b) ((b 'yes) 'no))

(define (If test thenpart elsepart)
  ((test thenpart) elsepart))

(define (Or a b) (If a True b))
(define (And a b) (If a b False))
(define (Not a) (If a False True))

(define (Cons first rest)
  (λ (selector) ((selector first) rest)))

(define (First pair) (pair True))
(define (Rest pair) (pair False))

(define Empty (λ (x) True))
(define (Empty? lst)
  (lst (λ (yes) (λ (no) False))))

(define Y
  (λ (f)
    ((λ (self) (f (self self)))
     (λ (self) (f (self self))))))

(define Z Empty)
(define Z? Empty?)

(define (ADD1 x)
   (If (Z? x)
       (Cons True Z)
       (If (First x) (Cons False (ADD1 (Rest x)))
           (Cons True (Rest x)))))

(define (ADD a b)
  (If (Z? a) b
   (If (Z? b) a
    (If (Not (First a)) (Cons (First b) (ADD (Rest a) (Rest b)))
     (If (Not (First b)) (Cons (First a) (ADD (Rest a) (Rest b)))
       (Cons False (ADD1 (ADD (Rest a) (Rest b)))))))))

(define (TAN x)
  (If (Z? x) 0
      (If (First x)
          (add1 (* 2 (TAN (Rest x))))
          (* 2 (TAN (Rest x))))))

(define (NAT n)
  (if (zero? n) Z (ADD1 (NAT (sub1 n)))))

(define (x1 n)
  (define (x2 m)
    (if (< m 0) (void)
     (begin
       (printf "~a ~a ~a\n" n m (TAN (ADD (NAT n) (NAT m))))
       (x2 (sub1 m)))))
  (if (< n 0) (void)
      (begin (x2 n) (x1 (sub1 n)))))



(define make_state (λ (t f h) (λ (sel) (sel t f h))))
;;(define State-true (λ))


(define List-ref (λ (lst loc) (If (Z? loc) (First lst) (List-ref (Rest lst) (SUB1 loc)))))
(define Replace (λ (lst val loc) (if (Z? loc) (Cons val (Rest lst)) (Cons (First lst) (Replace (Rest lst) val (SUB1 loc))))))


(define run (λ (states mem) (run-help states mem state-loc mem-loc)))

(define run-help (λ (run-help states mem state-loc mem-loc)
                   ((λ (curr-state curr-mem)
                      (If (State-halt curr-state) mem
                          (If curr-mem (run-help
                                        states
                                        ;; 
                                        (Replace mem (First (Rest (State-treu curr-state))) mem-loc)
                                        ;; third of list getting next state
                                        (First (Rest (Rest (State-true curr-state))))
                                        (If (First (State-true curr-state)) (ADD1 mem-loc) (SUB1 mem-loc)))))
                      ) (List-ref states state-loc) (List-ref mem mem-loc))))

(Define states (Cons (make-State (Cons True (Cons True (Cons Z Empty))) (Cons False (Cons True (Cons ADD1 Z) Empty)) False)
                     (Cons (make-State Empty Empty True) Empty)))

(Display (run states mem))


