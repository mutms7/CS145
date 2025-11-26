#lang lazy

    ((λ (f)
    ((λ (self) (f (self self)))
     (λ (self) (f (self self)))))
   (λ (rec)
     (λ (x)
       ((((λ(lst) (lst (λ (yes) (λ (no) (λ (yes) (λ (no) no)))))) x)
         ((λ(first rest) (λ (selector) ((selector first) rest))) (λ (yes) (λ (no) yes)) (λ (x) (λ (yes) (λ (no) yes)))))
        ((((λ(pair) (pair (λ (yes) (λ (no) yes)))) x)
          ((λ(first rest) (λ (selector) ((selector first) rest))) (λ (yes) (λ (no) no)) (rec ((λ(pair) (pair (λ (yes) (λ (no) no)))) x))))
         ((λ(first rest) (λ (selector) ((selector first) rest))) (λ (yes) (λ (no) yes)) ((λ(pair) (pair (λ (yes) (λ (no) no)))) x)))))))