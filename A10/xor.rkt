#lang lazy
(λ (one two)
  ;; and
((λ (test thenpart)
    ((test thenpart) (λ (no) (λ (yes) yes))))
  ;;or
  ((λ (a1 a2)
    ((a1 (λ (yes) (λ (no) yes))) a2)) one two)

  ;;not
  ((λ (b1)
    ((b1 (λ (no) (λ (yes) yes))) (λ (yes) (λ (no) yes))))
  ((λ (c1 c2)
    ((c1 c2) (λ (no) (λ (yes) yes)))) one two))))

