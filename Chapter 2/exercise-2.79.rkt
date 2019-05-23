#lang racket

;Exercise 2.79: Define a generic equality predicate equ? that tests the equality of two numbers, and install it in the generic arithmetic package. This operation should work for ordinary numbers, rational numbers, and complex numbers.

(define (equ? x y) (apply-generic 'equ? x y))

(put 'equ? '(scheme-number scheme-number)
       (lambda (x y) (tag (= x y))))

(put 'equ? '(complex-number complex-number)
       (lambda (x y) (tag (and
                           (= (real-part x) (real-part y))
                           (= (img-part x) (img-part y))))))

