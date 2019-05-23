#lang racket

;Exercise 2.80: Define a generic predicate =zero? that tests if its argument is zero, and install it in the generic arithmetic package. This operation should work for ordinary numbers, rational numbers, and complex numbers.


(define (=zero? x) (apply-generic '=zero? x))

(put '=zero? '(scheme-number)
       (lambda (x) (tag (= x 0))))

(put '=zero? '(complex-number)
       (lambda (x) (tag (and
                           (= (real-part x) 0)
                           (= (img-part x) 0)))))