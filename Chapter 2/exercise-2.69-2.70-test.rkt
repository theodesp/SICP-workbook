#lang racket

(require "exercise-2.69-2.70.rkt"
         rackunit
         rackunit/text-ui)

(define s '((A 2) (NA  16) (BOOM 1) (GET  2) (SHA  3) (YIP  9) (WAH  1) (JOB  2)))
(define ht (generate-huffman-tree s))
        
(define exercise-tests
  (test-suite
   "Tests for exercise-2.69-2.70.rkt"

   (check-equal? (encode '(GET A JOB) ht) '(1 1 1 1 0 1 1 1 0 1 1 1 1 1 0))
   (check-equal? (encode '(SHA BOOM) ht) '(1 1 0 1 1 1 1 1 1 1))))
  
(run-tests exercise-tests)
