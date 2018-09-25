#lang racket
 (require "exercise-2.62.rkt"
         rackunit
         rackunit/text-ui)
        
(define exercise-tests
  (test-suite
   "Tests for exercise-2.62.rkt"
    (check-equal? (union-set '(2 4 6 8) '(1 3 5 7)) '(1 2 3 4 5 6 7 8))))
  
(run-tests exercise-tests)