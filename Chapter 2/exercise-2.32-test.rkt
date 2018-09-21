#lang racket

(require "exercise-2.32.rkt"
         rackunit
         rackunit/text-ui)
        
(define exercise-tests
  (test-suite
   "Tests for exercise-2.32.rkt"

   (check-equal? (subsets '(1 2 3)) '(() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3)))))

(run-tests exercise-tests)


