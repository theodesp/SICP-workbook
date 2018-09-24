#lang racket

(require "exercise-2.41.rkt"
         rackunit
         rackunit/text-ui)
        
(define exercise-tests
  (test-suite
   "Tests for exercise-2.41.rkt"

   (check-equal? (sum-triplets 5 8) '((4 3 1 8) (5 2 1 8)))))
  
(run-tests exercise-tests)
