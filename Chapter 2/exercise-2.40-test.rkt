#lang racket

(require "exercise-2.40.rkt"
         rackunit
         rackunit/text-ui)
        
(define exercise-tests
  (test-suite
   "Tests for exercise-2.40.rkt"

   (check-equal? (unique-pairs 3) '((2 1) (3 1) (3 2)))))
  
(run-tests exercise-tests)