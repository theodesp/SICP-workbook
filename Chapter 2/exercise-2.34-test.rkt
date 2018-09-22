#lang racket

(require "exercise-2.34.rkt"
         rackunit
         rackunit/text-ui)
        
(define exercise-tests
  (test-suite
   "Tests for exercise-2.34.rkt"

   (check-equal? (horner-eval 2 (list 1 3 0 5 0 1)) 20)))
  
(run-tests exercise-tests)
