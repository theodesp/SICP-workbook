#lang racket

(require "exercise-2.58.rkt"
         rackunit
         rackunit/text-ui)
        
(define exercise-tests
  (test-suite
   "Tests for exercise-2.58.rkt"

   (check-equal? (deriv '(x * y) 'x) 'y)))
  
(run-tests exercise-tests)