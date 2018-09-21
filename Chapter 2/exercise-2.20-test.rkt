#lang racket

(require "exercise-2.20.rkt"
         rackunit
         rackunit/text-ui)
        
(define exercise-tests
  (test-suite
   "Tests for exercise-2.20.rkt"

   (check-equal? (same-parity 2 3 4 5 6 7) '(2 4 6))
   (check-equal? (same-parity 1 2 3 4 5 6 7) '(1 3 5 7))))
  
(run-tests exercise-tests)
