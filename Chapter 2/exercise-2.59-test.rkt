#lang racket

(require "exercise-2.59.rkt"
         rackunit
         rackunit/text-ui)
        
(define exercise-tests
  (test-suite
   "Tests for exercise-2.59.rkt"

   (check-equal? (union-set '(1 2 3) '(2 3 4)) '(1 2 3 4))))
  
(run-tests exercise-tests)
