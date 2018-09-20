#lang racket

(require "exercise-2.18.rkt"
         rackunit
         rackunit/text-ui)
        
(define exercise-tests
  (test-suite
   "Tests for exercise-2.18.rkt"

   (check-equal? (reverse (list 1 4 9 16 25)) '(25 16 9 4 1))
   (check-equal? (reverse (list 1)) '(1))))
  
(run-tests exercise-tests)
