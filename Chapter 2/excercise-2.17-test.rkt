#lang racket

(require "excercise-2.17.rkt"
         rackunit
         rackunit/text-ui)
        
(define exercise-tests
  (test-suite
   "Tests for exercise-2.17.rkt"

   (check-equal? (last-pair (list 23 72 149 34)) '(34))
   (check-equal? (last-pair (list 1)) '(1))))
  
(run-tests exercise-tests)
