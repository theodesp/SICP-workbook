#lang racket

(require "exercise-2.61.rkt"
         rackunit
         rackunit/text-ui)
        
(define exercise-tests
  (test-suite
   "Tests for exercise-2.61.rkt"

   (check-equal? (adjoin-set 4 '(1 2 3 6)) '(1 2 3 4 6))))
  
(run-tests exercise-tests)
