#lang racket

(require "exercise-2.65.rkt"
         rackunit
         rackunit/text-ui)
        
(define exercise-tests
  (test-suite
   "Tests for exercise-2.65.rkt"

   (check-equal? (lookup 9 '(7 (2 () ()) (3 (6 () ()) ()))) #f)
   (check-equal? (lookup 2 '(7 (2 () ()) (3 (6 () ()) ()))) #t)))
  
(run-tests exercise-tests)
