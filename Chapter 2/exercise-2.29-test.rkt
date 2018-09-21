#lang racket

(require "exercise-2.29.rkt"
         rackunit
         rackunit/text-ui)
        
(define exercise-tests
  (test-suite
   "Tests for exercise-2.29.rkt"

   (check-equal? (total-weight '((1 ()) (5 ()))) 6)
   (check-equal? (total-weight '((4 ((4 ()) (4 ()))) (5 ()))) 17)))
  
(run-tests exercise-tests)
