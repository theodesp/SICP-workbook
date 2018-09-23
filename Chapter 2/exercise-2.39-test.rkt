#lang racket

(require "exercise-2.39.rkt"
         rackunit
         rackunit/text-ui)


(define exercise-tests
  (test-suite
   "Tests for exercise-2.39.rkt"

   (test-case
    "Test reverse"
    (check-equal? (reverse '(1 2 3)) '(3 2 1)))

   (test-case
    "Test reverse-l"
    (check-equal? (reverse-l '(1 2 3)) '(3 2 1)))))
    
(run-tests exercise-tests)
