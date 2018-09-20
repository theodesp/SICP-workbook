#lang racket

(require "exercise-2.6.rkt"
         rackunit
         rackunit/text-ui)

(define exercise-tests
  (test-suite
   "Tests for exercise-2.6.rkt"

   (test-case
    "Zero"
    (check-equal? ((zero add1) 0) 0)
    (check-equal? ((zero add1) 1) 1))

   (test-case
    "One"
    (check-equal? ((one add1) 0) 1)
    (check-equal? ((one add1) 1) 2))

   (test-case
    "Two"
    (check-equal? ((two add1) 0) 2)
    (check-equal? ((two add1) 1) 3))

   ))
    

(run-tests exercise-tests)
