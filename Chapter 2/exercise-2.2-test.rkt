#lang racket

(require "exercise-2.2.rkt"
         rackunit
         rackunit/text-ui)

(define exercise-tests
  (test-suite
   "Tests for exercise-2.2.rkt"

   (test-case
    "Test make-segment"
    (check-equal? (make-segment 1 1 2 2) '((1 . 1) 2 . 2)))
    

   (test-case
    "Test mid-segment"
    (check-equal? (mid-segment (make-segment 1 2 3 4)) '(1/2 . 2)))))

(run-tests exercise-tests)
