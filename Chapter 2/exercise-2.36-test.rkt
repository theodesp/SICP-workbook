#lang racket

(require "exercise-2.36.rkt"
         rackunit
         rackunit/text-ui)

(define s '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))

(define exercise-tests
  (test-suite
   "Tests for exercise-2.36.rkt"

   (test-case
    "Test accumulate-n"
    (check-equal? (accumulate-n + 0 s) '(22 26 30)))))
    
(run-tests exercise-tests)
