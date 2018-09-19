#lang racket

(require "exercise-2.5.rkt"
         rackunit
         rackunit/text-ui)

(define exercise-tests
  (test-suite
   "Tests for exercise-2.5.rkt"

   (test-case
    "Car"
    (check-equal? (car (cons 1 2)) 1)
    (check-equal? (car (cons 4 9)) 4))
    

   (test-case
    "Cdr"
    (check-equal? (cdr (cons 1 2)) 2)
    (check-equal? (cdr (cons 4 9)) 9))))

(run-tests exercise-tests)
