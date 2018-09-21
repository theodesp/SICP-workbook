#lang racket

(require "exercise-2.28.rkt"
         rackunit
         rackunit/text-ui)
        
(define exercise-tests
  (test-suite
   "Tests for exercise-2.28.rkt"

   (check-equal? (list (list 1 2) (list 3 4)) '((4 3) (2 1)))
   (check-equal? (list (list 1 3 4 (list 12 2)) (list 3 4)) '((1 3 4 (12 2)) (3 4)))))
  