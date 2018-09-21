#lang racket

(require "exercise-2.30.rkt"
         rackunit
         rackunit/text-ui)

(define x (square-tree
           (list 1 (list 2 (list 3 4) 5) (list 6 7))))
        
(define exercise-tests
  (test-suite
   "Tests for exercise-2.30.rkt"

   (check-equal? x '(1 (4 (9 16) 25) (36 49)))))
  
(run-tests exercise-tests)
