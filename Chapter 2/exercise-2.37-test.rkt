#lang racket

(require "exercise-2.37.rkt"
         rackunit
         rackunit/text-ui)

(define s '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))

(define exercise-tests
  (test-suite
   "Tests for exercise-2.37.rkt"

   (test-case
    "Test matrix-*-vector"
    (check-equal? (matrix-*-vector s '(4 2  3)) '(17 44 71 98)))

   (test-case
    "Test matrix-*-matrix"
    (check-equal? (matrix-*-matrix (cdr s) (cdr s)) '((111 126 141) (174 198 222) (237 270 303))))

   (test-case
    "Test transpose"
    (check-equal? (transpose s) '((1 4 7 10) (2 5 8 11) (3 6 9 12))))))
    
(run-tests exercise-tests)
