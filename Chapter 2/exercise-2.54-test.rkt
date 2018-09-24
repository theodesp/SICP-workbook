#lang racket

(require "exercise-2.54.rkt"
         rackunit
         rackunit/text-ui)
        
(define exercise-tests
  (test-suite
   "Tests for exercise-2.54.rkt"

   (check-equal? (equal? '(this is a list) '(this (is a) list)) #f)))
  
(run-tests exercise-tests)
