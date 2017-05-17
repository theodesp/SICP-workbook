#lang racket

(require "exercise-2.1.rkt"
         rackunit
         rackunit/text-ui 
         )


(define exercise-tests
  (test-suite
   "Tests for exercise-2.1.rkt"

   (check-equal? (make-rat 5 4) '(5 . 4) "Positive numbers")
   (check-equal? (make-rat -5 -4) '(5 . 4) "Negative numbers")
   (check-equal? (make-rat 5 -4) '(-5 . 4) "Denom negative")
   (check-equal? (make-rat -5 4) '(-5 . 4) "Nominator negative")
   
   )
  )

(run-tests exercise-tests)
