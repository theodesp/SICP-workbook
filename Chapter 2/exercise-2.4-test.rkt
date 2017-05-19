#lang racket

(require "exercise-2.4.rkt"
         rackunit
         rackunit/text-ui 
         )


(define exercise-tests
  (test-suite
   "Tests for exercise-2.4.rkt"

   (check-equal? (cdr (cons 5 4)) 4 "Check cdr")
   (check-equal? (car (cons -5 -4)) -5 "Check car")

   )
  )

(run-tests exercise-tests)
