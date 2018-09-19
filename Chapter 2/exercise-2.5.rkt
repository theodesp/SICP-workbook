#lang racket

;Exercise 2.5: Show that we can represent pairs of nonnegative integers using only numbers and arithmetic operations if we represent the pair a and b as the integer that is the product 2a3b. Give the corresponding definitions of the procedures cons, car, and cdr.

(define (cons a b)
  (lambda (m) (m a b (* (expt 2 a) (expt 3 b)))))

(define (car z)
  (z (lambda (p q res) (floor (logarithm 2 (/ res (expt 3 q)))))))

(define (cdr z)
  (z (lambda (p q res) (floor (logarithm 3 (/ res (expt 2 p)))))))

(define (logarithm a base)
  (/ (log base) (log a)))
