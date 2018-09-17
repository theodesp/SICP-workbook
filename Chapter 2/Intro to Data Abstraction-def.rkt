#lang racket
; Arithmetic Operations for Rational Numbers

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (make-rat n d) (cons n d))
(define (numer x) (car x))
(define (denom x) (cdr x))

; cons makes a pair a b
(define x (cons 1 2))
(define y (cons 3 4))
(define z (cons x y))

; car takes the first element of a pair
(car (car z)) ; 1

; cdr takes the second element of a pair
(car (cdr z)) ; 3

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define one-half (make-rat 1 2))
(print-rat one-half)

(define one-third (make-rat 1 3))
(print-rat
 (add-rat one-half one-third))

; This reduced the rational numbers to lowest terms
(define (make-rat2 n d)
  (let ((g (gcd n d)))
    (cons (/ n g) 
          (/ d g))))

(define (cons x y)
  (lambda (m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else 
           (error "Argument not 0 or 1:
                   CONS" m)))))

  ; We return a function dispatch so that when applied to with a
  ; param we get either the first or the second param

(define (car z) (z 0))
(define (cdr z) (z 1))
