#lang racket
(require scheme/base)

; Simple var defines
(define size 10)

; Simple function definition
(define (square x ) (* x x))

; Composition of functions 
(define (sum-of-squares x y)
  (+ (square x) (square y)))

; Conditional expressions
(define (abs x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))

; Conditional expressions 2nd way
(define (abs2 x)
  (cond ((< x 0) (- x))
        (else x)))

; Conditional expressions 3nd way
(define (abs3 x)
  (if (< x 0)
      (- x)
      x))

; Comparisons
(define (>= x y) 
  (or (> x y) (= x y)))

; Exercise 1.1: Below is a sequence of expressions.
; What is the result printed by the interpreter in
; response to each expression?
; Assume that the sequence is to be evaluated in
; the order in which it is presented

10 ; 10
(+ 5 3 4) ; 12
(- 9 1) ; 8
(/ 6 2) ; 3
(+ (* 2 4) (- 4 6)) ; 6
(define a 3)
(define b (+ a 1)) ; b = 4
(+ a b (* a b)) ; 19
(= a b) ; #f
(if (and (> b a) (< b (* a b)))
    b
    a) ; 4
(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a)) ; 16
      (else 25))
(+ 2 (if (> b a) b a)) ; 6 as b > a
(* (cond ((> a b) a)
         ((< a b) b) ; 4 * 4 = 16
         (else -1))
   (+ a 1))

; Exercise 1.3: Define a procedure that takes three numbers as
; arguments and returns the sum of the squares of the two larger numbers.

(define (squareMax2 a b c)
  (cond ((> a b)
        (cond ((> b c) (+ (* b b) (* c c)))
              (else (+ (* a a) (* c c)))))
        (else (+ (* b b) (* c c)))
        ))

; Exercise 1.4: Observe that our model of evaluation allows for
; combinations whose operators are compound expressions.
; Use this observation to describe the behavior of the following
; procedure:


(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))
; (a-plust-abs-b 4 -2)
; (> b 0) will return -
; (- a b) will do a -(-b) = a + b = 6

; Exercise 1.5: Ben Bitdiddle has invented a test to determine
; whether the interpreter he is faced
; with is using applicative-order evaluation or normal-order evaluation.

(define (p) (p))

(define (test x y) 
  (if (= x 0) 
      0 
      y))

; Then he evaluates the expression

(test 1 (p))

; What behavior will Ben observe with an interpreter that uses
; applicative-order evaluation? What behavior
; will he observe with an interpreter that uses normal-order evaluation?

; Normal order is first expand and the reduce
; So (test 0 (p)) will become
; (if (= 0 0)
;      0
;      y))

;then
; (if (= 0 0)
;      0
;      (p))

;then
; (if (= 0 0)
;      0
;      ((p))

; and it will not stop

; If it was applicative order it would go
; (test 0 (p))
; then
; ; (test 0 ((p)))

; and it will not stop