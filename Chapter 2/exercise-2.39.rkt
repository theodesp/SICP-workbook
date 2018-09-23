#lang racket

;Exercise 2.39: Complete the following definitions of reverse (Exercise 2.18) in terms of fold-right and fold-left from Exercise 2.38:
;
;(define (reverse sequence)
;  (fold-right 
;   (lambda (x y) ⟨??⟩) nil sequence))
;
;(define (reverse sequence)
;  (fold-left 
;   (lambda (x y) ⟨??⟩) nil sequence))

(define (reverse sequence)
  (foldr 
   (lambda (x y) (append y (list x))) '() sequence))

(define (reverse-l sequence)
  (foldl 
   (lambda (x y) (cons x y)) '() sequence))

(provide reverse reverse-l)
