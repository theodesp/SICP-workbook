#lang racket

;Exercise 2.62: Give a Θ(n) implementation of union-set for sets represented as ordered lists.

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (cond ((element-of-set? x set) set)
        ((< x (car set)) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))

(define (union-set set1 set2)
   (cond ((null? set1) set2)
         ((null? set2) set1)
         (else (let ((x1 (car set1)) (x2 (car set2)))
           (cond ((= x1 x2)
                  (cons x1 (union-set 
                         (cdr set1)
                         (cdr set2))))
                 ((< x1 x2) (append (list x1 x2) (union-set 
                          (cdr set1) 
                          (cdr set2))))
                 ((< x2 x1) (append (list x2 x1) (union-set 
                          (cdr set1)  
                          (cdr set2)))))))))
 (provide union-set)
