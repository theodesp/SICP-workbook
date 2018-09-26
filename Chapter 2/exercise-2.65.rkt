#lang racket
(require racket/trace)
;
;Exercise 2.66: Implement the lookup procedure for the case where the set of records is structured as a binary tree, ordered by the numerical values of the keys.

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (lookup given-key tree)
   (let ((left (left-branch tree))
        (right (right-branch tree))
        (empty? (null? tree)))
  (cond (empty? false)
        ((and (null? left) (null? right)) #f)
        ((null? left) (lookup given-key right))
        ((null? right) (lookup given-key left))
        ((equal? given-key 
                 (entry left)) #t)
        ((equal? given-key 
                 (entry right)) #t)
        (else
         (or (lookup given-key 
                 left)
              (lookup given-key 
                 right))))))


(provide lookup)
