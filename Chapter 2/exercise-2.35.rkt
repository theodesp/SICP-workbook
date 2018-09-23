#lang racket

;Exercise 2.35: Redefine count-leaves from 2.2.2 as an accumulation:
;
;(define (count-leaves t)
;  (accumulate ⟨??⟩ ⟨??⟩ (map ⟨??⟩ ⟨??⟩)))

(define (enumerate-tree tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (list tree))
        (else (append 
               (enumerate-tree (car tree))
               (enumerate-tree (cdr tree))))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op 
                      initial 
                      (cdr sequence)))))

(define (count-leaves-helper x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

(define (count-leaves t)
  (accumulate (lambda (x y)
                (+ (length x) y)) 0
              (map enumerate-tree t)))

(count-leaves '((1 2) 3 4))

