#lang racket

;Exercise 2.41: Write a procedure to find all ordered triples of distinct positive integers i, j, and k less than or equal to a given integer n that sum to a given integer s.

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low 
            (enumerate-interval 
             (+ low 1) 
             high))))

(define (flatmap proc seq)
  (foldr append '() (map proc seq)))

(define (equal-sum? triplet s)
  (= (+ (car triplet) (cadr triplet) (caddr triplet)) s))

(define (make-triplet-sum triplet)
  (list (car triplet) 
        (cadr triplet)
        (caddr triplet)
        (+ (car triplet) (cadr triplet) (caddr triplet))))

(define (sum-triplets n s)
  (map make-triplet-sum
       (filter
        (lambda (x)
          (if (equal-sum? x s) #t #f))
        (flatmap
         (lambda (i)
           (flatmap
             (lambda (j)
                   (map (lambda (k)
                         (list i j k))
                        (enumerate-interval 1 (- j 1))))
                  (enumerate-interval 1 (- i 1))))
         (enumerate-interval 1 n)))))

(provide sum-triplets)
