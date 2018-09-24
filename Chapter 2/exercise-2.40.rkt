#lang racket

(define (range low high)
  (if (> low high)
      '()
      (cons low 
            (range 
             (+ low 1) 
             high))))

(define (flatmap proc seq)
  (foldr append '() (map proc seq)))

;Exercise 2.40: Define a procedure unique-pairs that, given an integer n, generates the sequence of pairs (i,j) with 1≤j<i≤n. Use unique-pairs to simplify the definition of prime-sum-pairs given above.


(define (unique-pairs n)
 (flatmap (lambda (i)
            (map (lambda (j)
                   (if (< j i)
                       (list i j)
                       '()))
                 (range 1 (- i 1))))
          (range 1 n)))

(provide unique-pairs)
