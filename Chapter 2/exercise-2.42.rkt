#lang racket

;We implement this solution as a procedure queens, which returns a sequence of all solutions to the problem of placing n queens on an n×n chessboard. Queens has an internal procedure queen-cols that returns the sequence of all ways to place queens in the first k columns of the board.

;In this procedure rest-of-queens is a way to place k−1 queens in the first k−1 columns, and new-row is a proposed row in which to place the queen for the kth column. Complete the program by implementing the representation for sets of board positions, including the procedure adjoin-position, which adjoins a new row-column position to a set of positions, and empty-board, which represents an empty set of positions. You must also write the procedure safe?, which determines for a set of positions, whether the queen in the kth column is safe with respect to the others. (Note that we need only check whether the new queen is safe—the other queens are already guaranteed safe with respect to each other.)

(define (flatmap proc seq)
  (foldr append '() (map proc seq)))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low 
            (enumerate-interval 
             (+ low 1) 
             high))))

(define q2 '(((1 2) (1 1)) ((2 2) (1 1)) ((1 2) (2 1)) ((2 2) (2 1))))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) 
           (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position 
                    new-row 
                    k 
                    rest-of-queens))
                 (enumerate-interval 
                  1 
                  board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(define empty-board '())

(define (safe? k positions)
  (cond ((null? positions) #t)
        (else (safe-helper k positions))))

(define (safe-helper k positions)
  (if (null? positions) #t
  (or (for/and ([q (enumerate-interval 1 k)]) (safe-pos? (list k q) (car positions)))
       (safe-helper k (cdr positions)))))
     
  
(define (safe-pos? q1 q2)
  (if (not (or (= (car q1) (car q2))
          (= (cadr q1) (cadr q2))
          (= (abs (- (car q1) (car q2))) (abs (- (cadr q1) (cadr q2))))))
      #t
      #f))

(define (adjoin-position new-row l rest)
  (cons (list new-row l) rest))