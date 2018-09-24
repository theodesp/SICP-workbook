#lang racket

(define seq
  (cons 1
      (cons 2
            (cons 3
                  (cons 4 'nil)))))


(define one-through-four (list 1 2 3 4))

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) 
                (- n 1))))

(define squares 
  (list 1 4 9 16 25))

(list-ref squares 3)

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

(define odds
  (list 1 3 5 7))

(length odds)

(define (length-it items)
  (define (length-iter a count)
    (if (null? a)
        count
        (length-iter (cdr a) 
                     (+ 1 count))))
  (length-iter items 0))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) 
            (append (cdr list1) 
                    list2))))

(define (scale-list items factor)
  (if (null? items)
      'nil
      (cons (* (car items) factor)
            (scale-list (cdr items) 
                        factor))))

(define (map proc items)
  (if (null? items)
      'nil
      (cons (proc (car items))
            (map proc (cdr items)))))

(define x (cons (list 1 2) (list 3 4)))

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

(count-leaves x)


(define (scale-tree tree factor)
  (cond ((null? tree) 'nil)
        ((not (pair? tree)) 
         (* tree factor))
        (else
         (cons (scale-tree (car tree) 
                           factor)
               (scale-tree (cdr tree) 
                           factor)))))

(scale-tree (list 1 
                  (list 2 (list 3 4) 5) 
                  (list 6 7))
            10)


(define (scale-tree-map tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree sub-tree factor)
             (* sub-tree factor)))
       tree))


define (sum-odd-squares tree)
  (cond ((null? tree) 0)
        ((not (pair? tree))
         (if (odd? tree) (square tree) 0))
        (else (+ (sum-odd-squares 
                  (car tree))
                 (sum-odd-squares 
                  (cdr tree))))))

(define (even-fibs n)
  (define (next k)
    (if (> k n)
        nil
        (let ((f (fib k)))
          (if (even? f)
              (cons f (next (+ k 1)))
              (next (+ k 1))))))
  (next 0))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate 
                       (cdr sequence))))
        (else  (filter predicate 
                       (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op 
                      initial 
                      (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low 
            (enumerate-interval 
             (+ low 1) 
             high))))

(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append 
               (enumerate-tree (car tree))
               (enumerate-tree (cdr tree))))))

(define (sum-odd-squares tree)
  (accumulate 
   +
   0
   (map square
        (filter odd?
                (enumerate-tree tree)))))

(define (even-fibs n)
  (accumulate 
   cons
   nil
   (filter even?
           (map fib
                (enumerate-interval 0 n)))))

(define (list-fib-squares n)
  (accumulate 
   cons
   nil
   (map square
        (map fib
             (enumerate-interval 0 n)))))

(define 
  (product-of-squares-of-odd-elements
   sequence)
  (accumulate 
   *
   1
   (map square (filter odd? sequence))))

(define 
  (salary-of-highest-paid-programmer
   records)
  (accumulate 
   max
   0
   (map salary
        (filter programmer? records))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) 
        (cadr pair) 
        (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter 
        prime-sum?
        (flatmap
         (lambda (i)
           (map (lambda (j) 
                  (list i j))
                (enumerate-interval 
                 1 
                 (- i 1))))
         (enumerate-interval 1 n)))))

(define (permutations s)
  (if (null? s)   ; empty set?
      (list nil)  ; sequence containing empty set
      (flatmap (lambda (x)
                 (map (lambda (p) 
                        (cons x p))
                      (permutations 
                       (remove x s))))
               s)))

(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))
