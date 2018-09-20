#lang racket

(define (make-interval a b) (cons a b))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) 
                    (lower-bound y))
                 (+ (upper-bound x) 
                    (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) 
               (lower-bound y)))
        (p2 (* (lower-bound x) 
               (upper-bound y)))
        (p3 (* (upper-bound x) 
               (lower-bound y)))
        (p4 (* (upper-bound x) 
               (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (when (= (upper-bound x) 0)
    (error "div-interval: upper-bound is 0"))
  (when (= (lower-bound y) 0)
    (error "div-interval: upper-bound is 0"))
  (mul-interval x 
                (make-interval 
                 (/ 1.0 (upper-bound y)) 
                 (/ 1.0 (lower-bound y)))))

;Exercise 2.7: Alyssa’s program is incomplete because she has not specified the implementation of the interval abstraction. Here is a definition of the interval constructor:
;
;(define (make-interval a b) (cons a b))
;Define selectors upper-bound and lower-bound to complete the implementation.

(define (lower-bound a)
  (car a))

(define (upper-bound a)
  (cdr a))

;Exercise 2.8: Using reasoning analogous to Alyssa’s, describe how the difference of two intervals may be computed. Define a corresponding subtraction procedure, called sub-interval.

(define (sub-interval x y)
  (make-interval (- (lower-bound x) 
                    (lower-bound y))
                 (- (upper-bound x) 
                    (upper-bound y))))

;Exercise 2.9: The width of an interval is half of the difference between its upper and lower bounds. The width is a measure of the uncertainty of the number specified by the interval. For some arithmetic operations the width of the result of combining two intervals is a function only of the widths of the argument intervals, whereas for others the width of the combination is not a function of the widths of the argument intervals. Show that the width of the sum (or difference) of two intervals is a function only of the widths of the intervals being added (or subtracted). Give examples to show that this is not true for multiplication or division.

(define (width x) (/ (+ (lower-bound ) (upper-bound x)) 2))

;Exercise 2.10: Ben Bitdiddle, an expert systems programmer, looks over Alyssa’s shoulder and comments that it is not clear what it means to divide by an interval that spans zero. Modify Alyssa’s code to check for this condition and to signal an error if it occurs.

(define (div-interval-safe x y)
  (unless (= (upper-bound x) 0)
    (error "div-interval: upper-bound is 0"))
  (unless (= (lower-bound y) 0)
    (error "div-interval: upper-bound is 0"))
  (mul-interval x 
                (make-interval 
                 (/ 1.0 (upper-bound y)) 
                 (/ 1.0 (lower-bound y)))))

;Exercise 2.11: In passing, Ben also cryptically comments: “By testing the signs of the endpoints of the intervals, it is possible to break mul-interval into nine cases, only one of which requires more than two multiplications.” Rewrite this procedure using Ben’s suggestion.

(define (mul-interval-fast x y)
  (cond ((<= (upper-bound x) 0)
         (cond ((<= (upper-bound y) 0)
                (make-interval (* (upper-bound x)
                                  (upper-bound y))
                               (* (lower-bound x)
                                  (lower-bound y))))
               ((>= (lower-bound y) 0)
                (make-interval (* (lower-bound x)
                                  (upper-bound y))
                               (* (upper-bound x)
                                  (lower-bound y))))
               (else (make-interval (* (lower-bound x)
                                       (upper-bound y))
                                    (* (lower-bound x)
                                       (lower-bound y))))))
        ((>= (lower-bound x) 0)
         (cond ((<= (upper-bound y) 0)
                (make-interval (* (upper-bound x)
                                  (lower-bound y))
                               (* (lower-bound x)
                                  (upper-bound y))))
               ((>= (lower-bound y) 0)
                (make-interval (* (lower-bound x)
                                  (lower-bound y))
                               (* (upper-bound x)
                                  (upper-bound y))))
               (else (make-interval (* (upper-bound x)
                                       (lower-bound y))
                                    (* (upper-bound x)
                                       (upper-bound y))))))
        (else
         (let ((p1 (* (upper-bound x)
                      (upper-bound y)))
               (p2 (* (lower-bound x)
                      (lower-bound y)))
               (p3 (* (upper-bound x)
                      (lower-bound y)))
               (p4 (* (lower-bound x)
                      (upper-bound y))))
           (make-interval (min (p3 p4)) (max (p1 p2)))))))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) 
        (upper-bound i)) 
     2))

(define (width-alt i)
  (/ (- (upper-bound i) 
        (lower-bound i)) 
     2))

;Exercise 2.12: Define a constructor make-center-percent that takes a center and a percentage tolerance and produces the desired interval. You must also define a selector percent that produces the percentage tolerance for a given interval. The center selector is the same as the one shown above.

(define (make-center-percent c p)
  (make-interval (- c (* c p)) (+ c (* c p))))

(define (percent i)
  (- 1 (/ (lower-bound i) (center i))))


(define (par1 r1 r2)
  (div-interval 
   (mul-interval r1 r2)
   (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval 
     one
     (add-interval 
      (div-interval one r1) 
      (div-interval one r2)))))

;Exercise 2.14: Demonstrate that Lem is right. Investigate the behavior of the system on a variety of arithmetic expressions. Make some intervals A and B, and use them in computing the expressions A/A and A/B. You will get the most insight by using intervals whose width is a small percentage of the center value. Examine the results of the computation in center-percent form (see Exercise 2.12).

(par2 (make-interval 2 5) (make-interval 3 6)) ;'(1.2000000000000002 . 2.727272727272727)
(par1 (make-interval 2 5) (make-interval 3 6)) ;'(0.5454545454545454 . 6.0)
          