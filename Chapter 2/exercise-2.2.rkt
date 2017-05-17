#lang racket

; Exercise 2.2Define a constructor make-segment and selectors
; start-segment and end-segment that define the representation
; of segments in terms of points. Furthermore, a point can be
; represented as a pair of numbers: the xx coordinate and the yy
; coordinate. Accordingly, specify a constructor make-point and
; selectors x-point and y-point that define this representation.
; Finally, using your selectors and constructors, define a procedure
; midpoint-segment that takes a line segment as argument and returns
; its midpoint (the point whose coordinates are the average of the
;                   coordinates of the endpoints).


(define (make-point x y)
  (cons x y)
  )

(define (x-point point)
  (car point)
  )

(define (y-point point)
  (cdr point)
  )

(define (make-segment a1 b1 a2 b2)
  (cons (make-point a1 b1) (make-point a2 b2))
  )

(define (start-segment segment)
  (car segment)
  )

(define (end-segment segment)
  (cdr segment)
  )

(define (mid-segment segment)
  (let ((mid-x (make-point
                (/ (+ (x-point (start-segment segment))) 2)
                (/ (+ (y-point (end-segment segment))) 2)
                )))
    mid-x
  )
)

(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(provide mid-segment)
(provide make-segment)
(provide print-point)