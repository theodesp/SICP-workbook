#lang racket

(define (cube x) (* x x x))
(define (square x) (* x x))

; Calculate Sum n from a to b
(define (sum-integers a b)
  (if (> a b)
      0
      (+ a (sum-integers (+ a 1) b))))

; Calculate Sum n^3 from a to b
(define (sum-cubes a b)
  (if (> a b) 
      0 
      (+ (cube a) 
         (sum-cubes (+ a 1) b))))

; Calculate Sum 1/1*3 + 1/5*7 + ... from a to b
(define (pi-sum a b)
  (if (> a b)
      0
      (+ (/ 1.0 (* a (+ a 2))) 
         (pi-sum (+ a 4) b))))

; or more general
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (inc n) (+ n 1))

; So
(define (sum-cubes2 a b)
  (sum cube a inc b))

(sum-cubes2 1 10)

(define (identity x) x)

(define (sum-integers2 a b)
  (sum identity a inc b))

(sum-integers2 1 10)

(define (pi-sum2 a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  
  (define (pi-next x)
    (+ x 4))
  
  (sum pi-term a pi-next b))

(* 8 (pi-sum2 1 1000))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  
  (* (sum f (+ a (/ dx 2.0)) add-dx b) dx)
)

(integral cube 0 1 0.01)

; Exercise 1.29: Simpson’s Rule is a more accurate
; method of numerical integration than the method illustrated above.
; Define a procedure that takes as arguments ff, aa, bb, and nn and
; returns the value of the integral, computed using Simpson’s Rule.
; Use your procedure to integrate cube between 0 and 1
; (with n=100n=100 and n=1000n=1000), and compare the results
; to those of the integral procedure shown above.

(define (even? n) (= (remainder n 2) 0))

(define (better-integral term a b n)
  (simpson term a b n))

(define (simpson term a b n)
  ; Define h
  (define (h) (/ (- b a ) n))
  
  ; Define f(k)
  (define (f_k k) (term (+ a (* k (h)))))
  
  ; Calculate cf(k)
  (define (coeff n) (* (if (even? n) 2.0 4.0) (f_k n)))
  
  ; Next is just n + 1
  (define (next n) (+ n 1))

  ; Calculate simpson rule
  (* (/ (h) 3.0) (+ (f_k 0.0) (sum coeff 0.0 next (- n 1.0)) (f_k n)))
 )

(better-integral cube 0 1 100)

; Exercise 1.30: The sum procedure above generates a linear recursion.
; The procedure can be rewritten so that the sum is performed
; iteratively. Show how to do this by filling in the missing
; expressions in the following definition:


(define (sum2 term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result a))))
  
  (iter (term a) 0))

(define (sum-integers3 a b)
  (sum2 identity a inc b))

(sum-integers3 1 10) ; 55

; Exercise 1.31:The sum procedure is only the simplest of a vast number of
; similar abstractions that can be captured as higher-order procedures.
;  Write an analogous procedure called product
; that returns the product of the values of a function at points over
; a given range. Show how to define factorial in terms of product.
; Also use product to compute approximations to ππ using the formula:
; π/4 = 2*4*4*6*6*8.../3*3*5*5*7*7...


(define (product term a next b)
  (if (> a b) 1
      (* (term a) (product term (next a) next b))
  )
 )

(define (fact n)
  (product identity 1 inc n)
)

(fact 5) ;24

; Wallis product https://en.wikipedia.org/wiki/Wallis_product
(define (pi limit)
  (define (f_x x)
    (* (/ (* 2 x) (- (* x 2) 1))
       (/ (* 2 x) (+ (* x 2) 1))))
  
  (* (product f_x 1 inc limit) 2))

(pi 50)

; If your product procedure generates a recursive process, write one that
; generates an iterative process. If it generates an iterative process,
; write one that generates a recursive process.

(define (product2 term a next b)
  (define (product-iter a result)
    (if (> a b)
        result
        (product-iter (next a) (* result a)))
    )
  (product-iter (term a) 1))

(define (fact2 n)
  (product2 identity 1 inc n))

(fact2 5)

; Exercise 1.32:
; Show that sum and product (Exercise 1.31) are both special cases of a
; still more general notion called accumulate that combines a collection
; of terms, using some general accumulation function:
; (accumulate 
;  combiner null-value term a next b)


(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) (accumulate combiner null-value term (next a) next b))))

(define (mul a b)
  (* a b))

(define (fact3 n)
  (accumulate mul 1 identity 1 inc n))

(fact3 5) ; 120

; If your accumulate procedure generates a recursive process,
; write one that generates an iterative process.
; If it generates an iterative process, write one that generates a recursive process.

(define (accumulate2 combiner null-value term a next b)
 (define (accumulate-iter a result)
    (if (> a b)
        result
        (accumulate-iter (next a) (combiner result a))))
  
  (accumulate-iter (term a) null-value))

(define (fact4 n)
  (accumulate2 mul 1 identity 1 inc n))

; Exercise 1.33: You can obtain an even more general version of accumulate
; (Exercise 1.32) by introducing the notion of a filter on the terms to be combined.
; That is, combine only those terms derived from values in the range that satisfy a
; specified condition. The resulting filtered-accumulate abstraction takes the same
; arguments as accumulate, together with an additional predicate of one argument that
; specifies the filter. Write filtered-accumulate as a procedure.
; Show how to express the following using filtered-accumulate:
; 
; 1) the sum of the squares of the prime numbers in the interval aa to bb
; (assuming that you have a prime? predicate already written)
; 2) the product of all the positive integers less than nn that are
; relatively prime to nn
; (i.e., all positive integers i<ni<n such that GCD(i,n)=1GCD(i,n)=1).

(define (filtered-accumulate
         predicate combiner null-value term a next b)
  (cond ((> a b) null-value)
        ((predicate a) (combiner (term a) (filtered-accumulate predicate combiner null-value term (next a) next b)))
        (else (filtered-accumulate predicate combiner null-value term (next a) next b))))
(define (divides? a b)
  (= (remainder b a) 0))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  ; Return the number if the square of test is bigger
  (cond ((> (square test-divisor) n)
         n)
        ; Test if it test devides n
        ((divides? test-divisor n) 
         test-divisor)
        ; Otherwise inc test and run the tests again
        (else (find-divisor 
               n 
               (+ test-divisor 1)))))

; A number is prime iff the smallest divisor is it self or 1
(define (prime? n)
  (= n (smallest-divisor n)))

(define (add a b)
  (+ a b))

(define (gcd a b)
  (cond [(= b 0) a]
        [else (gcd b (modulo a b))]))

(define (sum-prime-squares a b)
  (filtered-accumulate prime? add 0 square a inc b))

(sum-prime-squares 1 100) ;65797

(define (prod-rel-primes a b)
  (define (rel-prime i)
    (= (gcd b i) 1))
 
  (filtered-accumulate rel-prime mul 1 identity a inc b))

(prod-rel-primes 1 55) ; 18538430496052184675120915079264902539141956381966336