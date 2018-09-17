#lang racket

; 1.3.2 Constructing Procedures Using Lambda


(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (square x) (* x x))

(define (pi-sum a b)
  (sum (lambda (x) (/ 1.0 (* x (+ x 2))))
       a
       (lambda (x) (+ x 4))
       b))

(define (integral f a b dx)
  (* (sum f (+ a (/ dx 2.0))
            (lambda (x) (+ x dx))
            b)
     dx))

(define (f x y)
  ; Assign a to be xy + 1 and b to be y - 1
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b)))
  )

; Exercise 1.34: Suppose we define the procedure
; What happens if we (perversely) ask the interpreter to evaluate the combination (f f)? Explain.


(define (ff g) (g 2))

; (ff ff) It will throw an error as it expects to apply ff to 2 but it applies ff to ff

; 1.3.3 Procedures as General Methods

(define (close-enough? x y) 
  (< (abs (- x y)) 0.001))

(define (average a b)
  (/ (+ a b) 2))

(define (search f neg-point pos-point)
  ; Get midpoint
  (let ((midpoint 
         (average neg-point pos-point)))
    ; If its small enough return it
    (if (close-enough? neg-point pos-point)
        midpoint
         ; Otherwise apply term to midpoint and check again
        (let ((test-value (f midpoint)))
          (cond 
           ((positive? test-value)
            (search f neg-point midpoint))
           ((negative? test-value)
            (search f midpoint pos-point))
           (else midpoint))))))

(define (half-interval-method f a b)
  ; Apply the term to start and end values
  (let ((a-value (f a))
        (b-value (f b)))
    ; If we have a < 0 and b > 0 then search in the a-b range
    (cond ((and (negative? a-value) 
                (positive? b-value))
           (search f a b))
          ((and (negative? b-value) 
                (positive? a-value))
           ; else search in the b-a range
           (search f b a))
          (else
           ; otherwise we have no root solution for f(x) = 0
           (error "Values are not of 
                   opposite sign" a b)))))

(half-interval-method sin 2.0 4.0) ;3.14111328125

(half-interval-method 
 (lambda (x) (- (* x x x) (* 2 x) 3))
 1.0
 2.0) ;1.89306640625

; Finding fixed points or f(x) = x

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  ; Check if v1, v2 are close enough
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) 
       tolerance))

  ; Apply term to guest and check if its close enough
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))

  ; Start with first-guess
  (try first-guess))

(fixed-point cos 1.0) ;.7390822985224023
(fixed-point (lambda (y) (+ (sin y) (cos y)))
             1.0) ;1.2587315962971173

(define (sqrt x)
  (fixed-point 
   (lambda (y) (average y (/ x y)))
   1.0))

; Exercise 1.35: Show that the golden ratio φ (1.2.2)
; is a fixed point of the transformation x↦1+1/xx↦1+1/x,
; and use this fact to compute φ by means of the fixed-point procedure.

(define (phi)
  (fixed-point (lambda (x) (+ 1 (/ 1 x)))
             1.0) ;1.2587315962971173
  )

(phi) ; 1.6180327868852458

; Exercise 1.36: Modify fixed-point so that it prints the sequence of
; approximations it generates, using the newline and display primitives shown
; in Exercise 1.22. Then find a solution to xx=1000xx=1000 by finding a fixed
; point of x↦log(1000)/log(x)x↦log⁡(1000)/log⁡(x).
; (Use Scheme’s primitive log procedure, which computes natural logarithms.)
; Compare the number of steps this takes with and without average damping.
; (Note that you cannot start fixed-point with a guess of 1,
;       as this would cause division by log(1)=0log⁡(1)=0.)


(define (fixed-point-debug f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) 
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (display next)
      (newline)
      (if (close-enough? guess next)
          next
          (try next))))

  (try first-guess))

(fixed-point-debug (lambda (x) (/ (log 1000) (log x)))
             (log 10)) ;4.555533026568209

; Exercise 1.37: Infinite continued fraction.
; 1) As an example, one can show that the infinite continued fraction
; expansion with the Ni and the Di all equal to 1 produces 1/φ, where φ
; One way to approximate an infinite continued fraction is to truncate the
; expansion after a given number of terms. Such a truncation—a so-called
; finite continued fraction k-term finite continued fraction
; Suppose that n and d are procedures of one argument (the term index ii)
; that return the NiNi and DiDi of the terms of the continued fraction.
; Define a procedure cont-frac such that evaluating (cont-frac n d k)
; computes the value of the kk-term finite continued fraction.
; Check your procedure by approximating 1/φ1/φ using
; 
; (cont-frac (lambda (i) 1.0)
;            (lambda (i) 1.0)
;            k)
; 
; for successive values of k. How large must you make k in order to get
; an approximation that is accurate to 4 decimal places?


(define (cont-frac n d k)
  (let ((n-value (n k))
        (d-value (d k)))
      (if (= k 0)
        0
        (/ n-value (+ d-value (cont-frac n d (- k 1)))))))

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           100) ;0.6179775280898876
; 2) If your cont-frac procedure generates a recursive process, write one that
; generates an iterative process. If it generates an iterative process, write
; one that generates a recursive process.


(define (cont-frac2 n d k)
  (cont-frac-iter n d k 1 (/ (n 1) (d 1))))

(define (cont-frac-iter n d k idx result)
  (let ((n-value (n idx))
        (d-value (d idx)))
      (if (= idx k)
        result
         (/ n-value (+ d-value (cont-frac-iter n d k (+ idx 1) result))))))

(cont-frac2 (lambda (i) 1.0)
           (lambda (i) 1.0)
           100) ; 0.6179775280898876

; Exercise 1.38: In 1737, the Swiss mathematician Leonhard Euler published a memoir
; De Fractionibus Continuis, which included a continued fraction expansion for
; e-2e-2, where ee is the base of the natural logarithms. In this fraction,
; the NiNi are all 1, and the DiDi are successively 1, 2, 1, 1, 4, 1, 1, 6, 1, 1, 8, ....
; Write a program that uses your cont-frac procedure from Exercise 1.37 to approximate
; ee, based on Euler’s expansion.

(define (e_2 n)
  (cont-frac (lambda (i) 1.0)
           (lambda (i)
             (if (= (- (* 3 i) 1) i)
                           i
                           1.0))
           n))

(e_2 100)

; Exercise 1.39: A continued fraction representation of the tangent function was published
; in 1770 by the German mathematician J.H. Lambert:
; Define a procedure (tan-cf x k) that computes an approximation to the tangent
; function based on Lambert’s formula. k specifies the number of terms to compute,
; as in Exercise 1.37.



; 1.3.4Procedures as Returned Values

(define (average-damp f)
  (lambda (x) 
    (average x (f x))))

; returns the average of 10 and 100, or 55
((average-damp square) 10)

(define (sqrt2 x)
  (fixed-point 
   (average-damp 
    (lambda (y) (/ x y)))
   1.0))

(define (cube-root x)
  (fixed-point 
   (average-damp 
    (lambda (y) 
      (/ x (square y))))
   1.0))

; Derivative
(define dx 0.00001)

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (cube x) (* x x x))

((deriv cube) 5) ;x|-> x^3 |- 3*x^2 = 3 * 25 =75

; Newton method
(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) 
            ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) 
               guess))

(define (sqrt3 x)
  (newtons-method 
   (lambda (y) 
     (- (square y) x)) 
   1.0))

(define (fixed-point-of-transform 
         g transform guess)
  (fixed-point (transform g) guess))

(define (sqrt4 x)
  (fixed-point-of-transform 
   (lambda (y) (/ x y))
   average-damp
   1.0))

(define (sqrt5 x)
  (fixed-point-of-transform 
   (lambda (y) (- (square y) x))
   newton-transform
   1.0))

; Exercise 1.40: Define a procedure cubic that can be used
; together with the newtons-method procedure in expressions
; of the form
; 
; (newtons-method (cubic a b c) 1)

; y |-> x/y^2 + x^2/y^2 + 

;(define (cubic a b c)
;  (lambda (y) (
;     (+ (/ x (square y))
;        (/ (* a (square x)) (square y))
;        (/ (* b x) (square y))
;        (/ c (square y)))
;     ))
;  )

; Exercise 1.41: Define a procedure double that takes a
; procedure of one argument as argument and returns a
; procedure that applies the original procedure twice.
; For example, if inc is a procedure that adds 1 to its argument,
; then (double inc) should be a procedure that adds 2.
; What value is returned by
; 
; (((double (double double)) inc) 5)


(define (double f)
  (lambda (x) (f (f x))))

(define (inc x) (+ x 1))

(((double (double double)) inc) 5) ; 21

; Exercise 1.42: Let ff and gg be two one-argument functions.
; The composition ff after gg is defined to be the function
; x↦f(g(x))x↦f(g(x)). Define a procedure compose that implements
; composition. For example, if inc is a procedure that adds 1 to its argument,
; ((compose square inc) 6)
; 49

(define (compose f g)
  (lambda (x) (f (g x))))

((compose square inc) 6) ; 49

; Exercise 1.43: If ff is a numerical function and nn is a positive integer,
; then we can form the nthnth repeated application of ff, which is defined
; to be the function whose value at xx is f(f(...(f(x))...))f(f(...(f(x))...)).
; For example, if ff is the function x↦x+1x↦x+1, then the nthnth repeated
; application of ff is the function x↦x+nx↦x+n. If ff is the operation of
; squaring a number, then the nthnth repeated application of ff is the function
; that raises its argument to the 2n-th2n-th power. Write a procedure that takes
; as inputs a procedure that computes ff and a positive integer nn and returns
; the procedure that computes the nthnth repeated application of ff.
; Your procedure should be able to be used as follows:
; 
; ((repeated square 2) 5)
; 625
; Hint: You may find it convenient to use compose from Exercise 1.42.

(define (repeated f times)
  (lambda (x)
    ; Define an iterative process that will call the func times
    (define (repeat f result curr)
            (if (= curr 0)
                result
                (repeat f (f result) (- curr 1))
            )
        )
     (repeat f x times)))

((repeated square 1) 5)

; Exercise 1.44: The idea of smoothing a function is an important concept in signal processing.
; If ff is a function and dxdx is some small number, then the smoothed version of ff
; is the function whose value at a point xx is the average of f(x-dx)f(x-dx), f(x)f(x),
; and f(x+dx)f(x+dx). Write a procedure smooth that takes as input a procedure that computes
; ff and returns a procedure that computes the smoothed ff. It is sometimes valuable to
; repeatedly smooth a function (that is, smooth the smoothed function, and so on) to obtain
; the n-fold smoothed function. Show how to generate the n-fold smoothed function of any
; given function using smooth and repeated from Exercise 1.43.


(define (smoothed f times)
  (lambda (x)
    (define (smoother y)
      (/ (+ (f (- y dx)) (f y) (f (+ y dx))) 3)
      )
    
    ((repeated smoother times) x)))

((smoothed inc 5) 5) ; 10.0

; Exercise 1.46: Several of the numerical methods described in this chapter are instances of
; an extremely general computational strategy known as iterative improvement. Iterative
; improvement says that, to compute something, we start with an initial guess for the answer,
; test if the guess is good enough, and otherwise improve the guess and continue the process
; using the improved guess as the new guess. Write a procedure iterative-improve that takes
; two procedures as arguments: a method for telling whether a guess is good enough and a method
; for improving a guess. Iterative-improve should return as its value a procedure that takes
; a guess as argument and keeps improving the guess until it is good enough. Rewrite the sqrt
; procedure of 1.1.7 and the fixed-point procedure of 1.3.3 in terms of iterative-improve.

(define (iterative-improve good-enough? improve-guess init)
  (lambda (x)
    (define (improve-iter good-enough? improve-guess result)
            (if (good-enough? x)
                result
                (improve-iter good-enough? improve-guess (improve-guess x))
            )
        )
   ((improve-iter good-enough? improve-guess ) x)))