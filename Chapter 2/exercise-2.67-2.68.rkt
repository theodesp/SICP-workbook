#lang racket

;Exercise 2.67: Define an encoding tree and a sample message:
;
;(define sample-tree
;  (make-code-tree 
;   (make-leaf 'A 4)
;   (make-code-tree
;    (make-leaf 'B 2)
;    (make-code-tree 
;     (make-leaf 'D 1)
;     (make-leaf 'C 1)))))
;
;(define sample-message 
;  '(0 1 1 0 0 1 0 1 0 1 1 1 0))
;Use the decode procedure to decode the message, and give the result.

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch 
                (car bits) 
                current-branch)))
          (if (leaf? next-branch)
              (cons 
               (symbol-leaf next-branch)
               (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) 
                        next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit: 
               CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) 
         (cons x set))
        (else 
         (cons (car set)
               (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set 
         (make-leaf (car pair)    ; symbol
                    (cadr pair))  ; frequency
         (make-leaf-set (cdr pairs))))))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) 
                (symbols right))
        (+ (weight left) (weight right))))

(define sample-tree '((leaf A 4) ((leaf B 2) ((leaf D 1) (leaf C 1) (D C) 2) (B D C) 4) (A B D C) 8))

(define sample-message 
  '(0 1 1 0 0 1 0 1 0 1 1 1 0))

; (decode sample-message sample-tree)

;Exercise 2.68: The encode procedure takes as arguments a message and a tree and produces the list of bits that gives the encoded message.
;
;(define (encode message tree)
;  (if (null? message)
;      '()
;      (append 
;       (encode-symbol (car message) 
;                      tree)
;       (encode (cdr message) tree))))
;Encode-symbol is a procedure, which you must write, that returns the list of bits that encodes a given symbol according to a given tree. You should design encode-symbol so that it signals an error if the symbol is not in the tree at all. Test your procedure by encoding the result you obtained in Exercise 2.67 with the sample tree and seeing whether it is the same as the original sample message.

(define (encode message tree)
  (if (null? message)
      '()
      (append 
       (encode-symbol (car message) 
                      tree)
       (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (cond ((not (list? (memq symbol (symbols tree))))
         (error "bad symbol 
               encode-symbol" symbol))
        (else (encode-symbol-helper symbol tree '()))))

(define (encode-symbol-helper symbol branch curr)
   (cond ((leaf? branch)
          curr)
         ((and (leaf? (left-branch branch))
               (eq? (symbol-leaf (left-branch branch)) symbol))
          (append curr '(0)))
         (else (encode-symbol-helper symbol (right-branch branch) (append curr '(1))))))
      
; (encode '(A D A B B C A) sample-tree)
