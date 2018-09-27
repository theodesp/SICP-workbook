#lang racket

(require racket/trace)

;Exercise 2.69: The following procedure takes as its argument a list of symbol-frequency pairs (where no symbol appears in more than one pair) and generates a Huffman encoding tree according to the Huffman algorithm.
;
;(define (generate-huffman-tree pairs)
;  (successive-merge 
;   (make-leaf-set pairs)))
;Make-leaf-set is the procedure given above that transforms the list of pairs into an ordered set of leaves. Successive-merge is the procedure you must write, using make-code-tree to successively merge the smallest-weight elements of the set until there is only one element left, which is the desired Huffman tree. (This procedure is slightly tricky, but not really complicated. If you find yourself designing a complex procedure, then you are almost certainly doing something wrong. You can take significant advantage of the fact that we are using an ordered set representation.)

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) 
                (symbols right))
        (+ (weight left) (weight right))))

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

(define (generate-huffman-tree pairs)
  (successive-merge 
   (make-leaf-set pairs)))

(define (successive-merge leaf-set)
  (if (= (length leaf-set) 2)
      (make-code-tree (car leaf-set) (cadr leaf-set))
      (successive-merge-helper (cddr leaf-set) (make-code-tree (car leaf-set) (cadr leaf-set)))))

(define (successive-merge-helper leaf-set curr)
  (if (null? leaf-set)
      curr
      (successive-merge-helper (cdr leaf-set) (make-code-tree (car leaf-set) curr))))
      
;Exercise 2.70: The following eight-symbol alphabet with associated relative frequencies was designed to efficiently encode the lyrics of 1950s rock songs. (Note that the “symbols” of an “alphabet” need not be individual letters.)
;
;
;A    2    NA  16
;BOOM 1    SHA  3
;GET  2    YIP  9
;JOB  2    WAH  1
;Use generate-huffman-tree (Exercise 2.69) to generate a corresponding Huffman tree, and use encode (Exercise 2.68) to encode the following message:
;
;Get a job
;Sha na na na na na na na na
;
;Get a job
;Sha na na na na na na na na
;
;Wah yip yip yip yip 
;yip yip yip yip yip
;Sha boom
;How many bits are required for the encoding? What is the smallest number of bits that would be needed to encode this song if we used a fixed-length code for the eight-symbol alphabet?

(define s '((A 2) (NA  16) (BOOM 1) (GET  2) (SHA  3) (YIP  9) (WAH  1) (JOB  2)))

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

(provide encode generate-huffman-tree)

(generate-huffman-tree '((A 1) (B  2) (C 3) (D  4) (E  5) (F  6)))
