#lang racket
Exercise 2.71: Suppose we have a Huffman tree for an alphabet of n symbols, and that the relative frequencies of the symbols are 1,2,4,…,2n−1. Sketch the tree for n=5; for n=10. In such a tree (for

'((leaf F 6) ((leaf E 5) ((leaf D 4) ((leaf C 3) ((leaf A 1) (leaf B 2) (A B) 3) (C A B) 6) (D C A B) 10) (E D C A B) 15) (F E D C A B) 21)


(F E D C A B) 21
     /\
    /  \ (E D C A B) 15)
(F 6)   \
        /\
       /  \(D C A B) 10)
      /   / \
     /  (D 4)\
    /         \ (C A B) 6)
(E 5)         / \
          (C 3)  \
                 / \ (B A) 3)
              (B 2) \
                     \ (A 1)
     
Exercise 2.71: Suppose we have a Huffman tree for an alphabet of n symbols, and that the relative frequencies of the symbols are 1,2,4,...,2n-1. Sketch the tree for n=5; for n=10. In such a tree (for general n) how many bits are required to encode the most frequent symbol? The least frequent symbol?eps needed to search the symbol list at each node encountered. To answer this question in general is difficult. Consider the special case where the relative frequencies of the n symbols are as described in Exercise 2.71, and give the order of growth (as a function of n) of the number of steps needed to encode the most frequent and least frequent symbols in the alphabet.