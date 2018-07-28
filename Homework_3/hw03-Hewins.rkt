#lang racket
(#%provide (all-defined))

#|
IMPORTANT:
Overall, you are allowed to change this file in any way that does *not* affect the
compilation of the accompanying test file. Changes that are almost certain to break
the above restriction are:
  - changing the names of any definitions that are explicitely used in the tests
    (e.g. function names, relevant constants)

If there are any specific instructions for a problem, please read them carefully. Otherwise,
follow these general rules:

   - replace the 'UNIMPLEMENTED symbol with your solution
   - you are NOT allowed to change the number of arguments of the pre-defined functions,
     because changing the number of arguments automatically changes the semantics of the 
     function. Changing the name of the arguments is permitted since that change only
     affects the readability of the function, not the semantics.
   - you may write any number of helper functions as you want.

When done, make sure that the accompanying test file compiles. 
|#
;======================================01=======================================
(define (folderleft op default-el lst)
  (if (= 1 (length lst))
      (op (car lst) default-el)
      (foldl-335 op (op (car lst) default-el) (cdr lst)))
)

(define (foldl-335 op default-el lst)
  (if (null? lst)
      (op default-el)
      (folderleft op default-el lst))
)

(define (foldr-335 op default-el lst)
   (if (null? lst)
      (op default-el)
      (folderleft op default-el (reverse lst)))
)

;======================================02=======================================
(define (andmap-335 test-op lst)
  (if (null? lst)
      #t
      (if (test-op (car lst))
          (andmap-335 test-op (cdr lst))
          #f))
)

;======================================03=======================================
(define (filter-335 test-op lst)
  (if (null? lst)
      '()
      (filterrr test-op lst '())
))

(define (filterrr test-op lst save)
  (if (null? lst)
      save
      (let ([x (car lst)])
        (if (test-op x)
            (filterrr test-op (cdr lst) (append save (list x)))
            (filterrr test-op (cdr lst) save)
            )
        )
      )
  )
          
      

;======================================04=======================================
(define (map-reduce m-op r-op default-el lst)
  (foldl r-op default-el (map m-op lst))
)

;======================================05=======================================
(define (series n)
  (if (>= 0 n)
      1
      (foldl + 1 (map seriesss (cdr (map (lambda (x) (+ 1 x)) (range (+ 1 n)))))))
)

(define (seriesss num)
  (if (odd? num)
      (/ 1 (faktorial num))
      (/ -1 (faktorial num)))
)

(define (faktorial num)
  (if (<= num 1)
      1
      (* num (faktorial (- num 1)))))
;======================================06=======================================
(define (zip lst1 lst2)
  (map  list lst1 lst2)
)

;======================================07=======================================
(define (matrix-to-vector op mat)
  (addd op mat '())
)

(define (applyyy op x)
  (define z (reverse (map op (map car x))))
  (foldl op (car z) (cdr z))
)

(define (addd op mat finalres)
  (if (null? (car mat))
      finalres
      (addd op (map cdr mat) (append finalres (list (applyyy op mat))))))
  
#|
(define (applyyy op x resulttt)
  (display "x: ")
  (display x)
  (display "\nResult: ")
  (display resulttt)
  (display "\nz: ")
  (if (null? (car x))
      resulttt
      (let ([z (map op (map car x))])
        (display z)
        (display (list (foldr op (car z) (cdr z))))
        (append resulttt (list (foldr op (car z) (cdr z))))
        (display "\nResult after: ")
        (display resulttt)
        (append '(4) resulttt)
        (display "\n")
        (applyyy op (map cdr x) resulttt))))
|#
(matrix-to-vector string-append '(("a" "c" "e")
    ("b" "d" "f") ("1" "2" "3")))