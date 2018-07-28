#lang racket
(#%provide (all-defined))
#|
If there are any specific instructions for a problem, please read them carefully. Otherwise,
follow these general rules:
   - replace the 'UNIMPLEMENTED symbol with your solution
   - you are NOT allowed to change the names of any definition
   - you are NOT allowed to change the number of arguments of the pre-defined functions,
     but you can change the names of the arguments if you deem it necessary.
   - make sure that you submit an asnwer sheet that compiles! If you cannot write
     a correct solution at least make it compile, if you cannot make it compile then
     comment it out. In the latter case, make sure that the default definitions
     for the problem are still present. 
   - you may use any number of helper functions you deem necessary.

When done, make sure that you do not get any errors when you hit the "Run" button. 
If you cannot come up with a correct solution then please make the answer-sheet
compile correctly and comment any partial solution you might have; if this is the case,
the default definitions will have to be present!
|#
;======================================01=======================================
(define (list-of-even-numbers? lst)
  (if (null? lst)
      #t
      (if (list? lst)
          (if (>= (length lst) 2)
              (check-1 lst)
              (if (and (number? (car lst)) (even? (car lst)))
                  #t
                  #f))
          #f
)))

(define (check-1 lst)
  (if (= (length lst) 1)
      (if (and (number? (car lst)) (even? (car lst))) #t #f)
      (if (and (number? (car lst)) (even? (car lst)))
          (check-1 (cdr lst))
          #f)))

;======================================02=======================================
;;for n > 0
;Sn = 1/1 + 1/4 + 1/9 + 1/16 + ...
(define (series-a n)
  (if (<= n 1)
      1/1
      (+ (/ 1 (* n n)) (series-a (- n 1))))
)

;====
;;for n >= 0
;Sn = 1 - 1/2 + 1/6 - 1/24 + ...
(define (series-b n)
  (if (<= n 0)
      1/1
      (if (even? n)
          (+ (/ 1 (faktorial (+ n 1) 1)) (series-b (- n 1)))
          (+ (/ -1 (faktorial (+ n 1) 1)) (series-b (- n 1)))))
)

(define (faktorial n m)
  (cond ((> m n) 0)
        ((= m n) m)
        ((< m n) (* m (faktorial n (+ m 1))))
        )
  )
      
      
;======================================03=======================================
(define (carpet n)
  (if (>= 0 n)
      '((%))
      (addto '((%)) (+ 1 n) 1)
))

(define (build n sym lst)
  (if (= n 1)
      (append lst (list sym))
      (build (- n 1) sym (append lst (list sym)))
      )
)

(define (addto table n m)
  (if (= m n)
      table
      (let
        ((sym (if (even? m) '% '+)))
         (addto (extend-carpet table (build (+ 1 (* 2 m)) sym '()) sym) n (+ 1 m))) ))
        
(define (extend-carpet table row sym)
  (letrec
      ((temp (map (λ (x) (append (list sym) x)) table))
       (temp2 (map (λ (x) (append x (list sym))) temp))
       (temp3 (cons row temp2))
       (temp4 (append temp3 (list row))))
  temp4)
)

  
;======================================04=======================================
(define (pascal n)
  (if (= n 1)
      '((1))
      (append (pascal (- n 1)) (list (lastLine (pascal (- n 1)))))))

(define (lastLine lst)
  (append (cons '1 (doAddition (last lst))) (list '1)))

(define (doAddition lst)
  (if (= (length lst) 1)
      '()
      (cons (+ (first lst) (second lst)) (doAddition (cdr lst)))
))

;======================================05=======================================
(define (balanced? in)
  (define lst (string->list in))
  (hit lst 0)
)

(define (hit lst n)
  (if (< n 0)
      #f
      (if (null? lst)
          (= n 0)
          (let ([char (car lst)])
            (if (equal? char #\()
                (hit (cdr lst) (+ n 1))
                (if (equal? char #\))
                    (hit (cdr lst) (- n 1))
                    (hit (cdr lst) n))))))
)
  
              
;======================================06=======================================
(define (list-of-all? predicate lst)
  (if (null? lst)
      #t
      (if (predicate (car lst))
          (list-of-all? predicate (cdr lst))
          #f))
)

;======================================07=======================================
(define (create-mapping keys vals)
  (cond
    ((null? keys) (raise "It makes no sense for null to be the key or the value does it?"))
    ((not (eq? (length keys) (length vals))) (raise "The lists are not of equal length."))
    ((not (list-of-all? symbol? keys)) (raise "The keys are not all symbols."))
    (else (lambda (name) (search name keys vals)))
  )
)

(define (sym-not-found-err sym)
  (string-append "Could not find mapping for symbol '" (symbol->string sym)))

(define (search name keys vals)
  (cond ((null? keys) 
          (raise (sym-not-found-err name)))
	
        ((eq? (car keys) name)
		  (car vals))
		 
		(else (search name (cdr keys) (cdr vals)))
  )
)
