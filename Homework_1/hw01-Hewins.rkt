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
     for the problem are still present. Otherwise you may be penalized up to 25%
     of the total points for the homework.
   - you may use any number of helper functions you deem necessary.

When done, make sure that you do not get any errors when you hit the "Run" button. You will
lose up to 25% of the total points for the entire homework depending on the number of errors.
If you cannot come up with a correct solution then please make the answer-sheet
compile correctly and comment any partial solution you might have; if this is the case,
the default definitions will have to be present!

|#
;======================================01=======================================
;((3 + 3) * 9)
;equal to 54
(define (p1-1)
  (* 9 (+ 3 3)))

;((6 * 9) / ((4 + 2) + (4 * 3)))
;equal to 3
(define (p1-2)
  (/ (* 6 9) (+ (+ 4 2) (* 4 3))))

;(2* ((20 - (91 / 7)) * (45 - 42)))
;equal to 42
(define (p1-3)
  (* 2 (- 20 (/ 91 7)) (* (- 45 42))))
;======================================02=======================================
;write your answer as a string; you do not need to write any special escape
;characters to distinguish new lines.
(define (p2)
  "Convert from infix to prefix, respecting the rules of order of operations."
)
;======================================03=======================================
;;Write the definitions of x,y,z here:
(define x 2)
(define y 3)
(define z 4)

;======================================04=======================================
;you will need to have solved problem 3. The values x,y,z are not parameters
;of this function!
(define (p4)
  (cond
   ((and (> x z) (> y z)) (+ x y))
   ((and (> y x) (> z x)) (+ y z))
   ((and (> x y) (> z y)) (+ x z)) 
   (else 0)
  )

)

;======================================05=======================================
(define (p5)
  (cond
   ((and (< x z) (< y z)) (+ x y))
   ((and (< y x) (< z x)) (+ y z))
   ((and (< x y) (< z y)) (+ x z)) 
   (else 0)
  )
)

;======================================06=======================================
(define (p6)
  (= x y)  
)

;======================================07=======================================
;same instructions as problem 02.
(define p7
  "The first assigns to a variable, the second makes it a procedure."
)

;======================================08=======================================
;same instructions as problem 02.
(define p8
  "' says treat this as plaintext and do not evaluate")

;======================================09=======================================
;same instructions as problem 02.
(define p9
  "A list is an immutable data structure, ' tells racket to not evaluate the expression and treat it as plaintext or it means treat this as a symbol."
)

;======================================10=======================================
;same instructions as problem 02.
(define p10
  "Strings are immutable tuples of characters, symbols are essentially pointers. With a symbol you can use = to compare other characters to see if they point to the same thing.
   To compare strings, you have to compare every char."
)

;======================================11=======================================
;(4 2 6 9)
(define (p11-1)
  (list 4 2 6 9)
)

;(spaceship
;  (name(serenity))
;  (class(firefly)))
(define (p11-2)
  (list 'spaceship (list 'name (list 'serenity)) (list 'class (list 'firefly)))
)

;(2 * ((20 - (91 / 7)) * (45 - 42)))
(define (p11-3)
  (list '2 '* (list (list '20 '- (list '91 '/ '7)) '* (list '45 '- 42)))
)

;======================================12=======================================
(define example '(a b c))

(define (p12-1 lst)
  (cons 'd lst))

(define (p12-2 lst)
  (list (car lst) (car (cdr lst)) 'd (car lst) (car (cdr lst)))
)

(define (p12-3 lst)
  (list (car(cdr lst)) (car(cdr(cdr lst))) 'd (car lst)))


;======================================13=======================================
(define p13
  "eq? returns #t if both args point to the same object, otherwise it returns false. equal? returns #t if the two args have equivalent values, whether or not they reference the same object"
)
; write your answer as a string; you do not need to write any special escape
; characters to distinguish new lines.


;======================================14=======================================
;Number 14
; "This is a custom error message we will be using next. Symbol 'answer-to-everything was not paired with value 42"
(define (create-error-msg sym val)
  (define str (string-append "This is a custom error message we will be using next. "))
  (define str2 (string-append str "Symbol '" (symbol->string sym)))
  (define str3 (string-append str2 " was not paired with value " (number->string val)))
  (string-append str3)
)
;======================================15=======================================
;Number 15
(define (check-correctness pair)
  (if (equal? "answer-to-everything" (symbol->string (car pair)))
     (if (eq? 42 (cadr pair))
         #t
         (raise (create-error-msg (car pair) 42)))
     #f))

;======================================16=======================================
;No answer necessary
