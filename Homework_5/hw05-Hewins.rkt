#lang racket
(#%provide (all-defined))
(#%require (lib "eopl.ss" "eopl"))

#|
IMPORTANT:
Overall, you are allowed to change this file in any way that does not affect the
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
   - you may write any number of helper functions

When done, make sure that the accompanying test file compiles. 
If you cannot come up with a correct solution then please make the answer-sheet
compiles. If you have partial solutions that do not compile please comment them out,
if this is the case, the default definitions will have to be present since the tests
will be expecting functions with the names defined here.

Submission guidelines:
   - please rename this file to hw05-yourlastname.rkt prior to submission
   - also rename hw05-tests.rkt to hw05-yourlastname-tests.rkt
   - upload both hw05-yourlastname.rkt and hw05-tests.rkt
|#
;=======================================01======================================
(define (invalid-args-msg fun-name-as-string
                          expected-value-type-as-predicate-string
                          received)
  (string-append "Invalid arguments in: " fun-name-as-string " --- "
                 "expected: " expected-value-type-as-predicate-string " --- "
                 "received: " (~a received)
                 )
)

;You can compare the contents of this answer sheet with the answer sheet of the
;previous homework to infer what is generated automatically by define-datatype.

(define-datatype step step?
  (left-step (n number?))
  (right-step (n number?))
  (up-step (n number?))
  (down-step (n number?))
  (seq-step (step1 step?)(step2 step?))
)

(define (up-step? st)
  (step? st)
)

(define (down-step? st)
  (step? st)  
)

(define (left-step? st)
  (step? st)  
)

(define (right-step? st)
  (step? st)  
)

(define (seq-step? st)
  (step? st)  
)

;;to avoid needless duplication we will only implement one extractor to handle all the
;;simple steps, rather than 4. So this should take: up, down, left and right steps.
(define (single-step->n st)
  (if (step? st)
      (cases step st
        (left-step (n) n)
        (right-step (n) n)
        (down-step (n) n)
        (up-step (n) n)
        (else (raise (invalid-args-msg "single-step->n" "single-step?" st))))
      (raise (invalid-args-msg "single-step->n" "single-step?" st)))
)

;;two extractors, one for each piece of data representing a sequential step
(define (seq-step->st-1 st)
  (cases step st
    (seq-step (st1 st2) st1)
    (else (raise (invalid-args-msg "seq-step->st-1" "seq-step?" st)))
    )
)

(define (seq-step->st-2 st)
  (cases step st
    (seq-step (st1 st2) st2)
    (else (raise (invalid-args-msg "seq-step->st-1" "seq-step?" st)))
    )
)
;;===================================
(define (move start-p st)
  (cases step st
    (right-step (n) (map + start-p (list n 0)))
    (left-step (n) (map - start-p (list n 0)))
    (up-step (n) (map + start-p (list 0 n)))
    (down-step (n) (map - start-p (list 0 n)))
    (seq-step (st1 st2) (move (move start-p st1) st2))
    )
)
;=======================================02======================================
;2.a
(define (exception-no-binding-msg sym)
  (string-append "No binding for '" (~a sym))
  )

(define-datatype environment environment?
  (empty-env)
  (extend-env (sym symbol?) (val number?) (environment environment?))
  (wrapper (sym symbol?) (val number?) (environment environment?) (final boolean?))
  )

;this is built to handle question 2b as well as 2a so it already has wrapper inside it
(define (apply-env env search-sym)
  (cases environment env
    (empty-env () (raise (exception-no-binding-msg search-sym)))
    (extend-env (sym val enviro)
                (if (eqv? sym search-sym)
                    val
                    (apply-env enviro search-sym)))
    (wrapper (sym val enviro f)
             (if (eqv? sym search-sym)
                 val
                 (apply-env enviro search-sym)))))

;==========
;2.b
(define (exception-sym-final-msg sym)
  (string-append "Symbol '" (~a sym) " is final and cannot be overriden.")
  )

(define FINAL #t)
(define NON-FINAL #f)

(define (isfinal env search-sym)
  (cases environment env
    (empty-env () #f)
    (extend-env (sym val environment) #f)
    (wrapper (sym val environment final)
             (if (eqv? sym search-sym)
                 final
                 (isfinal environment search-sym)))
    )
  )

(define (extend-env-wrapper sym val old-env final?)
  (if (isfinal old-env sym)
      (raise (exception-sym-final-msg sym))
      (wrapper sym val old-env final?))
)
