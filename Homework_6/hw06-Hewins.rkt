#lang racket
(#%provide (all-defined))
(#%require (lib "eopl.ss" "eopl"))
(#%require "hw06-env-values.rkt")

;===============================================================================
;========================= Lexical and Grammar Specs ===========================
;===============================================================================

(define lexical-spec
  '(
    (whitespace (whitespace) skip)
    (comment ("#" (arbno (not #\newline))) skip)
    (number (digit (arbno digit)) number)
    (number ("-" digit (arbno digit)) number)
    (identifier (letter (arbno (or letter digit "_" "-" "?"))) symbol)
    )
  )

(define problem-1-answer
   (string-append "Line by line analysis:"
                  "1. Skip whitespace"
                  "2. # as the start of a comment, allowing an arbitrary # of chars, terminated by the first newline. Skip these as well."
                  "3 and 4. Allow numbers as (essentially) anything in the reals (datatype: number)."
                  "5. Identifiers are a letter, than any number of letters, digits or any of _, -, or ?. They are symbols."
  ))

(define grammar-spec
  '((program (expr (arbno expr)) a-program)
    (expr (number) num-expr)
    (expr ("up(" expr ")") up-expr)
    (expr ("down(" expr ")") down-expr)
    (expr ("left(" expr ")") left-expr)
    (expr ("right(" expr ")") right-expr)
    (expr ("(" expr expr ")") point-expr)
    (expr ("+" expr expr) add-expr)
    (expr ("-" expr expr) sub-expr)
    (expr ("*" expr expr) prod-expr)
    (expr ("/" expr expr) div-expr)
    (expr ("**" expr expr) pow-expr)
    (expr ("origin?(" expr ")") origin-expr)
    (expr ("if(" expr ")" "then" expr "else" expr) if-expr)
    (expr ("move(" expr expr (arbno expr) ")") move-expr)
    (expr (identifier) iden-expr)
    (expr ("{" (arbno var-expr) (arbno expr) "}") block-expr)
    (var-expr ("val" identifier "=" expr) val)
    (var-expr ("final" "val" identifier "=" expr) final-val))
)

;given one or more arguments this function will return a flat list
(define (flat-list el1 . rest)
  (flatten (list el1 rest))
)
;===============================================================================
;================================ Value-of =====================================
;===============================================================================
;Create the AST. Use value of to evaluate, use an empty env as initial condition
(define (run program-string)
  (value-of (parser program-string) (empty-env))
)

;decider for what function to use to eval a class
(define (value-of ast env)
  (if (program? ast)
      (cases program ast
        (a-program (ex1 ex2) (andmap (λ (x) (vox x env)) (flat-list ex1 ex2)))
        (else (raise (~a "value-of-program error: unimplemented expression: " ast))))
      (if (expr? ast)
          (vox ast env)
          (if (var-expr? ast)
              (vov ast env)
              (raise "This isn't in the language"))
)))

(define (finder st) (cond ((up-step? st) 0) ((right-step? st) 1) ((down-step? st) 2) ((left-step? st) 3)))
  
(define (whichdir dir a)
  (cond ((= 0 dir) (step-val (up-step a)))
        ((= 1 dir) (step-val (right-step a)))
        ((= 2 dir) (step-val (down-step a)))
        ((= 3 dir) (step-val (left-step a)))))

;this one will do expr
(define (vox ast env)
  (cases expr ast
    (num-expr (x) (num-val x))
    (up-expr (x) (step-val (up-step (num-val->n (value-of x env)))))
    (down-expr (x) (step-val (down-step (num-val->n (value-of x env)))))
    (left-expr (x) (step-val (left-step (num-val->n (value-of x env)))))
    (right-expr (x) (step-val (right-step (num-val->n (value-of x env)))))
    (point-expr (x y) (point-val (point (num-val->n (value-of x env)) (num-val->n (value-of y env)))))
    (origin-expr (x) (bool-val (if (= 0 (point->x (point-val->p (value-of x env))) (point->y (point-val->p (value-of x env)))) #t #f)))
    (add-expr (x y) (letrec ([s1 (step-val->st (value-of x env))]
                             [s2 (step-val->st (value-of y env))]
                             [stepper (λ (a) (if (> (single-step->n s1) (single-step->n s2))
                                                 (whichdir dir1 a)
                                                 (whichdir dir2 a)))]
                             [dir1 (finder s1)]
                             [dir2 (finder s2)])
                      (cond ((= dir1 dir2) (stepper (+ (single-step->n s1) (single-step->n s2))))
                            ((or (= 2 (+ dir1 dir2)) (= 4 (+ dir1 dir2))) (stepper (abs (- (single-step->n s1) (single-step->n s2)))))
                            (else (raise "ERRORRRRRR")))))
    (sub-expr (x y) (letrec ([s1 (step-val->st (value-of x env))]
                             [s2 (step-val->st (value-of y env))]
                             [stepper (λ (a) (if (> (single-step->n s1) (single-step->n s2))
                                                 (whichdir dir1 a)
                                                 (whichdir dir2 a)))]
                             [dir1 (finder s1)]
                             [dir2 (finder s2)])
                      (cond ((= dir1 dir2) (stepper (- (single-step->n s1) (single-step->n s2))))
                            ((or (= 2 (+ dir1 dir2)) (= 4 (- dir1 dir2))) (stepper (+ (single-step->n s1) (single-step->n s2))))
                            (else (raise "ERRORRRRRR")))))
    (prod-expr (step num) (define s (step-val->st (value-of step env)))
                          (whichdir (finder s) (* (single-step->n s) (num-val->n (value-of num env)))))
    (div-expr (step num)  (define s (step-val->st (value-of step env)))
                          (whichdir (finder s) (/ (single-step->n s) (num-val->n (value-of num env)))))
    (pow-expr (step num)  (define s (step-val->st (value-of step env)))
                          (whichdir (finder s) (expt (single-step->n s) (num-val->n (value-of num env)))))
    (if-expr (bool body else) (if (bool-val->b (value-of bool env))
                                  (value-of body env)
                                  (value-of else env)))
    (move-expr (f1 f2 f3) (foldl (λ (s1 s2) (compass s1 s2 env)) (value-of f1 env) (cons f2 f3)))
    (iden-expr (x) (apply-env env x))
    (block-expr (x y) (andmap (λ (z) (value-of z (extender x env))) y))
    (else (raise (~a "unimplemented expr: " ast)))
))

(define (compass st1 st2 env)
  (define first (step-val->st (value-of st1 env)))
  (define n (single-step->n first))
  (define p (point-val->p st2))
  (define x (point->x p))
  (define y (point->y p))
  (point-val (cond ((down-step? first) (point x (- y n)))
                   ((up-step? first) (point x (+ n y)))
                   ((left-step? first) (point (- x n) y))
                   ((right-step? first) (point (+ n x) y))
                   (else (raise "Something really went wrong here...not a step"))))
)
  
(define (extender x y)
  (if (null? x)
      y
      (extender (cdr x) (value-of (car x) y))))

(define (vov ast env)
  (cases var-expr ast
    (val (f1 f2) (extend-env-wrapper f1 (value-of f2 env) env #f))
    (final-val (f1 f2) (extend-env-wrapper f1 (value-of f2 env) env #t))
    (else (raise (~a "value-of-var-expr error: unimplemented expression: " ast)))
  )
)

;==============================================================================================================
;==============================================================================================================
;==============================================================================================================
(sllgen:make-define-datatypes lexical-spec grammar-spec)
(define (show-data-types) (sllgen:list-define-datatypes lexical-spec grammar-spec))
(define parser (sllgen:make-string-parser lexical-spec grammar-spec))
(define scanner (sllgen:make-string-scanner lexical-spec grammar-spec))