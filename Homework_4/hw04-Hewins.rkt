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
   - you may write any number of helper functions

When done, make sure that the accompanying test file compiles. 
If you cannot come up with a correct solution then please make the answer-sheet
compiles. If you have partial solutions that do not compile please comment them out,
if this is the case, the default definitions will have to be present since the tests
will be expecting functions with the names defined here.

Submission guidelines:
   - please rename the file to hw04-yourlastname-answer.rkt prior to submission
   - only the renamed file file needs to be uploaded
|#
;======================================01=======================================
(define-syntax-rule (for {var <- value-range} yield result)
  (map (λ (var) result) value-range)
)
;======================================02=======================================
(define-syntax-rule (seq expr1 expr2)
  ((λ () expr1 expr2))
)

;====
(define-syntax-rule (while condition body)
  (letrec ((loop (λ () (if condition (begin body (loop)) 0))))
    (loop)
  )
)
;======================================03=======================================
(define (invalid-args-msg string-name expected-value-type received)
  (string-append "Invalid arguments in: " string-name " --- expected: " expected-value-type " --- received: " received)
)

(define (dostep dir n)
  (if (number? n)
      (list dir n)
      (raise (invalid-args-msg (string-append (symbol->string dir) "-step") "number?" n))
))

(define (up-step n)
  (dostep 'up n)
)

(define (down-step n)
  (dostep 'down n)
)

(define (left-step n)
  (dostep 'left n)
)

(define (right-step n)
  (dostep 'right n)
)

(define (seq-step s1 s2)
  (if (step? s1)
      (if (step? s2)
          (list 'seq s1 s2)
          (raise (invalid-args-msg "seq-step" "step?" s2)))
      (raise (invalid-args-msg "seq-step" "step?" s1)))
)

(define (up-step? st)
  (a-step? st 'up)
)

(define (down-step? st)
  (a-step? st 'down)
)

(define (left-step? st)
  (a-step? st 'left)
)

(define (right-step? st)
  (a-step? st 'right)
)

(define (onestep? st)
  (or (up-step? st) (down-step? st) (right-step? st) (left-step? st))
)

(define (a-step? st dir)
  (if (list? st)
      (and (= (length st) 2) (equal? (car st) dir) (number? (second st)))
      #f
  )
)

(define (step? st)
  (or (onestep? st) (seq-step? st))
)

(define (seq-step? st)
  (if (list? st)
      (and (= (length st) 3) (equal? (car st) 'seq) (step? (second st)) (step? (third st)))
      #f
  )
)

(define (single-step->n st)
  (if (onestep? st)
      (cadr st)
      (raise (invalid-args-msg "single-step->n" "single-step?" st))
      )
  )

(define (seq-step->st-1 st)
  (if (seq-step? st)
      (second st)
      (raise (invalid-args-msg "seq-step->st-1" "seq-step?" st))
      )
  )

(define (seq-step->st-2 st)
  (if (seq-step? st)
      (third st)
      (raise (invalid-args-msg "seq-step->st-2" "seq-step?" st))
      )  
  )

(define (euclid x y)
  (list x y)
)

(define (xparam x)
  (car x)
)

(define (yparam y)
  (second y)  
)

(define (move start-p st)
  (cond ((up-step? st) (euclid (xparam start-p) (+ (yparam start-p) (single-step->n st))))
        ((down-step? st) (euclid (xparam start-p) (- (yparam start-p) (single-step->n st))))
        ((left-step? st) (euclid ( - (xparam start-p) (single-step->n st)) (yparam start-p)))
        ((right-step? st) (euclid ( + (xparam start-p) (single-step->n st)) (yparam start-p)))
        ((seq-step? st) (move (move start-p (seq-step->st-1 st)) (seq-step->st-2 st)))
        (else (raise (invalid-args-msg "move" "step?" st)))
  )  
)
;======================================04=======================================
;singleton-set should return a function that takes a number as an argument and
;tells whether or not that number is in the set
(define (singleton-set x)
  (λ (a) (equal? x a))
)

;the set of all elements that are in either 's1' or 's2'
(define (union s1 s2)
  (λ (a) (or (s1 a) (s2 a)))
)

;the set of all elements that are in both  in 's1' and 's2'
(define (intersection s1 s2)
  (λ (a) (and (s1 a) (s2 a)))
)

;the set of all elements that are in 's1', but that are not in 's2'
(define (diff s1 s2)
  (λ (a) (and (s1 a) (not (s2 a))))
)

;returns the subset of s, for which the predicate 'predicate' is true.
(define (filter predicate s)
  (λ (a) (and (s a) (predicate a)))
)

(define bound 100)

;builds the set to make computation easier
(define (buildset s built counter)
  (if (not (null? (cdr s)))
      (buildset (cdr s) (append built (if (car s) (list counter) '())) (+ 1 counter))
      built
))


(define (exists? predicate s)
  (define x (map (λ (a) (if (s a) #t #f)) (range bound)))
  (define theset (buildset x '() 0))
  (ormap predicate theset)
)

(define (prime? n)
  (define (non-divisible? n)
    (lambda (i)
      (not (= (modulo n i) 0))))
  (define range-of-prime-divisors (cddr (range (+ (integer-sqrt n) 1))))
  (if (equal? n 1)
      #f
      (andmap (non-divisible? n) range-of-prime-divisors)
      )
  )

;returns whether or not the predicate is true for all the elements
;of the given set s
(define (all? predicate s)
  (define x (map (λ (a) (if (s a) #t #f)) (range bound)))
  (define theset (buildset x '() 0))
  (andmap predicate theset)
)
;returns a new set where "op" has been applied to all elements
; NOTE: just because a procedure/function has the word "map" in it, it 
;       doesn't mean you have to use map higher order function to implement it. 
;       Map is a functional operation with well defined behavior that 
;       is not tied to any implementation.
(define (map-set op s)
  (define x (map (λ (a) (if (s a) #t #f)) (range bound)))
  (define theset (map op (buildset x '() 0)))
  (foldr union (singleton-set (car theset)) (map singleton-set (cdr theset)))
)

;just a sample predicate


;=====================================05====================================
; FYI:
;  to emphasize the procedural-based approach to implement "step" data type and to
;  contrast it with the data structure-based approach for "step" implementation 
;  used in p3, here we add "-proc" suffix to each corresponding function name.

;====p5-a================
(define (up-step-proc n)
  (if (number? n)
      (λ (a) (if (eq? a 'extract-size)
            n
            (if (eq? a 'up-step)
                #t
                #f)))
      (invalid-args-msg "up-step-proc" "number?" "not-a-number")
))

(define (down-step-proc n)
  (if (number? n)
      (λ (a) (if (eq? a 'extract-size)
            n
            (if (eq? a 'down-step)
                #t
                #f)))
      (invalid-args-msg "down-step-proc" "number?" "not-a-number")
))

(define (left-step-proc n)
  (if (number? n)
      (λ (a) (if (eq? a 'extract-size)
            n
            (if (eq? a 'left-step)
                #t
                #f)))
      (invalid-args-msg "left-step-proc" "number?" "not-a-number")
))

(define (right-step-proc n)
  (if (number? n)
      (λ (a) (if (eq? a 'extract-size)
            n
            (if (eq? a 'right-step)
                #t
                #f)))
      (invalid-args-msg "right-step-proc" "number?" "not-a-number")
))

(define (seq-step-proc st-1 st-2)
  (if (and (step-proc? st-1) (step-proc? st-2))
      (λ (a)
        (if (eq? a 'extract-size)
            (list (st-1 'extract-size) (st-2 'extract-size))
            (if (eq? a 'extract-s1)
                st-1
                (if (eq? a 'extract-s2)
                    st-2
                    (if (eq? a 'seq-step)
                        #t
                        #f)))))
      (invalid-args-msg "seq-step-proc" "step-proc?" "not-a-step-proc")
))

;;====
(define (up-step-proc? st)
  (st 'up-step)
)

(define (down-step-proc? st)
  (st 'down-step)
)

(define (left-step-proc? st)
  (st 'left-step)  
)

(define (right-step-proc? st)
  (st 'right-step)  
)

(define (seq-step-proc? st)
  (st 'seq-step)  
)

;This is a predicate that tells you whether or not st is a step,
; it should return true when given either up, down, left, right or seq steps.
(define (step-proc? st)
  (if (procedure? st)
      (or (up-step-proc? st) (down-step-proc? st) (left-step-proc? st) (right-step-proc? st) (seq-step-proc? st))
      #f)
)


;;to avoid needless duplication we will only implement one extractor to handle all the
;; simple steps, rather than four of them. So this should take: up, down, left and right 
;; steps. 
(define (single-step-proc->n st)
  (if (step-proc? st)
      (st 'extract-size)
      (invalid-args-msg "single-step-proc->n" "single-step-proc?" "not-a-single-step-proc"))
)

;;two extractors
(define (seq-step-proc->st-1 st)
  (if (not (step-proc? st))
      (invalid-args-msg "seq-step-proc->n" "seq-step-proc?" "not-a-seq-step-proc")
      (st 'extract-s1))
)


(define (seq-step-proc->st-2 st)
  (if (step-proc? st)
      (st 'extract-s2)
      (invalid-args-msg "seq-step-proc->n" "seq-step-proc?" "not-a-seq-step-proc"))
)
;;========p5-b
(define (move-proc origin step-proc)
  (cond
    ((up-step-proc? step-proc) (map + origin  (list 0 (single-step-proc->n step-proc))))
    ((down-step-proc? step-proc) (map - origin (list 0 (single-step-proc->n step-proc))))
    ((left-step-proc? step-proc) (map - origin (list (single-step-proc->n step-proc) 0)))
    ((right-step-proc? step-proc) (map + origin (list (single-step-proc->n step-proc) 0)))   
    ((seq-step-proc? step-proc) (move-proc (move-proc origin (seq-step-proc->st-1 step-proc)) (seq-step-proc->st-2 step-proc)))
))