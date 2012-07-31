;#lang scheme
;(require (planet williams/science/random-source))

(define rember
        (lambda  (a L) 
              (cond ((null? L)                           (quote () ) )
                    ((equal? (car  L) a)      (rember  a  (cdr  L) ) ) 
                    (else  (cons  (car  L)  (rember  a  (cdr  L) ) ) )
              )
        )
)

(define count 
       (lambda (a L) 
                (cond ( (null? L)                                  0 ) 
                      ( (equal? a (car L))  (+ 1 (count a (cdr L)) ) )
                      ( else                      (count a (cdr L) ) )
                )
       )
)

(define occurences 
        (lambda (L) 
                (if  (null? L) 
                     (quote ( ) )
                     (cons (list (car L)        (count (car L) L) )
                            (occurences (rember (car L) (cdr L) ) )
                     )
                )    
        )
)

;works on bher
(define non-dec (lambda (n l) (if (= n l) (list l) (non-dec n (+ l 1)))))

;works on bher
(define (random-element L)
  (list-ref L (car (non-dec (floor (* (random-real) (length L))) 0))))

;(define bit-list 
;    (lambda(obs n) (if (eq? obs 0) '() (cons  (list n (* (- (/ obs 2) (floor (/ obs 2))) 2)) (bit-list (truncate (/ obs 2)) (+ n 1)))))
;)

;works on bher
(define modulo-n 
    (lambda(num numerate) (* (- (/ num numerate) (floor (/ num numerate))) numerate))
)

;(define check-bit
;    (lambda(obs n) (if (eq? (caar obs) n) (if (eq? (cadar obs) 1) #t #f) (check-bit (cdr obs) n)))
;)

;works on bher
(define recursive-divide (lambda (n L) (if (null? L ) (/ n 1) (/ (recursive-divide n (cdr L)) (car L)))))

;works on bher
(define recursive-and (lambda (X) (if (null? X) #t (and (car X) (recursive-and (cdr X))))))

;works on bher
(define recursive-or (lambda (X) (if (null? X) #f (or (car X) (recursive-or (cdr X))))))

(define patternBuildRepeat 
    (lambda(len data)
        (if (eq? len 0) '() (cons (last (patternBuild rules data)) (patternBuildRepeat (- len 1) (last (patternBuild rules data)))))
    ) 
)

(define patternBuild 
        (lambda (rules L) 
                 (if (null? rules) (quote ( ) )
                     (letrec ([rule (random-element rules)]
                              [aug (flatten (append (list (rule L) L)))])
                         (cons (remq '() (append (list (rule L)) L)) (patternBuild (remq rule rules) (remq '() (remq '() (append (list (rule L)) L)))))
                     )
                 )
        )
)

(define patternBuild-repeat-n 
        (lambda (n rules L) 
                 (if (= 0 n) '()
                     (letrec ([rule (random-element rules)]
                              [aug (flatten (append (list (rule L) L)))])
                         (append (list aug) (patternBuild-repeat-n (- n 1) rules aug))
                     )
                 )
        )
)

(define pick-n-rand-rules 
        (lambda ( n rules) 
                 (if (= 0 n) (quote ( ) )
                     (cons (random-element rules) (pick-n-rand-rules (- n 1) (cdr rules)))
                 )
        )
)

;works on bher
#|(define makeRules
    (lambda(L n) 
         (letrec ([logic-operator (if (= 0 (modulo-n n 2)) (lambda (x)  (recursive-or x)) (lambda (x)  (recursive-and x)))]
                  [reg-op1 (if (= 0 (modulo-n (truncate (recursive-divide n '(2))) 3)) (lambda (x)  (car x)) 
                               (if (= 1 (modulo-n (truncate (recursive-divide n '(2))) 3)) (lambda (x)  (cadr x)) 
                                   (lambda (x)  (caddr x))))]
                  [reg-op2 (if (= 0 (modulo-n (truncate (recursive-divide n '(2 3))) 3)) (lambda (x)  (cadr x))
                               (if (= 1 (modulo-n (truncate (recursive-divide n '(2 3))) 3)) (lambda (x)  (caddr x))
                                   (lambda (x)  (cadddr x))))])
             (if (= 0 (modulo-n (truncate (recursive-divide n '(2 3 3))) 2))
                 (list (lambda(X) 
                         (if (equal? (reg-op1 X) (random-element L)) (random-element L) (random-element L)))) 
                 (list (lambda(X)
                         (if 
                          (logic-operator (list (equal? (reg-op1 X) (random-element L)) (equal? (reg-op2 X) (random-element L)))) (random-element L) (random-element L)))))
         )
    )
)
|#

(define makeRules
    (lambda(L n) 
         (letrec (
                  [A (random-element L)];these shouldn't be random
                  [B (random-element L)]
                  [C (random-element L)]
                  [D (random-element L)]
                  [E (random-element L)]
                  [F (random-element L)]
                  [G (random-element L)]
                  [logic-operator (if (= 0 (modulo-n n 2)) (lambda (x)  (recursive-or x))                        ; mod 2 = 0
                                                           (lambda (x)  (recursive-and x)))]                     ; mod 2 = 1
                  [reg-op1 (if (= 0 (modulo-n (truncate (recursive-divide n '(2))) 3)) (lambda (x)  (car x))     ; mod 3 = 0
                           (if (= 1 (modulo-n (truncate (recursive-divide n '(2))) 3)) (lambda (x)  (cadr x))    ; mod 3 = 1
                           (lambda (x)  (caddr x))))]                                                            ; mod 3 = 2
                  [reg-op2 (if (= 0 (modulo-n (truncate (recursive-divide n '(2 3))) 3)) (lambda (x)  (cadr x))  ; mod 3 = 0
                           (if (= 1 (modulo-n (truncate (recursive-divide n '(2 3))) 3)) (lambda (x)  (caddr x)) ; mod 3 = 1
                           (lambda (x)  (cadddr x))))])                                                          ; mod 3 = 2
             (if (= 0 (modulo-n (truncate (recursive-divide n '(2 3 3))) 2))
                 (list (lambda(X) 
                         (if (equal? (reg-op1 X) A) B C)))                                                       ; mod 2 = 0
                 (list (lambda(X)
                         (if 
                          (logic-operator (list (equal? (reg-op1 X) D) (equal? (reg-op2 X) E))) F G))))          ; mod 2 = 1
         )
    )
)

;(define data (list 'a 'b 'c 'd 'e 'q))
(define data (list 'a 'b 'a 'b 'a 'b))

(define makeRulesRepeat
    (lambda(num data)
        (if  (eq? num 0) '() (cons (makeRules data num) (makeRulesRepeat (- num 1) data)))
    ) 
)

(define rules (flatten (makeRulesRepeat 1000 data)))

(define rules-n (pick-n-rand-rules 4 rules))

(define observedData (last (patternBuild-repeat-n 6 rules-n data)))

;(length observedData)
;(list observedData)

;(append (list ((car rules-n) data) data))

#|

(define makeRandomRule
    (lambda(L n) 
         (letrec ([logic-operator (if (= 0 (modulo-n n 2)) (lambda (x)  (recursive-or x)) (lambda (x)  (recursive-and x)))]
                  [reg-op1 (if (= 0 (modulo-n (truncate (recursive-divide n '(2))) 3)) (lambda (x)  (car x)) 
                               (if (= 1 (modulo-n (truncate (recursive-divide n '(2))) 3)) (lambda (x)  (cadr x)) 
                                   (lambda (x)  (caddr x))))]
                  [reg-op2 (if (= 0 (modulo-n (truncate (recursive-divide n '(2 3))) 3)) (lambda (x)  (cadr x))
                               (if (= 1 (modulo-n (truncate (recursive-divide n '(2 3))) 3)) (lambda (x)  (caddr x))
                                   (lambda (x)  (cadddr x))))])
             (if (= 0 (modulo-n (truncate (recursive-divide n '(2 3 3))) 2))
                 (list (lambda(X) 
                         (if (equal? (reg-op1 X) (random-element L)) (random-element L) (random-element L)))) 
                 (list (lambda(X)
                         (if 
                          (logic-operator (list (equal? (reg-op1 X) (random-element L)) (equal? (reg-op2 X) (random-element L)))) (random-element L) (random-element L)))))
         )
    )
)|#

(define makeRandomRule
    (lambda(L n) 
         (letrec (
                  [A (random-element L)]
                  [B (random-element L)]
                  [C (random-element L)]
                  [D (random-element L)]
                  [E (random-element L)]
                  [F (random-element L)]
                  [G (random-element L)]
                  [logic-operator (if (= 0 (modulo-n n 2)) (lambda (x)  (recursive-or x))                        ; mod 2 = 0
                                                           (lambda (x)  (recursive-and x)))]                     ; mod 2 = 1
                  [reg-op1 (if (= 0 (modulo-n (truncate (recursive-divide n '(2))) 3)) (lambda (x)  (car x))     ; mod 3 = 0
                           (if (= 1 (modulo-n (truncate (recursive-divide n '(2))) 3)) (lambda (x)  (cadr x))    ; mod 3 = 1
                           (lambda (x)  (caddr x))))]                                                            ; mod 3 = 2
                  [reg-op2 (if (= 0 (modulo-n (truncate (recursive-divide n '(2 3))) 3)) (lambda (x)  (cadr x))  ; mod 3 = 0
                           (if (= 1 (modulo-n (truncate (recursive-divide n '(2 3))) 3)) (lambda (x)  (caddr x)) ; mod 3 = 1
                           (lambda (x)  (cadddr x))))])                                                          ; mod 3 = 2
             (if (= 0 (modulo-n (truncate (recursive-divide n '(2 3 3))) 2))
                 (list (lambda(X) 
                         (if (equal? (reg-op1 X) A) B C)))                                                       ; mod 2 = 0
                 (list (lambda(X)
                         (if 
                          (logic-operator (list (equal? (reg-op1 X) D) (equal? (reg-op2 X) E))) F G))))          ; mod 2 = 1
         )
    )
)

(define func-list-build 
    (lambda(num obs)
        (if  (eq? num 0) '() (append (makeRandomRule obs num) (func-list-build (- num 1) obs) )
        )
    )
)


(define func-list-build-from-list 
    (lambda(obs L)
        (if  (null? L) '() (append (makeRandomRule obs (car L)) (func-list-build-from-list  obs (cdr L)) )
        )
    )
)

(define rules-list (func-list-build 36 observedData))

;(patternBuild-repeat-n 20 rules-list data)

;(list observedData 'break (last (patternBuild-repeat-n 20 (pick-n-rand-rules 20 rules-list) data)))

(define samples
  (mh-query
     30000 10

     (equal? observedData (last (patternBuild-repeat-n 6 (pick-n-rand-rules 5 rules-list) data)))

     #t
   )
)

(occurences samples)	

;(list rules-list)

;(eq? (length observedData) (length (flatten (patternBuild rules-list data))))

