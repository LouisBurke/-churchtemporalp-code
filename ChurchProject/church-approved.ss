#lang scheme
(require (planet williams/science/random-source))
;works on bher
(define non-dec (lambda (n l) (if (= n l) (list l) (non-dec n (+ l 1)))))

;works on bher
(define (random-element list)
  (list-ref list (car (non-dec (floor (* (random-real) (length list))) 0))))

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

;bher problem
(define makeRules
    (lambda(L n) 
         (let ([logic-operator (cond;cond is disappointing
                                   [(= 0 (modulo-n n 2)) (lambda (x)  (recursive-or x))]
                                   [(= 1 (modulo-n n 2)) (lambda (x)  (recursive-and x))])]
               [reg-op1 (cond
                            [(= 0 (modulo-n (truncate (recursive-divide n '(2))) 3)) (lambda (x)  (car x))]
                            [(= 1 (modulo-n (truncate (recursive-divide n '(2))) 3)) (lambda (x)  (cadr x))]
                            [(= 2 (modulo-n (truncate (recursive-divide n '(2))) 3)) (lambda (x)  (caddr x))])]
               [reg-op2 (cond
                            [(= 0 (modulo-n (truncate (recursive-divide n '(2 3))) 3)) (lambda (x)  (cadr x))]
                            [(= 1 (modulo-n (truncate (recursive-divide n '(2 3))) 3)) (lambda (x)  (caddr x))]
                            [(= 2 (modulo-n (truncate (recursive-divide n '(2 3))) 3)) (lambda (x)  (cadddr x))])])
             (cond [(= 0 (modulo-n (truncate (recursive-divide n '(2 3 3))) 2))
                 (list (lambda(L) 
                         (if (equal? (reg-op1 L) (random-element L)) (random-element L) (random-element L))))] 
                   [(= 1 (modulo-n (truncate (recursive-divide n '(2 3 3))) 2)) 
                 (list (lambda(L) ;L is inappropriate
                         (if 
                          (logic-operator (list (equal? (reg-op1 L) (random-element L)) (equal? (reg-op2 L) (random-element L)))) (random-element L) (random-element L))))])
         )
    )
)
(define data (list 'a 'b 'c 'd 'e 'q))

(define makeRulesRepeat 
    (lambda(num data)
        (cond 
            ((eq? num 0) '())
            (#t (let ([x (last (makeRules data num))] ) (list x (makeRulesRepeat (- num 1) data))))
        )
    ) 
)

(define rules (flatten (makeRulesRepeat 50 data)))

(define patternBuildRepeat 
    (lambda(len data)
        (cond 
            ((eq? len 0) '())
            (#t (let ([x (last(patternBuild rules data))] ) (append x (patternBuildRepeat (- len 1) x))))
        )
    ) 
)

(define patternBuild 
        (lambda (rules L) 
                 (cond ( (null? rules) (quote ( ) ) )
                       ( #t (let ([x (list ((car rules) L))])
                            (cons (remq '() (append x L)) (patternBuild (cdr rules) (remq '() (append x L)))
                            ))
                       )
                 )

        )
)

;(patternBuildRepeat 100 data)
(define observedData (patternBuildRepeat 100 data))

(define makeRandomRule
    (lambda(L n) 
         (let ([logic-operator (cond
                                   [(= 0 (modulo-n n 2)) (lambda (x)  (recursive-or x))]
                                   [(= 1 (modulo-n n 2)) (lambda (x)  (recursive-and x))])]
               [reg-op1 (cond
                            [(= 0 (modulo-n (truncate (recursive-divide n '(2))) 3)) (lambda (x)  (car x))]
                            [(= 1 (modulo-n (truncate (recursive-divide n '(2))) 3)) (lambda (x)  (cadr x))]
                            [(= 2 (modulo-n (truncate (recursive-divide n '(2))) 3)) (lambda (x)  (caddr x))])]
               [reg-op2 (cond
                            [(= 0 (modulo-n (truncate (recursive-divide n '(2 3))) 3)) (lambda (x)  (cdr x))]
                            [(= 1 (modulo-n (truncate (recursive-divide n '(2 3))) 3)) (lambda (x)  (cddr x))]
                            [(= 2 (modulo-n (truncate (recursive-divide n '(2 3))) 3)) (lambda (x)  (cdddr x))])])
             (cond [(= 0 (modulo-n (truncate (recursive-divide n '(2 3 3))) 2))
                 (list (lambda(L) 
                         (if (equal? (reg-op1 L) (random-element L)) (random-element L) (random-element L)))
                 (list 'lambda '(L) 
                       (list 'if 
                             (list 'equal? (list reg-op1 'L) (random-element L)) (random-element L) (random-element L))))] 
                   [(= 1 (modulo-n (truncate (recursive-divide n '(2 3 3))) 2)) 
                 (list (lambda(L) 
                         (if 
                          (logic-operator (list (equal? (reg-op1 L) (random-element L)) (equal? (reg-op2 L) (random-element L)))) (random-element L) (random-element L)))
                 (list 'lambda '(L) 
                       (list 'if 
                             (list logic-operator 
                                   (list 'list 
                                         (list 'equal? 
                                               (list reg-op1 'L) (random-element L)) 
                                         (list 'equal? (list reg-op2 'L) (random-element L)))) (random-element L) (random-element L))))])
         )
    )
)

(define func-list-build 
    (lambda(num obs)
        (cond 
            ((eq? num 0) '())
            (#t (append (makeRandomRule obs num) (func-list-build (- num 1) obs) ))
        )
    ) 
)

(func-list-build 1024 observedData)



