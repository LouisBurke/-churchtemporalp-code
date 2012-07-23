
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

;works on bher
(define makeRules
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
(define data (list 'a 'b 'c 'd 'e 'q))

(define makeRulesRepeat
    (lambda(num data)
        (if  (eq? num 0) '() (cons (makeRules data num) (makeRulesRepeat (- num 1) data)))
    ) 
)

(define rules (flatten (makeRulesRepeat 50 data)))

(define patternBuildRepeat 
    (lambda(len data)
        (if (eq? len 0) '() (cons (last (patternBuild rules data)) (patternBuildRepeat (- len 1) (last (patternBuild rules data)))))
    ) 
)

(define patternBuild 
        (lambda (rules L) 
                 (if (null? rules) (quote ( ) )
                     (cons (remq '() (append (list ((car rules) L)) L)) (patternBuild (cdr rules) (remq '() (remq '() (append (list ((car rules) L)) L)))))
                 )
        )
)

;(patternBuildRepeat 100 data)
(define observedData (flatten (patternBuildRepeat 10 data)))

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
                         (if (equal? (reg-op1 X) (random-element L)) (random-element L) (random-element L)))
                       (list 
                        'lambda '(X) (list 
                            'if (list 'equal? (list reg-op1 'X) (random-element L)) (random-element L) (random-element L)))) 
                 (list (lambda(X)
                         (if 
                          (logic-operator (list (equal? (reg-op1 X) (random-element L)) (equal? (reg-op2 X) (random-element L)))) (random-element L) (random-element L)))
                       (list 'lambda '(X) 
                       (list 'if 
                             (list logic-operator 
                                   (list 'list 
                                         (list 'equal? 
                                               (list reg-op1 'X) (random-element L)) 
                                         (list 'equal? (list reg-op2 'X) (random-element L)))) (random-element L) (random-element L)))))
         )
    )
)

(define func-list-build 
    (lambda(num obs)
        (if  (eq? num 0) '() (append (makeRandomRule obs num) (func-list-build (- num 1) obs) )
        )
    )
)

;(define functions (func-list-build 1024 observedData))

;(define obs-length (length observedData))

(define patternBuildRepeat
    (lambda(len data)
        (if (eq? len 0) '() (cons (last (patternBuild rules data)) (patternBuildRepeat (- len 1) (last (patternBuild rules data)))))
    ) 
)

(define patternBuild 
        (lambda (rules L) 
                 (if (null? rules) (quote ( ) )
                     (cons (remq '() (append (list ((car rules) L)) L)) (patternBuild (cdr rules) (remq '() (remq '() (append (list ((car rules) L)) L)))))
                 )
        )
)

(define samples
  (mh-query
     10 10

     (define rules-list (func-list-build 27 observedData))

     rules-list
     
     (equal? observedData (flatten (patternBuildRepeat obs-length data)))
   )
)
(list samples "Fair coin?")



