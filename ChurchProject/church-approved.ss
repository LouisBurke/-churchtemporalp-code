#lang scheme
(require (planet williams/science/random-source))

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

(define non-dec (lambda (n l) (if (= n l) (list l) (non-dec n (+ l 1)))))

(define (random-element list)
  (list-ref list (car (non-dec (round (* (random-real) (length list))) 1))))

(define bit-list 
    (lambda(obs n) (if (eq? obs 0) '() (cons  (list n (* (- (/ obs 2) (floor (/ obs 2))) 2)) (bit-list (truncate (/ obs 2)) (+ n 1)))))
)

(define modulo-n 
    (lambda(num numerate) (* (- (/ num numerate) (floor (/ num numerate))) numerate))
)

(define check-bit
    (lambda(obs n) (if (eq? (caar obs) n) (if (eq? (cadar obs) 1) #t #f) (check-bit (cdr obs) n)))
)

;define the constraints of these rules. -> this should mimic rules hypothis generator
#|(define rules (list 
                   (lambda(curr) (if (eq? (first curr) 'a) 'b '())) ;1
                   (lambda(curr) (if (> (length curr) 3) (if (and (eq? (first curr) 'c) (eq? (second curr) 'a) (eq? (third curr) 'b)) 'e '()) '()));2
                   (lambda(curr) (if (eq? (car curr) 'b) 'a '()));3
                   (lambda(curr) (if (> (length curr) 3) (if (and (eq? (first curr) 'c) (eq? (second curr) 'a) (eq? (third curr) 'b)) 'e '()) '()));4
                   (lambda(curr) (if (and (eq? (car curr) 'a) (eq? (cadr curr) 'b)) 'c '()));5
                   (lambda(curr) (if (> (length curr) 3) (if (and (eq? (first curr) 'c) (eq? (second curr) 'a) (eq? (third curr) 'b)) 'e '()) '()));6
                   (lambda(curr) (if (and (eq? (car curr) 'e) (eq? (cadr curr) 'c)) 'q '()));7
                   (lambda(curr) (if (and (eq? (first curr) 'c) (eq? (last curr) 'b)) 'a '()));8
                   (lambda(curr) (if (and (eq? (car curr) 'q) (eq? (cadr curr) 'e)) (random-element curr) '()))
              )
)|#

(define makeRules
    (lambda(L n) 
         (let (;[ante1 (random-element L)]
               ;[ante2 (random-element L)]
               ;[conse1 (random-element L)]
               ;[conse2 (random-element L)]
               ;[failure (random-element L)]
               [div 3]
               [logic-operator (cond
                                   [(modulo-n n 2) (lambda X (apply or X))]; needs some thought. Modulo concept. For 3 exclusive choices.
                                   [(modulo-n n 2) (lambda X (apply and X))])]
                                   ;[(modulo-n n 3) (lambda (x y)  (not x y))])];not x y
               [reg-op1 (cond
                            [(modulo-n n (recursive-divide n '(2))) (lambda (x)  (car x))];result of division
                            [(modulo-n n (recursive-divide n '(2))) (lambda (x)  (cadr x))]
                            [(modulo-n n (recursive-divide n '(2))) (lambda (x)  (caddr x))])]
               [reg-op2 (cond
                            [(modulo-n n (recursive-divide n '(2 3))) (lambda (x)  (car x))]
                            [(modulo-n n (recursive-divide n '(2 3))) (lambda (x)  (cdr x))]
                            [(modulo-n n (recursive-divide n '(2 3))) (lambda (x)  (cddr x))])])
             (if (modulo-n n (recursive-divide n '(2 3 1))) 
                 (list (lambda(L) (if (equal? (reg-op1 L) (random-element L)) conse1 failure));think recursively.
                 (list 'lambda '(L) (list 'if (list 'equal? (list reg-op1 'L) ante1) conse1 failure)))
                 (list (lambda(L) (if (logic-operator (equal? (reg-op1 L) ante1) (equal? (reg-op2 L) ante2)) conse1 failure))
                 (list 'lambda '(L) (list 'if (list logic-operator (list 'equal? (list reg-op1 'L) ante1) (list 'equal? (list reg-op2 'L) ante2)) conse1 failure))))
         )
    )
)


(define patternBuildRepeat 
    (lambda(len data)
        (cond 
            ((eq? len 0) '())
            (#t (let ([x (last(patternBuild rules data))] ) (append x (patternBuildRepeat (- len 1) x))))
        )
    ) 
)

(define data (list 'a 'b))

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

(define observedData (patternBuildRepeat 100 data))

(define recursive-divide (lambda (n L) (if (null? L ) (/ n 1) (/ (recursive-divide n (cdr L)) (car L)))))

(define makeRandomRule
    (lambda(L n) 
         (let ([ante1 (random-element L)]
               [ante2 (random-element L)]
               [conse1 (random-element L)]
               [conse2 (random-element L)]
               [failure (random-element L)]
               [div 3]
               [logic-operator (cond
                                   [(modulo-n n 3) (lambda (x y)  (or x y))]; needs some thought. Modulo concept. For 3 exclusive choices.
                                   [(modulo-n n 3) (lambda (x y)  (and x y))]
                                   [(modulo-n n 3) (lambda (x y)  (not x y))])];not x y
               [reg-op1 (cond
                            [(modulo-n n (recursive-divide n '(3))) (lambda (x)  (car x))];result of division
                            [(modulo-n n (recursive-divide n '(3))) (lambda (x)  (cadr x))]
                            [(modulo-n n (recursive-divide n '(3))) (lambda (x)  (caddr x))])]
               [reg-op2 (cond
                            [(modulo-n n (recursive-divide n '(3 3))) (lambda (x)  (car x))]
                            [(modulo-n n (recursive-divide n '(3 3))) (lambda (x)  (cdr x))]
                            [(modulo-n n (recursive-divide n '(3 3))) (lambda (x)  (cddr x))])])
             (if (modulo-n n (recursive-divide n '(3 3 1))) 
                 (list (lambda(L) (if (equal? (reg-op1 L) ante1) conse1 failure));think recursively.
                 (list 'lambda '(L) (list 'if (list 'equal? (list reg-op1 'L) ante1) conse1 failure)))
                 (list (lambda(L) (if (logic-operator (equal? (reg-op1 L) ante1) (equal? (reg-op2 L) ante2)) conse1 failure))
                 (list 'lambda '(L) (list 'if (list logic-operator (list 'equal? (list reg-op1 'L) ante1) (list 'equal? (list reg-op2 'L) ante2)) conse1 failure))))
         )
    )
)

(define func-list-build 
    (lambda(len obs num)
        (cond 
            ((eq? len 0) '())
            (#t (append (makeRandomRule obs num) (func-list-build (- len 1) obs num) ))
        )
    ) 
)

(func-list-build 2 observedData 1024)
