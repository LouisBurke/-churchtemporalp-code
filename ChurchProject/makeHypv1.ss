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

;Church Approved Start
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define non-dec (lambda (n l) (if (= n l) (list l) (non-dec n (+ l 1)))))

(define (random-element list)
  (list-ref list (car (non-dec (round (* (random-real) (length list))) 1))));;This will cause a problem in Church.

;define the constraints of these rules. -> this should mimic rules hypothis generator
(define rules (list 
                   (lambda(curr) (if (eq? (first curr) 'a) 'b '())) ;1
                   (lambda(curr) (if (> (length curr) 3) (if (and (eq? (first curr) 'c) (eq? (second curr) 'a) (eq? (third curr) 'b)) 'e '()) '()));2
                   (lambda(curr) (if (eq? (car curr) 'b) 'a '()));3
                   (lambda(curr) (if (> (length curr) 3) (if (and (eq? (first curr) 'c) (eq? (second curr) 'a) (eq? (third curr) 'b)) 'e '()) '()));4
                   (lambda(curr) (if (and (eq? (car curr) 'a) (eq? (cadr curr) 'b)) 'c '()));5
                   (lambda(curr) (if (> (length curr) 3) (if (and (eq? (first curr) 'c) (eq? (second curr) 'a) (eq? (third curr) 'b)) 'e '()) '()));6
                   (lambda(curr) (if (and (eq? (car curr) 'e) (eq? (cadr curr) 'c)) 'q '()));7
                   (lambda(curr) (if (and (eq? (first curr) 'c) (eq? (last curr) 'b)) 'a '()));8
                   (lambda(curr) (if (and (eq? (car curr) 'q) (eq? (cadr curr) 'e)) (random-element curr) '()));;This will cause a problem in Church.
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

;(last (patternBuild rules data))
(define observedData (patternBuildRepeat 100 data))
;Church Approved End
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;observedData

;(define makeHyps
;   (lambda(obs)
;      (cond ( (null? obs) '() )
;            (#t (cons (lambda(curr) (if (equal? curr (car obs)) (list (if (equal? (cdr obs) '()) '(X) (cadr obs))) '(X))) (makeHyps (cdr obs))))
;      )
;   )
;)

;(define some-hyps (makeHyps observedData))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;conbinatorics no same rules twice
(define makeRandomRuleLogic
    (lambda(obs) 
         (let ([ante1 (if (> 0.5 (random-real)) (random-element obs) (random-element obs))]
               [ante2 (if (> 0.5 (random-real)) (random-element obs) (random-element obs))]
               [conse1 (if (> 0.5 (random-real)) (random-element obs) (random-element obs))]
               [conse2 (if (> 0.5 (random-real)) (random-element obs) (random-element obs))]
               [failure (random-element obs)]
               [logic-operator (if (> 0.5 (random-real)) (lambda (x y)  (or x y)) (lambda (x y)  (and x y)))]
               [reg-op1 (cond
                            [(> 0.5 (random-real)) (lambda (x)  (car x))]
                            [(> 0.5 (random-real)) (lambda (x)  (cadr x))]
                            [(> 0.5 (random-real)) (lambda (x)  (caddr x))]
                            [ #t (lambda (x)  (car x))])]
               [reg-op2 (cond
                            [(> 0.5 (random-real)) (lambda (x)  (car x))]
                            [(> 0.5 (random-real)) (lambda (x)  (cdr x))]
                            [(> 0.5 (random-real)) (lambda (x)  (cddr x))]
                            [ #t (lambda (x)  (car x))])])
             (if (> 0.5 (random-real)) 
                 (list (lambda(obs) (if (equal? (reg-op1 obs) ante1) conse1 failure));think recursively.
                 (list 'lambda '(obs) (list 'if (list 'equal? (list reg-op1 'obs) ante1) conse1 failure)))
                 (list (lambda(obs) (if (logic-operator (equal? (reg-op1 obs) ante1) (equal? (reg-op2 obs) ante2)) conse1 failure))
                 (list 'lambda '(obs) (list 'if (list logic-operator (list 'equal? (list reg-op1 'obs) ante1) (list 'equal? (list reg-op2 'obs) ante2)) conse1 failure))))
         )
    )
)

;1) make sure this all runs in church 2) mh-query 
(define func-list-build 
    (lambda(len obs)
        (cond 
            ((eq? len 0) '())
            (#t (append (makeRandomRuleLogic obs) (func-list-build (- len 1) obs) ))
        )
    ) 
)

;Church Approved Start
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(* (- (/ obs 2.0) (floor (/ obs 2.0))) 2)
(define bit-list 
    (lambda(obs n) (if (eq? obs 0) '() (cons  (list n (* (- (/ obs 2) (floor (/ obs 2))) 2)) (bit-list (truncate (/ obs 2)) (+ n 1)))))
)

(define check-bit
    (lambda(obs n) (if (eq? (caar obs) n) (if (eq? (cadar obs) 1) #t #f) (check-bit (cdr obs) n)))
)
(bit-list 500 1)
(check-bit (bit-list 500 1) 6)

;Church Approved End
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define makeRandomRule
    (lambda(L n) 
         (let ([ante1 (random-element L)]
               [ante2 (random-element L)]
               [conse1 (random-element L)]
               [conse2 (random-element L)]
               [failure (random-element L)]
               [logic-operator (cond
                                   [(check-bit (bit-list n 1) 1) (lambda (x y)  (or x y))]
                                   [(check-bit (bit-list n 1) 2) (lambda (x y)  (and x y))]
                                   [(check-bit (bit-list n 1) 3) (lambda (x y)  (not x y))])]
               [reg-op1 (cond
                            [(check-bit (bit-list n 1) 4) (lambda (x)  (car x))]
                            [(check-bit (bit-list n 1) 5) (lambda (x)  (cadr x))]
                            [(check-bit (bit-list n 1) 6) (lambda (x)  (caddr x))]
                            [ #t (lambda (x)  (car x))])]
               [reg-op2 (cond
                            [(check-bit (bit-list n 1) 7) (lambda (x)  (car x))]
                            [(check-bit (bit-list n 1) 8) (lambda (x)  (cdr x))]
                            [(check-bit (bit-list n 1) 9) (lambda (x)  (cddr x))]
                            [ #t (lambda (x)  (car x))])])
             (if (> 0.5 (random-real)) 
                 (list (lambda(L) (if (equal? (reg-op1 L) ante1) conse1 failure));think recursively.
                 (list 'lambda '(L) (list 'if (list 'equal? (list reg-op1 'L) ante1) conse1 failure)))
                 (list (lambda(L) (if (logic-operator (equal? (reg-op1 L) ante1) (equal? (reg-op2 L) ante2)) conse1 failure))
                 (list 'lambda '(L) (list 'if (list logic-operator (list 'equal? (list reg-op1 'L) ante1) (list 'equal? (list reg-op2 'L) ante2)) conse1 failure))))
         )
    )
)

(makeRandomRule observedData 1500)
(makeRandomRule observedData 1000)
(makeRandomRule observedData 2000)