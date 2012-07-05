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

(define (random-element list)
  (list-ref list (random (length list))))

;define the constraints of these rules.
(define rules (list 
                   (lambda(curr) (if (eq? (first curr) 'a) 'b '())) ;1
                   (lambda(curr) (if (> (length curr) 3) (if (and (eq? (first curr) 'c) (eq? (second curr) 'a) (eq? (third curr) 'b)) 'e '()) '()));2
                   (lambda(curr) (if (eq? (car curr) 'b) 'a '()));3
                   (lambda(curr) (if (> (length curr) 3) (if (and (eq? (first curr) 'c) (eq? (second curr) 'a) (eq? (third curr) 'b)) 'e '()) '()));4
                   (lambda(curr) (if (and (eq? (car curr) 'a) (eq? (cadr curr) 'b)) 'c '()));5
                   (lambda(curr) (if (> (length curr) 3) (if (and (eq? (first curr) 'c) (eq? (second curr) 'a) (eq? (third curr) 'b)) 'e '()) '()));6
                   (lambda(curr) (if (and (eq? (car curr) 'e) (eq? (cadr curr) 'c)) 'q '()));7
                   (lambda(curr) (if (and (eq? (first curr) 'c) (eq? (last curr) 'b)) 'a '()));8
                   (lambda(curr) (if (and (eq? (car curr) 'q) (eq? (cadr curr) 'e)) (random-element curr) '()));9
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

;observedData

(define makeHyps
   (lambda(obs)
      (cond ( (null? obs) '() )
            (#t (cons (lambda(curr) (if (equal? curr (car obs)) (list (if (equal? (cdr obs) '()) '(X) (cadr obs))) '(X))) (makeHyps (cdr obs))))
      )
   )
)

(define some-hyps (makeHyps observedData))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
                 (list (lambda(obs) (if (equal? (reg-op1 obs) ante1) conse1 failure))
                 (list 'lambda '(obs) (list 'if (list 'equal? (list reg-op1 'obs) ante1) conse1 failure)))
                 (list (lambda(obs) (if (logic-operator (equal? (reg-op1 obs) ante1) (equal? (reg-op2 obs) ante2)) conse1 failure))
                 (list 'lambda '(obs) (list 'if (list logic-operator (list 'equal? (list reg-op1 'obs) ante1) (list 'equal? (list reg-op2 'obs) ante2)) conse1 failure))))
         )
    )
)

(define func-list-build 
    (lambda(len obs)
        (cond 
            ((eq? len 0) '())
            (#t (append (makeRandomRuleLogic obs) (func-list-build (- len 1) obs) ))
        )
    ) 
)

;(define t (makeRandomRuleLogic observedData))
;(define u (makeRandomRuleLogic observedData))

;(list t)
;(list u)

;((car t) (list 'c 'a))
;((car u) (list 'c 'a))

;(define func-list (func-list-build 10 observedData))
(func-list-build 100 observedData)

;((car func-list) observedData)

;(let ([logic-operator (if (> 0.5 (random-real)) (lambda (x y)  (or x y)) (lambda (x y)  (and x y)))])
;          (list (logic-operator #t #f))
;)
;(if (or (equal? (car observedData) 'c) (equal? (cadr observedData) 'q)) 'b 'b)

