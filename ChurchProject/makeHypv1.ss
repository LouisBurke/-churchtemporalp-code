#lang scheme
(require (planet williams/science/random-source))

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
                (if  (null? L) (quote ( ) )
                     (cons (list (car L)        (count (car L) L) )
                            (occurences (remq (car L) (cdr L) ) )
                     )
                )    
        )
)

(define (random-element list)
  (list-ref list (random (length list))))

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
(define observedData (patternBuildRepeat 5 data))

;observedData

(define makeHyps
   (lambda(obs)
      (cond ( (null? obs) '() )
            (#t (cons (lambda(curr) (if (equal? curr (car obs)) (list (if (equal? (cdr obs) '()) '(X) (cadr obs))) '(X))) (makeHyps (cdr obs))))
      )
   )
)

(define some-hyps (makeHyps observedData))

(define test (list 'c 'q 'e 'c 'a 'q 'b 'a 'q 'b))

(define index-func (let ((count 0)) (lambda (val obs) 
    (cond ( (null? obs) (quote ( ) ) )
            ( #t (set! count (+ count 1)) (if (eq? val (car obs)) (cons count (index-func val (cdr obs))) (index-func val (cdr obs))))
    )
))
)
  
(index-func 'a observedData)
