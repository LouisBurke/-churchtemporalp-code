#lang scheme
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
                (if  (null? L) (quote ( ) )
                     (cons (list (car L)        (count (car L) L) )
                            (occurences (rember (car L) (cdr L) ) )
                     )
                )    
        )
)

;Build a pattern repeated len times.
(define patternBuild 
    (lambda(len)
        (cond 
            ((eq? len 0) '())
            (#t (append (list 'a 'b 'c (if (flip) 'd 'e)) (patternBuild (- len 1)) 
                )
            )             
        )
    ) 
)

(define particularValue 
    (lambda(L char)
        (cond 
            ( (or (null? L) (null? char))         '() )
            ( (eq? (car (car L))  char) (cdr (car L)) )
            ( else     (particularValue (cdr L) char) )           
        )
    ) 
)

(define observedData (patternBuild 100))

(define E (particularValue (occurences observedData) 'e)) ;Get the number of a particular value
(define D (particularValue (occurences observedData) 'd)) ;in the observed data. 

;(if (eq? (apply > (append E D)) #t) (list 'e) (list 'd))

(define makeHyps
   (lambda(obs)
      (cond ( (null? obs) '() )
            (#t (cons
                    (lambda(curr) (if (equal? curr (car obs)) 
                                      (list 
                                          (if (equal? (cdr obs) '()) 
                                              ;if the hypothesis doesn't know then pick the most 
                                              ;frequent of the two observations
                                              (if (eq? (apply > (append E D)) #t) (list 'e) (list 'd)) 
                                              (cadr obs)
                                          )
                                      ) 
                                      (if (eq? (apply > (append E D)) #t) (list 'e) (list 'd))
                                  )
                    )
                    (makeHyps (cdr obs))
                )
            )
      )
   )
)

(define someHyps (makeHyps observedData)) ;generate hypotheses.

;((car someHyps) 'c)

;(define samples
;    (mh-query 100 100
   
;        (equal? 'e ((car someHyps) 'c))

;        (equal? 'c ((car someHyps) 'b))
;    )
;)

;(hist samples)

;(define eachHyps
;   (lambda(data hypoth)
;       (cond ((null? data) '()) 
;             (#t (cons
;                      (hypoth         (car data))
;                      (eachHyps (cdr data) hypoth)
;                 )
;             )
;       )
;   )
;) ;run through each observation.

;(define tryHyps
;   (lambda(data hypothilist)
;       (cond ((null? hypothilist) '()) 
;             (#t (append (eachHyps data (car hypothilist)) '(%) (tryHyps data (cdr hypothilist))))
;       )
;   )
;)

;(tryHyps observedData someHyps) ;try hypotheses.
