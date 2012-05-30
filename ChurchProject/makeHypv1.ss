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

(define particularValue 
    (lambda(L char)
        (cond 
            ( (or (null? L) (null? char))         '() )
            ( (eq? (car (car L))  char) (cdr (car L)) )
            ( else     (particularValue (cdr L) char) )           
        )
    ) 
)

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

(define observedData (patternBuild 100))

(define makeHyps
   (lambda(obs)
      (cond ( (null? obs) '() )
            (#t (cons
                    (lambda(curr) (if (equal? curr (car obs)) 
                                      (list 
                                          (if (equal? (cdr obs) '()) 
                                              (if (flip) 'd 'e) 
                                              (cadr obs)
                                          )
                                      ) 
                                      (if (flip) 'd 'e)
                                  )
                    )
                    (makeHyps (cdr obs))
                )
            )
      )
   )
)

(define someHyps (makeHyps observedData)) ;generate hypotheses.

((car someHyps) 'c)
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
