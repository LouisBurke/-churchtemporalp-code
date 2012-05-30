#lang scheme

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
