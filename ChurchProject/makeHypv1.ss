#lang scheme
(define observedData '(a e a e a e a e))
(define testList '())

(define makeHyps
   (lambda(obs)
      (cond ( (null? obs) '() )
            (#t (cons
                    (lambda(curr) (if (equal? curr (car obs)) 
                                      (list 
                                          (if (equal? (cdr obs) '()) 
                                              'X 
                                              (cadr obs)
                                          )
                                      ) 
                                      'X
                                  )
                    )
                    (makeHyps (cdr obs))
                )
            )
      )
   )
)

(define someHyps (makeHyps observedData)) ;generate hypotheses.

(define eachHyps
   (lambda(data hypoth)
       (cond ((null? data) '()) 
             (#t (cons
                      (hypoth         (car data))
                      (eachHyps (cdr data) hypoth)
                 )
             )
       )
   )
) ;run through each observation.

(define tryHyps
   (lambda(data hypothilist)
       (cond ((null? hypothilist) '()) 
             (#t (append (eachHyps data (car hypothilist)) '(%) (tryHyps data (cdr hypothilist))))
       )
   )
)

(tryHyps observedData someHyps) ;try hypotheses.

;(length someHyps)
;(length observedData)
;(eachHyps observedData (car someHyps))
;(eachHyps observedData (car (cdr someHyps)))
;(eachHyps observedData (car (cdr (cdr someHyps))))
;(eachHyps observedData (car (cdr (cdr (cdr someHyps)))))
;(eachHyps observedData (car (cdr (cdr (cdr (cdr someHyps))))))
;(eachHyps observedData (car (cdr (cdr (cdr (cdr (cdr someHyps)))))))