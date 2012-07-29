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

(define modulo-n 
    (lambda(num numerate) (* (- (/ num numerate) (floor (/ num numerate))) numerate))
)

(define denom (list 2 3 3 2))

(define cols 
    (lambda (num L)
        (if (null? L) '()
            (cons (modulo-n num (car L)) (cols (floor (/ num (car L))) (cdr L)))
        )
    )
)

(define rows 
    (lambda (num_row L)
        (if (= 0 num_row) '()
            (cons (cols num_row L) (rows (- num_row 1) L))
        )
    )
)

(occurences (rows 36 denom))
