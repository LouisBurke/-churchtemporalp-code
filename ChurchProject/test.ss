#lang scheme

(define rules (list 
                   (lambda(curr) (if (eq? (first curr) 'a) 'b '())) 
                   (lambda(curr) (if (> (length curr) 3) (if (and (eq? (first curr) 'c) (eq? (second curr) 'a) (eq? (third curr) 'b)) 'e '()) '()))
                   (lambda(curr) (if (eq? (car curr) 'b) 'a null))
                   (lambda(curr) (if (> (length curr) 3) (if (and (eq? (first curr) 'c) (eq? (second curr) 'a) (eq? (third curr) 'b)) 'e '()) '()))
                   (lambda(curr) (if (and (eq? (car curr) 'a) (eq? (cadr curr) 'b)) 'c '()))
                   (lambda(curr) (if (> (length curr) 3) (if (and (eq? (first curr) 'c) (eq? (second curr) 'a) (eq? (third curr) 'b)) 'e '()) '()))
              )
)


(define patternBuildRepeat 
    (lambda(len)
        (cond 
            ((eq? len 0) '())
            (#t (append (last (patternBuild rules data)) (patternBuildRepeat (- len 1)) 
                )
            )             
        )
    ) 
)

(define patternBuild 
        (lambda (rules L) 
                (if  (null? rules) (quote ( ) )
                     (cons (remq '() (append (list ((car rules) L)) L))
                           (patternBuild (cdr rules) (remq '() (append (list ((car rules) L)) L)))
                     )
                )    
        )
)

(define data (list 'a 'b 'c ))

(last (patternBuild rules data))

(patternBuildRepeat 8)