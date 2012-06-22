#lang scheme
;(random-seed 100)

(define coin-flip
    (lambda()
        (random)
    )  
)

(define (random-element list)
  (list-ref list (random (length list))))

(define rules (list 
                   (lambda(curr) (if (eq? (first curr) 'a) 'b '())) 
                   (lambda(curr) (if (> (length curr) 3) (if (and (eq? (first curr) 'c) (eq? (second curr) 'a) (eq? (third curr) 'b)) 'e '()) '()))
                   (lambda(curr) (if (eq? (car curr) 'b) 'a '()))
                   (lambda(curr) (if (> (length curr) 3) (if (and (eq? (first curr) 'c) (eq? (second curr) 'a) (eq? (third curr) 'b)) 'e '()) '()))
                   (lambda(curr) (if (and (eq? (car curr) 'a) (eq? (cadr curr) 'b)) 'c '()))
                   (lambda(curr) (if (> (length curr) 3) (if (and (eq? (first curr) 'c) (eq? (second curr) 'a) (eq? (third curr) 'b)) 'e '()) '()))
                   (lambda(curr) (if (and (eq? (car curr) 'e) (eq? (cadr curr) 'c)) 'q '()))
                   ;(lambda(curr) (if (and (eq? (car curr) 'q) (eq? (cadr curr) 'e)) (random-element curr) '()))
              )
)

(define patternBuildRepeat 
    (lambda(len data)
        (cond 
            ((eq? len 0) '())
            (#t (append (last (patternBuild rules data)) (patternBuildRepeat (- len 1) (last (patternBuild rules data))) 
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

(define data (list 'b))
;(define test (list 'q 'e ))
;(last (patternBuild rules data))

(patternBuildRepeat 5 data)

(define coin-flip-n
    (lambda(n)
        (if (eq? n 0) (quote ( ) )
            (cons (coin-flip) (coin-flip-n (- n 1)))
        )
    )  
)

;(coin-flip-n 10)