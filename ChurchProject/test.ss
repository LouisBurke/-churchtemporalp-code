#lang scheme
(require (planet williams/science/random-source))
;(random-seed 100)

(define coin-flip
    (lambda()
        (random 0.1)
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
            ;(#t (append (last (patternBuild rules data)) (patternBuildRepeat (- len 1) (last (patternBuild rules data)))))  
            (#t (let ([x (last(patternBuild rules data))] ) (append x (patternBuildRepeat (- len 1) x))))
        )
    ) 
)
(define data (list 'a 'b))
(define procedure (make-vector 1))

(define patternBuild 
        (lambda (rules L) 
                 (cond ( (null? rules) (quote ( ) ) )
                       ( #t (vector-set! procedure 0 (list ((car rules) L))) 
                            (cons (remq '() (append (flatten (vector->list procedure)) L)) (patternBuild (cdr rules) (remq '() (append (flatten (vector->list procedure)) L)))
                            )
                       )
                 )

        )
)

(last (patternBuild rules data))
(patternBuildRepeat 5 data)


