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

(define num-new-symbols 60)

(define non-dec (lambda (n l) (if (= n l) (list l) (non-dec n (+ l 1)))))


(define (random-element L)
  (list-ref L (car (non-dec (floor (* (random-real) (length L))) 0))))


(define modulo-n 
    (lambda(num numerate) (* (- (/ num numerate) (floor (/ num numerate))) numerate))
)

(define recursive-divide (lambda (n L) (if (null? L ) (/ n 1) (/ (recursive-divide n (cdr L)) (car L)))))


(define recursive-and (lambda (X) (if (null? X) #t (and (car X) (recursive-and (cdr X))))))


(define recursive-or (lambda (X) (if (null? X) #f (or (car X) (recursive-or (cdr X))))))

(define list-truth 
  (lambda (L1 L2) 
    (if (null? L1) '() 
      (cons (> (count (car L1) L2) 0) 
        (list-truth (cdr L1) L2)
      )
    )
  )
)

(define pos_rules
  (lambda (rules D) 
    (if (null? rules) '() 
      (cons ((car (pick-n-rand-rules 1 (strip-list-func rules))) D) (pos_rules (cdr rules) D))
    )
  )
) 

#|(define patternBuild-repeat-n 
  (lambda (len rules D)
    ;(letrec ([new_D (cons (random-element (pos_rules rules D)) D)])
    (letrec ([rand_rule (pick-n-rand-rules 1 rules)])
      (if (= (length (flatten new_D)) len) new_D
          (patternBuild-repeat-n len rules (flatten new_D))
      )
    )
  )
)|#

(define patternBuild-repeat-n 
  (lambda (len rules D)
    (letrec ([len_D (length (flatten D))])
        (if (= len len_D) (list )
        (letrec ([rand_rule (pick-n-rand-rules 1 rules)])
          (append (if (null? ((caar rand_rule) (flatten D))) '() rand_rule) 
                  (patternBuild-repeat-n len rules (append (list ((caar rand_rule) (flatten D))) (flatten D)))) 
        )
      )
    )
  )
)

(define pick-n-rand-rules 
        (lambda (n rules) 
                 (if (= 0 n) (quote ( ) )
                     (cons (random-element rules) (pick-n-rand-rules (- n 1) (cdr rules)))
                 )
        )
)


(define truth-index 
  (lambda (L n) 
    (if (eq? n (length L)) '() 
      (cons (if (eq? (list-ref L n) #t) n '()) (truth-index  L (+ n 1)))
    )
  )
)

(define expose_data 
  (lambda (L D) 
    (if (null? L) '() (cons (cons ((caar L) D) D) (expose_data (cdr L) (cons ((caar L) D) D))))
  )
)

(define expose_rule_desc 
  (lambda (L) 
    (if (null? L) '() (cons (cadar L) (expose_rule_desc (cdr L))))
  )
)

(define expose_rule_func 
  (lambda (L) 
    (if (null? L) '() (cons (caar L) (expose_rule_func (cdr L))))
  )
)

(define strip-list-func (lambda (L) (if (null? L) '() (cons (car L) (strip-list-func (cddr L))))) 
)

(define strip-list-desc (lambda (L) (if (null? L) '() (cons (cadr L) (strip-list-desc (cddr L))))) 
)

(define list-funcs 
  (lambda(ind L) 
    (if (null? ind) '() 
      (append (list (list-ref L (car ind)) #\newline) (list-funcs  (cdr ind) L))) 
  )
)

(define makeRules
    (lambda(L n) 
         (letrec (
                  [A (random-element L)]
                  [B (random-element L)]
                  [C '()]
                  [D (random-element L)]
                  [E (random-element L)]
                  [F (random-element L)]
                  [G '()]
                  [logic-operator (if (= 0 (modulo-n n 2)) (cons (lambda (x)  (recursive-or x)) (list 'recursive-or))      ; mod 2 = 0
                                                           (cons (lambda (x)  (recursive-and x))(list 'recursive-and)))]   ; mod 2 = 1
                  [reg-op1 (if (= 0 (modulo-n (truncate (recursive-divide n '(2))) 3)) 
                               (cons (lambda (x)  (car x)) (list (list 'car 'x)))                                                    ; mod 3 = 0 
                           (if (= 1 (modulo-n (truncate (recursive-divide n '(2))) 3)) 
                               (cons (lambda (x) (if (> (length x) 1) (cadr x) '())) 
                                     (list (list 'lambda '(x) (list 'if (list '> (list 'length 'x) 1) (list 'cadr 'x) '()))))
                               (cons (lambda (x) (if (> (length x) 2) (caddr x) '())) 
                                     (list (list 'lambda '(x) (list 'if (list '> (list 'length 'x) 2) (list 'caddr 'x) '()))))))] ; mod 3 = 2
                  [reg-op2 (if (= 0 (modulo-n (truncate (recursive-divide n '(2 3))) 3)) 
                               (cons (lambda (x) (if (> (length x) 1) (cadr x) '())) 
                                     (list (list 'lambda '(x) (list 'if (list '> (list 'length 'x) '1) (list 'cadr 'x) '()))))    ; mod 3 = 0
                           (if (= 1 (modulo-n (truncate (recursive-divide n '(2 3))) 3)) 
                               (cons (lambda (x) (if (> (length x) 2) (caddr x) '())) 
                                     (list (list 'lambda '(x) (list 'if (list '> (list 'length 'x) '2) (list 'caddr 'x) '()))))   ; mod 3 = 1
                               (cons (lambda (x) (if (> (length x) 3) (cadddr x) '())) 
                                     (list (list 'lambda '(x) (list 'if (list '> (list 'length 'x) '3) (list 'cadddr 'x) '()))))))])                                      ; mod 3 = 2
             (if (= 0 (modulo-n (truncate (recursive-divide n '(2 3 3))) 2))
                 (list (list (lambda(X) 
                         (if (equal? ((car reg-op1) X) A) B C)) 
                           (list 'lambda '(X) (list 'if (list 'equal? (list (cadr reg-op1) 'X) A) B C))))                                                                 ; mod 2 = 0
                 (list (list (lambda(X)
                         (if 
                          ((car logic-operator) (list (equal? ((car reg-op1) X) D) (equal? ((car reg-op2) X) E))) F G))
                 (list 'lambda '(X) (list 
                         'if 
                          (list (cadr logic-operator) (list 'equal? (list (cadr reg-op1) 'X) D) (list 'equal? (list (cadr reg-op2) 'X) E)) F G))))) ; mod 2 = 1
         )
    )
)

(define symbols (list 'a 'b 'c 'd 'e 'c 'q))

(define data (list 'a))

(define makeRulesRepeat
    (lambda(num symbols)
        (if  (eq? num 0) '() (append (makeRules symbols num) (makeRulesRepeat (- num 1) symbols)))
    ) 
)

(define rules (makeRulesRepeat 3600 symbols))

;(define data_n_rules (patternBuild-repeat-n num-new-symbols rules data))

;(define observedData (last (expose_data data_n_rules data)))

;observedData

;(expose_rule_func data_n_rules)

(define observedData (last (expose_data (patternBuild-repeat-n num-new-symbols rules data) data)))

observedData

(define samples
  (mh-query
     10000 10
     
     (define try (last (expose_data (patternBuild-repeat-n num-new-symbols rules data) data)))
     
     (equal? observedData try)

     #t
   )
)

#|

(define built-rules-list (func-list-build 108 observedData)) ;Build all possible rules (with random variables as args)

(define desc-list (strip-list-desc built-rules-list))
(define rules-list (strip-list-func built-rules-list))

(define rules5 (pick-n-rand-rules 5 rules-list))

(define samples
  (mh-query
     10000 10

     (equal? observedData (last (patternBuild-repeat-n 6 rules5 data)))

     #t
   )
)

(occurences samples)

(define occur (occurences samples))

(define truthval (if (equal? (car (car occur)) #t) (cadr (car occur)) (cadr (if (< (length occur) 2) '(#t 0) (cadr occur)))))

(define top (cadr (car occur)))
(define bot (if (< (length occur) 2) 0 (cadr occur)))

(list top bot)

(list "Probability of any rules being involed." (* 100 (/ (+ truthval 0) (+ bot top)))"%")

(define rules5mh (pick-n-rand-rules num-new-symbols rules-list))

(define samples
  (mh-query
     5000 10

     (define rules5mh (pick-n-rand-rules num-new-symbols rules-list))

     (list-truth rules-list rules5mh)

     (equal? observedData (last (patternBuild-repeat-n num-new-symbols rules5mh data)))
   )
)

(define indices (flatten (truth-index (caar (occurences samples)) 0)))

(list-funcs indices desc-list)
|#
