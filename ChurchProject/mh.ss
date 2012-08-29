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

(define num-new-symbols 10)

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

(define applies? 
  (lambda (rules D cnt)
    (cond ( (null? rules)                                                 '() ) 
          ( (null? ((car rules) D)) (append '() (applies? (cdr rules) D (+ 1 cnt)) ))
          ( else      (append (list cnt) (applies? (cdr rules) D (+ 1 cnt)) ) ))))

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

(define seq-build 
  (lambda (rules D len n)
    (let ([applies (applies? rules D 0)])
      (if (or (= len (- n 1)) (null? applies)) (list D)
        (seq-build rules 
          (flatten (list ((list-ref rules (random-element applies)) D) D)) (+ len 1) n)
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

(define pick-n-rand-rules-index 
        (lambda (n rules) 
                 (if (= 0 n) (quote ( ) )
                     (cons (car (non-dec (floor (* (random-real) (length rules))) 0))
                           (pick-n-rand-rules-index (- n 1) rules))
                 )
        )
)

(define get-rules-by-index 
        (lambda (rules index) 
                 (if (null? index) (quote ( ) )
                     (cons (list-ref rules (car index))
                           (get-rules-by-index rules (cdr index)))
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
    (if (null? L) '() (cons (cons (caar L) #\newline) (expose_rule_func (cdr L))))
  )
)

(define strip-list-func (lambda (L) (if (null? L) '() (cons (caar L) (strip-list-func (cdr L))))) 
)

(define strip-list-desc (lambda (L) (if (null? L) '() (cons (cadr L) (strip-list-desc (cddr L))))) 
)

(define list-funcs 
  (lambda(ind L) 
    (if (null? ind) '() 
      (append (list (list-ref L (car ind)) #\newline) (list-funcs  (cdr ind) L))) 
  )
)

(define make-rules-simple
    (lambda(L n start) 
         (letrec ([A 'a]
                  [B (random-element L)]
                  [C '()]
                  [reg-op1 (cons (lambda (x)  (car x)) (list 'car 'X))])
           (list (list (lambda(X) 
                         (if (equal? ((car reg-op1) X) A) B C)) 
                           (list 'lambda '(X) (list 'if (list 'equal? (list (cadr reg-op1) 'X) A) B C))))                                                              ; mod 2 = 0
         )
    )
)

(define two-rules
    (lambda() 
         (letrec ([A 'a]
                  [B 'b]
                  [C '()]
                  [reg-op1 (cons (lambda (x)  (car x)) (list 'car 'X))])
           (list (list (lambda(X) 
                         (if (equal? ((car reg-op1) X) A) B C)) 
                           (list 'lambda '(X) (list 'if (list 'equal? (list (cadr reg-op1) 'X) A) B C)))
                 (list (lambda(X) 
                         (if (equal? ((car reg-op1) X) B) A C)) 
                           (list 'lambda '(X) (list 'if (list 'equal? (list (cadr reg-op1) 'X) B) A C))))                                                              ; mod 2 = 0
         )
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
                               (cons (lambda (x)  (car x)) (list 'car 'X))                                                    ; mod 3 = 0 
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
(define simple-symbols (list 'a 'b 'c))

(define data (list 'a))
(define data-1 (list 'b))

(define makeRulesRepeat
    (lambda(num syms)
        (if  (eq? num 0) '() (append (makeRules syms num) (makeRulesRepeat (- num 1) syms)))
    ) 
)

(define make-simple-rules-repeat
    (lambda(num syms start)
        (if  (eq? num 0) '() (append (make-rules-simple syms num start) (make-simple-rules-repeat (- num 1) syms start)))
    ) 
)

(define rules (makeRulesRepeat 72 symbols))
(define simple-rules-1 (make-simple-rules-repeat 36 simple-symbols data))
;(define simple-rules-2 (make-simple-rules-repeat 18 simple-symbols data-1))

#|(define hidden_rules (patternBuild-repeat-n num-new-symbols rules data))

(define exposed_data (last (expose_data hidden_rules data)))

(define observedData exposed_data)|#

(define dataBuild-repeat-n 
        (lambda (rules L) 
                 (if (null? rules) '()
                     (letrec ([rule (caar rules)]; this might cause problems
                              [aug (flatten (append (list (rule L) L)))])
                         (append (list aug) (dataBuild-repeat-n  (cdr rules) aug))
                     )
                 )
        )
)

(define stream 
  (lambda (rules len)
    (let ([rand-rules (pick-n-rand-rules 5 rules)])
      (let ([sym-list (flatten (seq-build (strip-list-func rand-rules) data 0 len))])
        (let ([sym-length (length sym-list)])
          ;(if (= sym-length len) (list '()) '())
            (if (= sym-length len) (list sym-list rand-rules) (stream rules len))
        )
      )
    )
  )
)

(define U (append rules simple-rules-1))
;(define U (two-rules))

(define observedData (stream U 7))

observedData

#|(flatten (seq-build (strip-list-func (pick-n-rand-rules 6 rules)) data))

(define rules-index (pick-n-rand-rules-index 5 rules))

;(last (makeRules symbols (car (non-dec (floor (uniform 1 10000)) 1))))

(strip-list-func (get-rules-by-index rules (pick-n-rand-rules-index 5 rules)))

(define samples
  (mh-query
     1000 10  
     
     (define rules-index (pick-n-rand-rules-index 5 rules))
     (define rules-sample (strip-list-func (get-rules-by-index rules (pick-n-rand-rules-index 5 rules))))
          
     rules-index     
     
     (equal? observedData (seq-build rules-sample data))
   )
)

(occurences samples)

(define samples
  (mh-query
     500 10

     (define data_n_rules (patternBuild-repeat-n num-new-symbols rules data)) 

     ;(expose_rule_desc data_n_rules)
     data_n_rules

     (equal? observedData (last (expose_data data_n_rules data)))
   )
)

(define samples
  (mh-query
     2000 10

     (equal? observedData (last (expose_data (patternBuild-repeat-n num-new-symbols rules data) data)))

     #t
   )
)
(define samples
  (mh-query
     10000 10

     (define data_n_rules (patternBuild-repeat-n num-new-symbols rules data)) 

     ;(expose_rule_desc data_n_rules)
     (equal? observedData (last (expose_data data_n_rules data)))

     (equal? (cdr observedData) (cdr (last (expose_data data_n_rules data))))
   )
)

(occurences samples)
(list (last (dataBuild-repeat-n (caar (occurences samples)) data)) #\newline observedData)|#

 