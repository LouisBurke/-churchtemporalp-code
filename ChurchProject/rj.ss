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

(define list-pos (lambda (l n start len) 
                   (if (= start len) '() 
                      (if (= (car l) n) 
                          (cons start (list-pos (cdr l) n (+ start 1) len))
                          (list-pos (cdr l) n (+ start 1) len)))))


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

#|(define get-rules-by-index 
        (lambda (rules index) 
                 (if (null? index) (quote ( ) )
                     (cons (list-ref rules (car index))
                           (get-rules-by-index rules (cdr index)))
                 )
        )
)|#

(define get-rules-by-index 
        (lambda (rules index) 
                 (if (null? index) (quote ( ) )
                     (cons (list-ref rules (- (car index) 1))
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

(define make-rules-simple-a
    (lambda(L n start) 
         (letrec ([A 'a]
                  [B (random-element (remq 'a L))]
                  [C '()]
                  [reg-op1 (cons (lambda (x)  (car x)) (list 'car 'X))])
           (list (list (lambda(X) 
                         (if (equal? ((car reg-op1) X) A) B C)) 
                           (list 'lambda '(X) (list 'if (list 'equal? (list (cadr reg-op1) 'X) A) B C))))                                                              ; mod 2 = 0
         )
    )
)

(define make-rules-simple-b
    (lambda(L n start) 
         (letrec ([A 'b]
                  [B (random-element (remq 'b L))]
                  [C '()]
                  [reg-op1 (cons (lambda (x)  (car x)) (list 'car 'X))])
           (list (list (lambda(X) 
                         (if (equal? ((car reg-op1) X) A) B C)) 
                           (list 'lambda '(X) (list 'if (list 'equal? (list (cadr reg-op1) 'X) A) B C))))                                                              ; mod 2 = 0
         )
    )
)

(define make-rules-simple-c
    (lambda(L n start) 
         (letrec ([A 'b]
                  [B (random-element (remq 'c L))]
                  [C '()]
                  [reg-op1 (cons (lambda (x)  (car x)) (list 'car 'X))])
           (list (list (lambda(X) 
                         (if (equal? ((car reg-op1) X) A) B C)) 
                           (list 'lambda '(X) (list 'if (list 'equal? (list (cadr reg-op1) 'X) A) B C))))                                                              ; mod 2 = 0
         )
    )
)

(define make-rules-simple-d
    (lambda(L n start) 
         (letrec ([A 'b]
                  [B (random-element (remq 'd L))]
                  [C '()]
                  [reg-op1 (cons (lambda (x)  (car x)) (list 'car 'X))])
           (list (list (lambda(X) 
                         (if (equal? ((car reg-op1) X) A) B C)) 
                           (list 'lambda '(X) (list 'if (list 'equal? (list (cadr reg-op1) 'X) A) B C))))                                                              ; mod 2 = 0
         )
    )
)

(define make-rules-simple-e
    (lambda(L n start) 
         (letrec ([A 'b]
                  [B (random-element (remq 'e L))]
                  [C '()]
                  [reg-op1 (cons (lambda (x)  (car x)) (list 'car 'X))])
           (list (list (lambda(X) 
                         (if (equal? ((car reg-op1) X) A) B C)) 
                           (list 'lambda '(X) (list 'if (list 'equal? (list (cadr reg-op1) 'X) A) B C))))                                                              ; mod 2 = 0
         )
    )
)

(define make-rules-simple-q
    (lambda(L n start) 
         (letrec ([A 'b]
                  [B (random-element (remq 'q L))]
                  [C '()]
                  [reg-op1 (cons (lambda (x)  (car x)) (list 'car 'X))])
           (list (list (lambda(X) 
                         (if (equal? ((car reg-op1) X) A) B C)) 
                           (list 'lambda '(X) (list 'if (list 'equal? (list (cadr reg-op1) 'X) A) B C))))                                                              ; mod 2 = 0
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

(define symbols (list 'a 'b 'c 'd 'e 'q))
(define simple-symbols (list 'a 'b 'c 'd 'e 'q))
;(define simple-symbols (list 'a 'b 'c))

(define data-a (list 'a))
(define data-b (list 'b))
(define data-c (list 'c))
(define data-d (list 'd))
(define data-e (list 'e))
(define data-q (list 'q))

(define makeRulesRepeat
    (lambda(num syms)
        (if  (eq? num 0) '() (append (makeRules syms num) (makeRulesRepeat (- num 1) syms)))
    ) 
)

(define make-simple-rules-repeat-a
    (lambda(num syms start)
        (if  (eq? num 0) '() (append (make-rules-simple-a syms num start) (make-simple-rules-repeat-a (- num 1) syms start)))
    ) 
)

(define make-simple-rules-repeat-b
    (lambda(num syms start)
        (if  (eq? num 0) '() (append (make-rules-simple-b syms num start) (make-simple-rules-repeat-b (- num 1) syms start)))
    ) 
)

(define make-simple-rules-repeat-c
    (lambda(num syms start)
        (if  (eq? num 0) '() (append (make-rules-simple-c syms num start) (make-simple-rules-repeat-c (- num 1) syms start)))
    ) 
)

(define make-simple-rules-repeat-d
    (lambda(num syms start)
        (if  (eq? num 0) '() (append (make-rules-simple-d syms num start) (make-simple-rules-repeat-d (- num 1) syms start)))
    ) 
)

(define make-simple-rules-repeat-e
    (lambda(num syms start)
        (if  (eq? num 0) '() (append (make-rules-simple-q syms num start) (make-simple-rules-repeat-e (- num 1) syms start)))
    ) 
)

(define make-simple-rules-repeat-q
    (lambda(num syms start)
        (if  (eq? num 0) '() (append (make-rules-simple-e syms num start) (make-simple-rules-repeat-q (- num 1) syms start)))
    ) 
)

(define rules (makeRulesRepeat 2000 symbols))
(define simple-rules-1 (make-simple-rules-repeat-a 10 simple-symbols data-a))
(define simple-rules-2 (make-simple-rules-repeat-b 10 simple-symbols data-b))
(define simple-rules-3 (make-simple-rules-repeat-c 10 simple-symbols data-c))
(define simple-rules-4 (make-simple-rules-repeat-d 10 simple-symbols data-d))
(define simple-rules-5 (make-simple-rules-repeat-e 10 simple-symbols data-e))
(define simple-rules-6 (make-simple-rules-repeat-q 10 simple-symbols data-q))


#|(define hidden_rules (patternBuild-repeat-n num-new-symbols rules data))

(define exposed_data (last (expose_data hidden_rules data-a)))

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
  (lambda (rules len num)
    (let* ([rules-index (pick-n-rand-rules-index num rules)]
           [rand-rules (get-rules-by-index U rules-index)])
      (let ([sym-list (flatten (seq-build (strip-list-func rand-rules) (list (random-element symbols)) 0 len))])
        (let ([sym-length (length sym-list)])
          ;(if (= sym-length len) (list '()) '())
            (if (= sym-length len) (list sym-list rand-rules rules-index) (stream rules len num))
        )
      )
    )
  )
)

(define stream-set-rules 
  (lambda (rules len)
    (let ([rand-rules rules])
      (let ([sym-list (flatten (seq-build (strip-list-func rand-rules) data-a  0 len))])
        (let ([sym-length (length sym-list)])
          ;(if (= sym-length len) (list '()) '())
            (if (= sym-length len) (list sym-list rand-rules) (stream-set-rules rules len))
        )
      )
    )
  )
)

(define irreg 
  (lambda (num-sym num-rules)
    (let ([observed-data-n-rules (stream U num-sym num-rules)])
      (if (= 1 (length (occurences (car observed-data-n-rules)))) (irreg num-sym num-rules) observed-data-n-rules)
    )
  )
)

;(irreg 4 3)

(define U (append rules simple-rules-1 simple-rules-2 simple-rules-3 simple-rules-4 simple-rules-5 simple-rules-6))

(define observed-data-n-rules (irreg 3 2))
;(define observed-data-n-rules (stream U 3 2))
(define observed-rules (cadr observed-data-n-rules))
(define observed-data (car observed-data-n-rules))
(define observed-rules-index (caddr observed-data-n-rules))

#|(define (samples)
  (rejection-query
     ;5000 10  
     
     (define rules-index (pick-n-rand-rules-index (length observed-rules) U))
     (define rules-sample (get-rules-by-index U rules-index))
     (define seq (car (seq-build (strip-list-func rules-sample) (list (last observed-data)) 0 (length observed-data))))
     
     ;(list (expose_rule_desc (get-rules-by-index U rules-index)) seq)     
     ;(list rules-index seq)     
     rules-index     
     
     (equal? observed-data seq)
  )
)


(define samples
  (mh-query
     100 1000  
     
     (define rules-index (pick-n-rand-rules-index (length observed-rules) U))
     (define rules-sample (get-rules-by-index U rules-index))
     (define seq (car (seq-build (strip-list-func rules-sample) (list (last observed-data)) 0 (length observed-data))))

     (list rules-index seq)    
     
     (equal? observed-data seq)
  )
)

(define samples
  (mh-query
     100 100  
     
     (define rules-index (pick-n-rand-rules-index (length observed-rules) U))
     (define rules-sample (get-rules-by-index U rules-index))
     (define seq (car (seq-build (strip-list-func rules-sample) (list (last observed-data)) 0 (length observed-rules))))
     
     (list rules-index seq)     
     
     #t
  )
)

(define (samples)
  (rejection-query
     ;5000 10  
     
     (define rules-index (pick-n-rand-rules-index 10 U))
     (define rules-sample (get-rules-by-index U rules-index))
     (define seq (car (seq-build (strip-list-func rules-sample) data 0 10)))
     
     (list rules-index seq)     
     
     #t
  )
)


;(define occurs (occurences (repeat 2000 samples)))
(define occurs (occurences (repeat 100 samples)))
(list (repeat 100 samples) #\newline #\newline #\newline #\newline 
             observed-data #\newline #\newline #\newline #\newline  
            observed-rules #\newline #\newline #\newline #\newline (expose_rule_desc U))
;(occurences samples)

(define list-freq (lambda (L) 
                    (if (null? L) '() 
                        (cons (last (car L)) (list-freq (cdr L))))))

;(list 'max 'freq (apply max (car (list (list-freq occurs) observed-data observed-rules-index))) 'observed 'data observed-data 'observed 'rules (expose_rule_desc observed-rules) occurs )

;(list occurs 'observed 'data observed-data observed-rules-index)

;(max (car (list-freq occurs)))


|#











#|
(list (car (stream-set-rules observed-rules (length observed-data)))
      (car (stream-set-rules (expose_rule_func  (get-rules-by-index U (caar (occurences samples)))) (length observed-data))))

(define match (lambda (L D)
  (if (null? L) '() 
    (if (equal? (caar L) D) (car L) (match (cdr L) D)))))

(define proportion (match (occurences samples2) observed-data))

(list (occurences samples2) #\newline #\newline
        (quote observed-data-=) observed-data #\newline #\newline
        (list (/ (last proportion) 5000.0 ) 'chance 'of 'inferred 'rules 'producing 'observed 'data))

|#
