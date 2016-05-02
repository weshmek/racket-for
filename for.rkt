#lang racket 
(provide my-for)
(provide my-for-2)
(define (derivative f dx)
	(lambda (x)
		(/ (- (f x) (f (+ x dx))) ;;The cool part is that f exists only in the lexical scope of this lambda
		   dx)))


(define-syntax (my-let stx)
	(syntax-case stx (my-let)
		[(my-let ([id expr] ...) bdy)
			#'((lambda (id ...) bdy) expr ...)]))


(define-syntax (my-let* stx)
	(syntax-case stx (my-let*)
		[(my-let* ([id expr]) bdy)
			#'(my-let ([id expr]) bdy)]
		[(my-let* ([id expr] rst ...) bdy)
			#' (my-let ([id expr]) (my-let* (rst ...) bdy))]))


(define (merge-creator le)
  (define (merge L1 L2)
    (cond
      [(empty? L1) L2]
      [(empty? L2) L1]
      [(le (first L1) (first L2)) (cons (first L1) (merge (rest L1) L2))]
      [else (cons (first L2) (merge L1 (rest L2)))]))
  
  merge)


(define (my-foldl f acc L)
  (cond
    [(empty? L) acc]
    [else (my-foldl (f (first L) acc) (rest L))]))

(define (my-reverse L)
  (foldl cons empty L))


(define (my-naive-reverse L)
  (define (rev-acc acc L)
    (cond
      [(empty? L) acc]
      [else (rev-acc (cons (first L) acc) (rest L))]))
  
  (rev-acc empty L))

(define (my-length L)
  (define (my-length-acc L acc)
    (cond
      [(empty? L) acc]
      [else (my-length-acc (rest L) (+ acc 1))]))
  (my-length-acc L 0))

(define (split L)
  (define (split-acc L len n acc)
    (cond
      [(>= n (/ len 2)) (cons (my-reverse acc) (cons L empty))]
      [else (split-acc (rest L) len (+ n 1) (cons (first L) acc))]))
  
  (split-acc L (my-length L) 0 empty))

(define my-merge (merge-creator <=))

(define (my-mergesort L)
  (cond
    [(empty? L) L]
    [(empty? (rest L)) L]
    [else
     (my-let* ([pair (split L)]
               [L1 (first  pair)]
               [L2 (second pair)])
              (my-merge (my-mergesort L1) (my-mergesort L2)))]))


(my-mergesort '(1999 1912 2003 2001 2014 2055))

;;my-foldr : (X Y -> Y) X (listof X) -> Y
(define (my-foldr f end L)
  (cond
    [(empty? L) end]
    [else (f (first L) (my-foldr f end (rest L)))]))


;;This sucker is gonna be hard...
(define-syntax (my-for/list stx) 
  (syntax-case stx (my-for/list)
    [(my-for/list [(var lst) ...] bdy)
     (with-syntax ([f (gensym)]
                   [acc (gensym)]
                   [(params ...) (generate-temporaries #'(lst ...))])
       #`(begin (define (f params ... acc)
                  (cond 
                    [(empty? params) (reverse acc)] ...
                    [else 
                     (f (rest params) ... (cons (let ([var (first params)] ...) bdy) acc))]))
                (f lst ... empty)))]))


(my-for/list ([i (range 1 10)]
              [x (range 1 7)])
             (+ (* i i) x))

;;DrBracket

;;gcd : Number Number -> Number
(define (gcd x y)
  (cond
    [(or (= y 0) (= x 0)) (+ x y)]
    [(<= x y) (gcd x (remainder y x))]
    ;; x > y
    [else (gcd y (remainder x y))])) 

#;(define-syntax (my-for/list stx)
    (syntax-case stx (my-for/list)
      [(my-for/list ([var lst] ...  body-or-break ... bdy))
       (with-syntax 
           ([f (gensym)]
            [acc (gensym)]
            [(params1 ... params2 ... params3 ...) (generate-temporaries #'(lst1 ... lst2 ... lst3 ...))])
         #'(begin 
             (define (f params ... acc)
               (cond
                 [(empty? params) (reverse acc)] ...
                 [else
                  (let ([var1 (first params1)] ...)
                    (my-for/list rst ... 	
                                 ))]))
             (f lst1 ... lst2 ... lst3 ... empty)))]))

(define (get-first obj)
  (cond
    [(list? obj) first]
    [(string? obj) (lambda (s) (first (string->list s)))]
    [else (error)]))

(define-syntax (my-for stx)
  (syntax-case stx (my-for)
    [(my-for [(var lst) ...] bdy)
     (with-syntax
         ([(params ...) (generate-temporaries #'(lst ...))])
       #'(begin
           (define (f params ...)
             (cond
               [(empty? params) (void)] ...
               [else
                (begin
                  (let ([var (first params)] ...) bdy)
                  (f (rest params) ...))]))
           
           (f lst ...)))]))

(my-for
 ([i (range 1 10)])
 (printf "~a~n" (* i i)))


(define-syntax (my-for/and stx)
  (syntax-case stx (my-for/and)
    [(my-for/and ([var lst] ...) bdy)
     (with-syntax
         ([(params ...) (generate-temporaries #'(var ...))])
       #'(begin
           (define (f params ...)
             (cond
               [(empty? params) #t] ...
               [else
                (and (let ([var (first params)] ...) bdy) (f (rest params) ...))]))
           (f lst ...)))]))

(define-syntax (my-for/or stx)
  (syntax-case stx (my-for/or)
    [(my-for/or ([var lst] ...) bdy)
     (with-syntax
         ([(params ...) (generate-temporaries #'(var ...))])
       #'(begin
           (define (f params ...)
             (cond 
               [(empty? params) #f] ...
               [else
                (or (let ([var (first params)] ...) bdy) (f (rest params) ...))]))
           (f lst ...)))]))


;;First attempt at a parser. 
;;Almost definitely does not work properly because any symbol that derives empty string is SOL.
(define-syntax (my-parse-1 stx)
  (syntax-case stx (my-parse-1 =>)
    [(my-parse-1 S ([A => symbs ...] ...))
     (with-syntax
         
         ;;No idea if this works... It's cool if it does, though...
         ;;Note to self: Don't use ellipses in comments. It's confusing.
         
         ([((rev-symbs ...) ...) (map reverse (map syntax->list (syntax->list #'((symbs ...) ...))))]
          [((rsts ...) ...) (map generate-temporaries (syntax->list #'((symbs ...) ...)))]) 
       #'(lambda (lst)
           (define	(f stk lst)
             (cond
               [(empty? lst) (if (equal? (first (first stk)) S) (first lst) (error "Parsing Error" lst stk))]
               [(and (not (empty? stk)) (equal? (first (first stk)) S)) (first stk) ]
               [else
                (match stk
                  ;;problem: b doesn't represent the "rest" of the list
                  ;;Probably fixed now
                  [(list-rest (list rev-symbs rsts) ... b) (f (cons (list A (list (list rev-symbs rsts) ...)) b) lst)] ...
                  [_ (f (cons (first lst) stk) (rest lst))])]))
           (f empty lst)))]))	

(define parser-1 (my-parse-1
                  'S
                  (['S => 'B 'eof]
                   ['A => 'w 'B 'y]
                   ['B => 'A]
                   ['A => 'x])))    

(parser-1 (list (list 'w empty) (list 'x empty) (list 'y empty) (list 'eof empty) (list empty empty)))

;;This call is successful

;;I think this is a working LR(0) parser generator


#;(define-syntax (my-parse-2 stx)
    (define (create-follow-sets pairs) bdy)
    (syntax-case stx (my-parse-2 =>)
      [(my-parse-2 S ([A => symbs ...] ...))
       (with-syntax 
           ([(pairs ...) (create-follow-sets (A => symbs ...))] 
            [((rev-symbs ...) ...) (map reverse (map syntax->list (syntax->list #'((symbs ...) ...))))]
            [((rsts ...) ...) (map generate-temporaries (syntax->list #'((symbs ...) ...)))]) 
         #'(lambda (lst)
             (define	(f stk lst)
               (cond
                 [(empty? lst) (if (equal? (first (first stk)) S) (first lst) (error "Parsing Error" lst stk))]
                 [(and (not (empty? stk)) (equal? (first (first stk)) S)) (first stk) ]
                 [else
                  (match (cons stk (cons (first lst) empty))
                    ;;problem: b doesn't represent the "rest" of the list
                    ;;Probably fixed now
                    [(cons (list-rest (list rev-symbs rsts) ... b) (cons follow empty)) (f (cons (list A (list (list rev-symbs rsts) ...)) b) lst)] ...
                    [_ (f (cons (first lst) stk) (rest lst))])]))
             (f empty lst)))]))

(define parens-parser (my-parse-1
                       'S
                       (['S => 'A 'eof]
                        ['A => 'oparen 'A 'cparen]
                        ['A => 'A 'A]
                        ['A => 'oparen 'cparen])))

(parens-parser '((oparen ()) (oparen ()) (cparen ()) (oparen ()) (cparen ()) (cparen ()) (eof ()) (() ())))

;(parens-parser '((cparen ()) (oparen ()) (eof ()) (() ())))

(define-syntax (define-values/list stx)
  (define (range start finish)
    (cond
      [(= start finish) '()]
      [else (cons start (range (+ start 1) finish))]))
  (syntax-case stx (define-values/list)
    [(define-values/list names ... expr)
     (with-syntax
         ([((numbs ...) ...) (map (lambda (x) (range 0 x)) (range 0 (length (syntax->list #'(names ...)))))]) 
       #'(begin
           (define-syntax (value-at stx)
             (let ([lst (syntax->list stx)])
               (if (equal? (cdr (cdr lst)) '())
                   (car (cdr lst))
                   #`(cdr (value-at #,(car (cdr lst)) #,@(cdr (cdr (cdr lst))))))))
           (define L expr)
           (define names (car (value-at L numbs ...))) ...))]))


(define-values/list x y z a b c '(x y z a b c))

x
y
z
a
b
c			


(define-syntax (my-parse-3 stx)
  (define empty '())
  (define first car)
  (define rest cdr)
  (define (symbol-equal? A B)
    (equal? (syntax->datum A) (syntax->datum B)))
  
  (define first-and-rest (lambda (lst) (cons (first lst) (rest (rest lst)))))
  ;; listof Symbol Symbol listof Symbol -> listof Symbol
  (define (insert lst S acc)
    (cond
      [(equal? '() lst) (cons S acc)]
      [(symbol-equal? S (first lst)) (append lst acc)]
      [else (insert (rest lst) S (cons (first lst) acc))]))
  ;; Listof rules -> listof Symbol
  (define (get-all-symbols rules acc)	
    (cond
      [(equal? rules '()) acc]
      [else
       (get-all-symbols (rest rules) (foldr (lambda (x y) (insert y x empty)) (insert acc (first (first rules)) empty) (rest (rest (first rules)))))]))	
  (define (remove lst S)
    (cond
      [(equal? lst '()) empty]
      [(symbol-equal? (first lst) S) (rest lst)]
      [else	(cons (first lst) (remove (rest lst) S))]))
  ;; Listof rules -> listof listof Symbol
  (define (get-terms-and-nonterms rules ret-nterms ret-terms)
    (cond
      [(equal? rules '()) (cons ret-nterms (cons ret-terms empty))]
      [else (get-terms-and-nonterms (rest rules) (insert ret-nterms (first (first rules)) empty) (remove ret-terms (first (first rules))))]))	
  
  (define L (get-terms-and-nonterms (map syntax->list (syntax->list (first (rest (syntax->list stx))))) empty (get-all-symbols (map syntax->list (syntax->list (first  (rest (syntax->list stx))))) empty)))
  (define nonterms (first L))
  (define terms (first (rest L)))
  #`'(#,terms #,nonterms))

(my-parse-3
 (['S => 'A 'eof]
  ['A => 'oparen 'A 'cparen]
  ['A => 'A 'A]
  ['A => 'oparen 'cparen])) 

(define (f a) (list a a a a a))
`(,@(f 'a) ,@(f 'b) (f c) ,(f 'd))



(define-syntax (my-cond stx)
  (define empty '())
  (define empty? (lambda (x) (equal? x empty)))
  (define first car)
  (define rest cdr)
  (define symbol-equal? (lambda (x y) (equal? x (syntax->datum y))))
  (let* ([lst (syntax->list stx)]
         [lst-2 (syntax->list (first (rest lst)))])
    (if (symbol-equal? 'my-else (first lst-2))
        (first (rest  lst-2))
        #`(if #,(first lst-2)
              #,(first (rest lst-2))
              (my-cond #,@(rest (rest lst)))))))


(my-cond
 [(= 3 2) 'true]
 [(equal? 'seven 'seven) 'false]
 [my-else 'two])


(my-cond
 [my-else 'two])

(my-cond
 [(= 7 3) 'equal-seven-three]
 [(equal? '4 4) 'equal-four-four]
 [#f 'false]
 [my-else 'my-else])

(define-syntax (my-and stx)
  (define empty '())
  (define empty? (lambda (x) (equal? x empty)))
  (define first car)
  (define rest cdr)
  (define symbol-equal? (lambda (x y) (equal? x (syntax->datum y))))
  (let ([lst (syntax->list stx)])
    (if (empty? (rest lst)) #'#t
        #`(if #,(first (rest lst)) (my-and #,@(rest (rest lst))) #f))))

(my-and #t #t #f)

(define-syntax (my-for-2 stx)
  ;(define-struct when-struct (acc ass-list))
  ;(define-struct unless-struct (acc ass-list))
  ;(define-struct list-assign-struct (ass-list names lst))
  (define empty '())
  (define empty? (lambda (x) (equal? x empty)))
  (define symbol-equal? (lambda (A x) (equal? A (syntax->datum x))))
  (define first car)
  (define rest cdr)
  
  (define lst (syntax->list stx))
  (define assigns (syntax->list (first (rest lst))))
  (define bodies (rest (rest lst)))
  (define list-assign-sym (gensym))
  (define (get-assigns ass-list acc)
    (cond
      [(empty? ass-list) (cons (reverse acc) (cons ass-list empty))]
      [(or (symbol-equal? '#:when (first ass-list)) (symbol-equal? '#:unless (first ass-list))) (cons (reverse acc) (cons ass-list empty))]
      [else (get-assigns (rest ass-list) (cons (first ass-list) acc))]))
  
  (if (empty? assigns)
      #`(begin
          #,@bodies)
      (let ([ass-list (get-assigns assigns empty)])
        (if (empty? (first ass-list))
            (if (symbol-equal? '#:unless (first (first (rest ass-list))))
                #`(cond [#,(first (rest (first (rest ass-list)))) (void)]
                        [else (my-for-2 #,(rest (rest (first (rest ass-list)))) #,@bodies)])
                (if (symbol-equal? '#:when (first (first (rest ass-list))))
                    #`(cond [#,(first (rest (first (rest ass-list)))) (my-for-2 #,(rest (rest (first (rest ass-list)))) #,@bodies)]
                            [else (void)])
                    
                    (error "Bad Syntax" (first (first (rest ass-list))))))
            (let* ([names-exprs (map syntax->list (first ass-list))]
                   [temp-names (generate-temporaries names-exprs)]
                   [tests (map (lambda (name) #`((equal? '() #,name) (void))) temp-names)]
                   [names-temps (map (lambda (x y) (cons (first x) (cons y empty))) names-exprs temp-names)] 
                   [lists (map (lambda (X) (car (cdr X))) names-exprs)]
                   [assigns (map (lambda (x) #`[#,(car x) (car #,(car (cdr x)))]) names-temps)]
                   [rests (map (lambda (x) #`(cdr #,x)) temp-names)])
              #`(begin
                  (define (f #,@temp-names)
                    (cond 
                      #,@tests
                      [else 
                       (begin (let           #,assigns
                                (my-for-2 #,(first (rest ass-list))
                                          #,@bodies))
                              (f #,@rests))]))
                  (f #,@lists)))))))

#;(define-syntax (my-for-3 stx)
    #`(call/cc #,(my-for-2 (cdr (syntax->list stx)))))

(my-for-2 ([i '(a b c)] [j '(d e f)])
          (printf "~a ~a~n" i j))

#;(require 'a)
(for ([i '(a b c)] [j '(d e f)])
  (printf "~a ~a~n" i j))
(my-for-2 ([book '("Guide" "Reference" "Other")]) (cond [#f (void)] [else (my-for-2 ([chapter '("Intro" "Conclusions")]) (printf "~a ~a~n" book chapter))]))
#;(my-for-2 ([book '("Guide" "Reference" "Other")]) (if (equal? book "Other") (void) (my-for-2 ([chapter '("Intro" "Conclusions")]) (printf "~a ~a~n" book chapter))))
(my-for-2 ([book '("Guide" "Reference" "Other")] #:unless (equal? book "Other") [chapter '("Intro" "Conclusions")])
          (printf "~a ~a~n" book chapter))

;;Reason cond works instead of if: If doesn't have an implicit local definitions block. Cond does.

(define-syntax (my-for* stx)
  (define lst (syntax->list stx))
  (define empty '())
  (define empty? (lambda (x) (equal? x empty)))
  (define symbol-equal? (lambda (A x) (equal? A (syntax->datum x))))
  (define first car)
  (define rest cdr)
  (define assigns (syntax->list (first (rest lst))))
  (define bodies (rest (rest lst)))
  
  (cond
    [(empty? assigns) #`(begin #,@bodies)]
    [(or (symbol-equal? '#:unless (first assigns)) (symbol-equal?  '#:when (first assigns))) #`(my-for-2 (#,(first assigns) #,(first (rest assigns))) (my-for* #,(rest (rest assigns)) #,@bodies))]
    [else #`(my-for-2 (#,(first assigns)) (my-for* #,(rest assigns) #,@bodies))]))

(printf "for* from Racket~n")
(for* ([i '(a b c)] [j '(d e f)])
  (printf "~a ~a~n" i j))

(printf "my-for*~n")
(my-for* ([i '(a b c)] [j '(d e f)])
         (printf "~a ~a~n" i j))

(my-for* ([book '("Guide" "Reference" "Other" "Biblio")] #:unless (equal? book "Other") [chapter '("Intro" "Conclusions")])
         (printf "~a ~a~n" book chapter))

(my-for* ([i '(1 2 3)] [j '(15 7 19)] [k '(9 14 25)] #:when (< (+ i j k) 20)) 
         (printf "~a~n" (* i j k)))

(define total 0)
(my-for* ([i (range 1 60)] [j (range 1 60)] #:unless (= i j) [k (range 1 60)] #:unless (= k j) #:unless (= k i) #:unless (< 60 (* i j k)))
                           (set! total (add1 total)))
total
                           
  #;(define-syntax (my-for-continuation stx)
    
    ;(define-struct when-struct (acc ass-list))
    ;(define-struct unless-struct (acc ass-list))
    ;(define-struct list-assign-struct (ass-list names lst))
    (define empty '())
    (define empty? (lambda (x) (equal? x empty)))
    (define symbol-equal? (lambda (A x) (equal? A (syntax->datum x))))
    (define first car)
    (define rest cdr)
    
    (define lst (syntax->list stx))
    (define assigns (syntax->list (first (rest lst))))
    (define bodies (rest (rest lst)))
    (define list-assign-sym (gensym))
    (define (get-assigns ass-list acc)
      (cond
        [(empty? ass-list) (cons (reverse acc) (cons ass-list empty))]
        [(or (symbol-equal? '#:when (first ass-list)) (symbol-equal? '#:unless (first ass-list))) (cons (reverse acc) (cons ass-list empty))]
        [else (get-assigns (rest ass-list) (cons (first ass-list) acc))]))
    
    #`(lambda (k)
        #,@(if (empty? assigns)
               #`(begin
                   #,@bodies)
               (let ([ass-list (get-assigns assigns empty)])
                 (if (empty? (first ass-list))
                     (let ([fass (first (first (rest ass-list)))]
                           [fond (first (rest (first (rest ass-list))))]
                           [fest (rest (rest (first (rest ass-list))))])
                       (cond 
                         [(symbol-equal? '#:unless fass) 
                          #`(cond [#,fond (void)]
                                  [else (my-for-2 #,fest #,@bodies)])]
                         [(symbol-equal? '#:when fass)
                          #`(cond [#,fond (void)]
                                  [else (my-for-2 #,fest #,@bodies)])]
                         [(symbol-equal? '#:break fass)
                          #`(cond [#,fond (k)]
                                  [else (my-for-2 #,fest #,@bodies)])]))
                     #;(if (symbol-equal? '#:unless (first (first (rest ass-list))))
                           #`(cond [#,(first (rest (first (rest ass-list)))) (void)]
                                   [else (my-for-2 #,(rest (rest (first (rest ass-list)))) #,@bodies)])
                           (if (symbol-equal? '#:when (first (first (rest ass-list))))
                               #`(cond [#,(first (rest (first (rest ass-list)))) (my-for-2 #,(rest (rest (first (rest ass-list)))) #,@bodies)]
                                       [else (void)])
                               
                               (error "Bad Syntax" (first (first (rest ass-list))))))
                     (let* ([names-exprs (map syntax->list (first ass-list))]
                            [temp-names (generate-temporaries names-exprs)]
                            [tests (map (lambda (name) #`((equal? '() #,name) (void))) temp-names)]
                            [names-temps (map (lambda (x y) (cons (first x) (cons y empty))) names-exprs temp-names)] 
                            [lists (map (lambda (X) (car (cdr X))) names-exprs)]
                            [assigns (map (lambda (x) #`[#,(car x) (car #,(car (cdr x)))]) names-temps)]
                            [rests (map (lambda (x) #`(cdr #,x)) temp-names)])
                       #`(begin
                           (define (f #,@temp-names)
                             (cond 
                               #,@tests
                               [else 
                                (begin (let           #,assigns
                                         (my-for-2 #,(first (rest ass-list))
                                                   #,@bodies))
                                       (f #,@rests))]))
                           (f #,@lists))))))))