#lang racket
;(require "for.rkt")
(provide my-for-4)
(define-syntax (my-for-continuation stx)
  
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
  #;(define list-assign-sym (gensym))
  (define (get-assigns ass-list acc)
    (cond
      [(empty? ass-list) (cons (reverse acc) (cons ass-list empty))]
      [(or (symbol-equal? '#:when (first ass-list)) (symbol-equal? '#:unless (first ass-list)) (symbol-equal? '#:break (first ass-list)) (symbol-equal? '#:final (first ass-list))) 
       (cons (reverse acc) (cons ass-list empty))]
      [else (get-assigns (rest ass-list) (cons (first ass-list) acc))]))
  (define k (gensym))
  (define f (gensym))
  #`(lambda (#,k)
      #,(if (empty? assigns)
            (if (symbol-equal? '#:break (first bodies))
                #`(cond
                    [#,(first (rest bodies)) (#,k)]
                    [else ((my-for-continuation () #,@(rest (rest bodies))) #,k)])
                (if (symbol-equal? '#:final (first bodies))
                    #`((my-for-continuation () #,@(rest (rest bodies)) (if #,(first (rest bodies)) (#,k) (void))) #,k)
                    #`(begin
                        #,@bodies)))
            (let ([ass-list (get-assigns assigns empty)])
              (if (empty? (first ass-list))
                  (let ([fass (first (first (rest ass-list)))]
                        [fond (first (rest (first (rest ass-list))))]
                        [fest (rest (rest (first (rest ass-list))))])
                    (cond 
                      [(symbol-equal? '#:unless fass) 
                       #`(cond [#,fond (void)]
                               [else ((my-for-continuation #,fest #,@bodies) #,k)])]
                      [(symbol-equal? '#:when fass)
                       #`(cond [#,fond (void)]
                               [else ((my-for-continuation #,fest #,@bodies) #,k)])]
                      [(symbol-equal? '#:break fass)
                       #`(cond [#,fond (#,k)]
                               [else ((my-for-continuation #,fest #,@bodies) #,k)])]
                      [(symbol-equal? '#:final fass)
                       #`((my-for-continuation #,fest #,@bodies (if #,(first (rest bodies)) (#,k) (void))) #,k)]
                      [else (error "Bad Syntax")]))
                  (let* ([names-exprs (map syntax->list (first ass-list))]
                         [temp-names (generate-temporaries names-exprs)]
                         [tests (map (lambda (name) #`((equal? '() #,name) (void))) temp-names)]
                         [names-temps (map (lambda (x y) (cons (first x) (cons y empty))) names-exprs temp-names)] 
                         [lists (map (lambda (X) (car (cdr X))) names-exprs)]
                         [assigns (map (lambda (x) #`[#,(car x) (car #,(car (cdr x)))]) names-temps)]
                         [rests (map (lambda (x) #`(cdr #,x)) temp-names)])
                    #`(begin
                        (define (#,f #,@temp-names)
                          (cond 
                            #,@tests
                            [else 
                             (begin (let        #,assigns
                                      ((my-for-continuation #,(first (rest ass-list))
                                                            #,@bodies) #,k))
                                    (#,f #,@rests))]))
                        (#,f #,@lists))))))))

(call/cc (my-for-continuation ([i '(3 2 9)] #:break (> i 9) [j '(2 3 4)])
                              (printf "~a~n" (+ i j))))
(define-syntax (my-for-4 stx)
  (define lst (syntax->list stx))
  #`(call/cc (my-for-continuation #,@(cdr lst))))

(call/cc (lambda (k) (void)))
(my-for-4 
 ([i '(1 2 5)] #:break (> i 3) [j '(4 6 9)] #:break (> j 8))
 (printf "~a~n" (+ i j)) (printf "~a~n" (- i j)))

(my-for-4 
 ([i '(1 3 5 8)] [j '(2 3 7 4)] #:final (> (* i j) 9)) #:break (> j 19)
 (printf "~a~n" (+ i j)))

(my-for-4 
 ([i '(1 3 5 8)] [j '(2 3 7 4)] #:break (> (* i j) 9)) #:break (> j 19)
 (printf "~a~n" (+ i j)))
(my-for-4 
 ([i '(1 3 5 8)] [j '(2 3 7 4)]) #:final (> (* i j) 9) #:break (> j 19)
 (printf "~a~n" (+ i j)))

(define-syntax (my-for*-continuation stx)
  (define lst (syntax->list stx))
  (define empty '())
  (define empty? (lambda (x) (equal? x empty)))
  (define symbol-equal? (lambda (A x) (equal? A (syntax->datum x))))
  (define first car)
  (define rest cdr)
  (define assigns (syntax->list (first (rest lst))))
  (define bodies (rest (rest lst)))
  
  #`(lambda (k) 
      #,(cond
          [(empty? assigns) #`((my-for-continuation () #,@bodies) k)]
          [(or (symbol-equal? '#:unless (first assigns)) (symbol-equal?  '#:when (first assigns)) (symbol-equal? '#:break (first assigns)) (symbol-equal? '#:final (first assigns))) #`((my-for-continuation (#,(first assigns) #,(first (rest assigns))) ((my-for*-continuation #,(rest (rest assigns)) #,@bodies) k)) k)]
          [else #`((my-for-continuation (#,(first assigns)) ((my-for*-continuation #,(rest assigns) #,@bodies) k)) k)])))

(define-syntax (my-for*-4 stx)
  #`(call/cc (my-for*-continuation #,@(cdr (syntax->list stx)))))

(my-for*-4 ([i '(1 2 4)] [j '(3 6 9)] [k '(7 14 21)]) 
           (printf "~a ~a ~a~n" i j k))

(my-for*-4 ([i '(1 2 4)] [j '(3 6 9)] [k '(7 14 21)]) #:final (< 19 (* i j k)) 
           (printf "~a ~a ~a~n" i j k))