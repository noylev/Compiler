(load "HW3/compilerHW2.scm")



(define deep-map
  (lambda (f l)
    (let deep ((x l))
      (cond ((null? x) x)
            ((pair? x) (map deep x))
            (else (f x))))))

(define precheck-for-caar
  (lambda(lst) (and (list? lst) (pair? lst) (list? (car lst)) (pair? (car lst)))))

(define if-tag-cont-else-fail
  (lambda (tag)
    (lambda (cont fail)
    (lambda (exp)
    (if (and (list? exp) (pair? exp) (eqv? (car exp) tag))
        (cont exp)
        (fail))))))

(define precheck-lambda-body-or-fail
  (lambda (exp)
    (or (((if-tag-cont-else-fail 'lambda-simple) caddr (lambda () #f)) exp)
        (((if-tag-cont-else-fail 'lambda-var) caddr (lambda () #f)) exp)
        (((if-tag-cont-else-fail 'lambda-opt) cadddr (lambda () #f)) exp))))

(define precheck-lambda-para-or-fail
  (lambda (exp)
    (or (((if-tag-cont-else-fail 'lambda-simple) cadr (lambda () #f)) exp)
        (((if-tag-cont-else-fail 'lambda-var) (lambda (x) (list (cadr x))) (lambda () #f)) exp)
        (((if-tag-cont-else-fail 'lambda-opt) (lambda (x) (append (cadr x)(list (caddr x)))) (lambda () #f)) exp))))


(define precheck-for-caddr
(lambda (x) (if(and (list? exp)(pair? exp)(pair? (cdr exp))(pair? (cddr exp))) caddr #f)))

(define precheck-for-cadr
(lambda (x) (if (and (list? exp)(pair? exp)(pair? (cdr exp))) cadr #f)))

(define precheck-define-body-or-fail
  (lambda (exp)
    (((if-tag-cont-else-fail 'def) precheck-for-caddr (lambda () #f)) exp)))

(define precheck-set-body-or-fail
  (lambda (exp)
    (((if-tag-cont-else-fail 'set) precheck-for-caddr (lambda () #f)) exp)))

(define precheck-seq-body-or-fail
  (lambda (exp)
    (((if-tag-cont-else-fail 'seq) cadr (lambda () #f)) exp)))


(define precheck-var-varname-or-fail
  (lambda (exp)
    (((if-tag-cont-else-fail 'var) cadr (lambda () #f)) exp)))
        
(define precheck-const-value-or-fail
  (lambda (exp)
    (((if-tag-cont-else-fail 'var) cadr (lambda () #f)) exp)))

(define precheck-if3-return-or-fail
  (lambda (exp)
    (((if-tag-cont-else-fail 'if3) (lambda (x) x) (lambda () #f)) exp)
       ))
(define precheck-if2-return-or-fail
  (lambda (exp)
        (((if-tag-cont-else-fail 'if2) (lambda (x) x) (lambda () #f)) exp)))

(define precheck-or-return-or-fail
  (lambda (exp)
        (((if-tag-cont-else-fail 'or) (lambda (x) x) (lambda () #f)) exp)))

(define contains-recursive? (lambda  (l i)
                              (if (not(pair? l)) #f
                                  (or (eqv? (car l) i) (if(list? (car l)) (contains-recursive? (car l) i) #f) (contains-recursive? (cdr l) i)))))

(define separator
  (lambda (parse-exprs ret-defines+body)
    (if (null? parse-exprs) (ret-defines+body '() '())
        (separator (cdr parse-exprs)
                   (lambda (definitions expressions)
                     (cond ((and (precheck-for-caar parse-exprs)(eq? (caar parse-exprs) 'def))
                            (ret-defines+body (cons (car parse-exprs) definitions) expressions))
                           ((and (precheck-for-caar parse-exprs)(eq? (caar parse-exprs) 'seq))
                            (separator (cadar parse-exprs)
                                       (lambda(ds es)
                                         (ret-defines+body
                                          (append ds definitions)
                                          (append es expressions)))))
                           (else (ret-defines+body definitions (cons (car parse-exprs) expressions)))
                           ))))))


(define create-new-body
  (lambda (def clean-body)
    `(applic (lambda-simple ,(map cadadr def)
                           (seq ,(append
                                   (map (lambda(set-item) `(set (var ,(cadadr set-item)) ,(caddr set-item)))
                                        def)
                                   (map (lambda (hey) `(applic (lambda-simple () ,hey) ())) clean-body))))
            ,(map (lambda(idk) `(const #f)) def)
            )))

(define exp-reassemble
  (lambda (lambda new-body)
    (cond ((or (eq? (car lambda) 'lambda-simple)
               (eq? (car lambda) 'lambda-var))
           `(,(car lambda) ,(cadr lambda) ,new-body))
          ((eq? (car lambda) 'lambda-opt)
           `(,(car lambda) ,(cadr lambda) ,(caddr lambda) ,new-body))
          )
    ))


(define eliminate-def-specific-lambda
  (lambda (exp-body-of-lambda)
    (let* ((exp-body (list exp-body-of-lambda))
           (nested-def (separator  exp-body (lambda (dif exp) dif)))
           (exp-body-without-nested-def(separator exp-body (lambda (dif exp) exp))))
      (if (not (null? nested-def))
          (let ((new-body (create-new-body nested-def exp-body-without-nested-def)))
            new-body
            )
          exp-body-of-lambda
          ))))

(define eliminate-nested-defines
  (lambda (exp)
    (cond ((null? exp)
           '())
          ((precheck-lambda-body-or-fail exp)
           (exp-reassemble exp
                           (eliminate-def-specific-lambda (eliminate-nested-defines(precheck-lambda-body-or-fail exp)))
                           ))
                           
          ((and (list? exp)(pair? exp))
           (cons (eliminate-nested-defines (car exp)) (eliminate-nested-defines (cdr exp))))
          (else exp))
          
    ))


(define p-empty-lambda
  (pattern-rule
   `(applic (lambda-simple () ,(? 'body)) ())
   (lambda (body)
    (remove-applic-lambda-nil body))))




(define remove-applic-lambda-nil
  (lambda (exp)
    (
     (compose-patterns p-empty-lambda) exp
                                       (lambda ()
                                         (cond ((null? exp) '())
                                               ((list? exp) (map remove-applic-lambda-nil exp))
                                               (else exp))
                                         ))))

(define pe->lex-pe
  (lambda (exp)
    (pe->lex-pe-helper exp '())))

(define pe->lex-pe-helper (lambda (exp varlist)
                     (cond ((null? exp) '())
                           ((and (list? exp) (pair? exp) (eqv? (car exp) 'var))
                            (varTransform exp varlist))
                           ((and (list? exp) (pair? exp) (eqv? (car exp) 'lambda-simple))
                            (cons (car exp) (cons (cadr exp) (cons (pe->lex-pe-helper (caddr exp) (expandVarList varlist (cadr exp))) (cdddr exp)))))
                           
                           ((and (list? exp) (pair? exp) (eqv? (car exp) 'lambda-var))
                            (cons (car exp) (cons (cadr exp) (cons (pe->lex-pe-helper (caddr exp) (expandVarList varlist (list (cadr exp)))) (cdddr exp)))))
                           
                           ((and (list? exp) (pair? exp) (eqv? (car exp) 'lambda-opt))
                            (cons (car exp) (cons (cadr exp) (cons (caddr exp) (cons (pe->lex-pe-helper (cadddr exp) (expandVarList varlist (append (cadr exp) (list (caddr exp))))) (cddddr exp))))))
                            
                           ((precheck-for-caar exp)
                            (cons (pe->lex-pe-helper (car exp) varlist) (pe->lex-pe-helper (cdr exp) varlist)))
                           ((and (list? exp) (pair? exp))
                            (cons (car exp) (pe->lex-pe-helper (cdr exp) varlist)))
                           (else exp)
                           )))
(define contains?-index
  (lambda (elemento)
    (lambda (lista)
    (if (eqv? (list? (memv elemento lista)) #t)
        (- (length lista) (length (memv elemento lista)))
        #f
        )
    )))


(define contains?-index-major
  (lambda (elemento)
    (lambda (lista)
    (cond ((null? lista) 0)
          (((contains?-index elemento) (car lista)) 0)
          (else (+ 1 ((contains?-index-major elemento) (cdr lista)))))
    )))

    (define varTransform (lambda (exp varList)
                           (let ((contains-index (contains?-index (cadr exp)))
                                 (contains-index-major (contains?-index-major (cadr exp))))
                         (cond
                           ((and (pair? varList)(contains-index (car varList)))
                            `(pvar ,(cadr exp) ,(contains-index (car varList))))
                           ((and (pair? varList) (ormap contains-index varList))
                             `(bvar ,(cadr exp)
                                    ,(contains-index-major (cdr varList))
                                    ,(contains-index (list-ref varList (contains-index-major varList)))
                                   ))
                           (else `(fvar ,(cadr exp)))))
                         ))
  (define expandVarList
    (lambda (varlist parameters)
      (cons parameters varlist)))
  
  (define box-set
    (lambda (exp)
      (cond ((null? exp) '())
            ((and (list? exp) (pair? exp) (eqv? (car exp) 'lambda-simple))
             (cons (car exp)
                   (cons (cadr exp)
                         (cons (box-set-specific-lambda (box-set(caddr exp))
                                                       (map add-3-booleans (cadr exp)))(cdddr exp)))))
            
            ((and (list? exp) (pair? exp) (eqv? (car exp) 'lambda-var))
             (cons (car exp)
                   (cons (cadr exp)
                         (cons (box-set-specific-lambda (box-set(caddr exp))
                                                        (map add-3-booleans (list (cadr exp)))) (cdddr exp)))))
            
            ((and (list? exp) (pair? exp) (eqv? (car exp) 'lambda-opt))
             (cons (car exp)
                   (cons (cadr exp)
                         (cons (caddr exp)
                              (cons (box-set-specific-lambda (box-set(cadddr exp))
                                                            (map add-3-booleans (append (cadr exp) (list caddr)))) (cddddr exp))))))
           
            ((and (list? exp) (pair? exp))
             (cons (box-set (car exp)) (box-set (cdr exp))))
            (else exp)
            )
      ))


  (define reconize-set-occur
    (lambda (exp)
      (lambda (varWithBoolean)
        (cond ((null? exp)  varWithBoolean)
              
               ((and (precheck-lambda-para-or-fail exp)(contains? (precheck-lambda-para-or-fail exp)(car varWithBoolean)))
               varWithBoolean)
              
              ((and (list? exp) (pair? exp) (eqv? (car exp) 'set) (pair? (cdr exp)) (list? (cadr exp)) (pair? (cadr exp)) (eqv? (caadr exp) 'var) (eqv? (cadadr exp) (car varWithBoolean)))
               `(,(car varWithBoolean) ,(cadr varWithBoolean) #t ,(cadddr varWithBoolean)))
              
              ((precheck-for-caar exp)
               `(,(car varWithBoolean) ,(cadr varWithBoolean) ,(or (caddr((reconize-set-occur (car exp)) varWithBoolean)) (caddr((reconize-set-occur (cdr exp)) varWithBoolean))) ,(cadddr varWithBoolean)))
              
              ((and (list? exp) (pair? exp))
               ((reconize-set-occur (cdr exp)) varWithBoolean))
              (else varWithBoolean)
              )
        )))




    (define reconize-get-occur
    (lambda (exp)
      (lambda (varWithBoolean)
        (cond ((null? exp)  varWithBoolean)

              ((and (precheck-lambda-para-or-fail exp)(contains? (precheck-lambda-para-or-fail exp)(car varWithBoolean)))
               varWithBoolean)
              ((and (eqv? (car exp) 'set) (pair? (cdr exp))(list? (cadr exp)) (pair? (cadr exp)) (eqv? (caadr exp) 'var) (pair? (cddr exp)))
               ((reconize-get-occur (caddr exp)) varWithBoolean))
              
              ((and (eqv? (car exp) 'var) (eqv? (cadr exp) (car varWithBoolean)))
               `(,(car varWithBoolean) ,(cadr varWithBoolean) ,(caddr varWithBoolean) #t ))
              
              ((precheck-for-caar exp)
               `(,(car varWithBoolean) ,(cadr varWithBoolean) ,(caddr varWithBoolean) ,(or (cadddr((reconize-get-occur (car exp)) varWithBoolean)) (cadddr((reconize-get-occur (cdr exp)) varWithBoolean)))))
              
              ((and (list? exp) (pair? exp))
               ((reconize-get-occur (cdr exp)) varWithBoolean))
              (else varWithBoolean)
              )
        )))





        (define reconize-bound-occur-helper
    (lambda (exp)
      (lambda (varWithBoolean)
        (cond ((null? exp)  varWithBoolean)

              ((and (precheck-lambda-para-or-fail exp) (contains? (precheck-lambda-para-or-fail exp)(car varWithBoolean)))
               varWithBoolean)
              
              ((and (list? exp) (pair? exp) (eqv? (car exp) 'var) (eqv? (cadr exp) (car varWithBoolean)))
               `(,(car varWithBoolean) ,(cadr varWithBoolean) ,(caddr varWithBoolean) #t ))
              
             ((list? exp)
               `(,(car varWithBoolean) ,(cadr varWithBoolean) ,(caddr varWithBoolean) ,(or (cadddr((reconize-bound-occur-helper (car exp)) varWithBoolean)) (cadddr((reconize-bound-occur-helper (cdr exp)) varWithBoolean)))))
              
              (else varWithBoolean)
              )
        )))



    (define reconize-bound-occur
      (lambda (exp)
        (lambda (varWithBoolean)
          (cond ((null? exp)  varWithBoolean)
                
                ((and (precheck-lambda-body-or-fail exp) (not(contains? (precheck-lambda-para-or-fail exp)(car varWithBoolean))))
                 `(,(car varWithBoolean) ,(cadddr((reconize-bound-occur-helper (precheck-lambda-body-or-fail exp)) `(,(car varWithBoolean) #f #f #f))) ,(caddr varWithBoolean) ,(cadddr varWithBoolean)))

                ((and (precheck-lambda-body-or-fail exp) (contains? (precheck-lambda-para-or-fail exp)(car varWithBoolean)))
                 varWithBoolean)

                
                ((and (list? exp)(pair? exp))
                 `(
                   ,(car varWithBoolean)
                   ,(or
                     (cadr((reconize-bound-occur (car exp)) varWithBoolean))
                     (cadr((reconize-bound-occur (cdr exp)) varWithBoolean)))
                   ,(caddr varWithBoolean)
                   ,(cadddr varWithBoolean)))
                
                (else varWithBoolean)
                )
          )))
    
    
  (define add-3-booleans
    (lambda (a)
      (list a #f #f #f)))
  
  (define box-set-specific-lambda
    (lambda (exp varList-withBooleans)
      (let*(
        (varList-after-set (map (reconize-set-occur exp) varList-withBooleans))
        (varlist-after-get (map (reconize-get-occur exp) varList-after-set))
        (varlist-after-bound (map (reconize-bound-occur exp) varlist-after-get))
        )
        ;(begin (display varlist-after-bound)
        ((if-box-add-box exp) varlist-after-bound)
        ;)
        )
      ))

  
(define box-get-replace-occur-1
  (lambda (varname)
    (lambda (exp)
      (cond ((null? exp) '())
            ((and
              (precheck-lambda-para-or-fail exp)
              (contains? (precheck-lambda-para-or-fail exp) varname))
              exp)
            ((and (list? exp) (pair? exp) (eq? (car exp) 'set )
                 (pair? (cdr exp)) (list? (cadr exp)) (pair? (cadr exp))
                  (eqv? (caadr exp) 'var )(eqv? (cadadr exp)  varname) (pair? (cddr exp)))
             `(box-set (var ,varname) ,((box-get-replace-occur-1 varname)(caddr exp))))
            ((and (list? exp) (pair? exp) (eq? (car exp) 'var )(eq? (cadr exp)  varname))
             `(box-get (var ,varname)))
            ((list? exp) (map (box-get-replace-occur-1 varname) exp))
            (else exp))
      )))

(define box-get-replace-occur
  (lambda (exp varlist)
    (if (null? varlist)
        exp
    (box-get-replace-occur
     ((box-get-replace-occur-1 (car varlist)) exp)
     (cdr varlist))
    )))
  (define if-box-add-box
    (lambda (exp)
      (lambda (varList-withBooleans)
        (if (ormap (lambda (varWithBoolean) (andmap (lambda (y) y) (cdr varWithBoolean))) varList-withBooleans)
            `(seq ,(append (fold-right
                            (lambda (c d)
                              (if (andmap (lambda (y) y) c)
                                  (cons `(set (var ,(car c)) (box (var ,(car c)))) d)
                                  d)
                              ) '() varList-withBooleans)
                           (if (eq? (car exp) 'seq)
                               (cadr (box-get-replace-occur exp (map car (filter (lambda (x) (andmap (lambda (y) y) x)) varList-withBooleans)))) 
                               (list (box-get-replace-occur exp (map car (filter (lambda (x) (andmap (lambda (y) y) x)) varList-withBooleans))))
                           )))
              exp)
      )))



  (define AnnotateHW3
    (lambda (tp?)
    (lambda (expr)
      (cond
        ((null? expr) '())
        
        ;for define
       ((precheck-define-body-or-fail expr)
         `(def ,(cadr expr) ,( (AnnotateHW3 #f) (precheck-define-body-or-fail expr))))

       ;for set
       ((precheck-set-body-or-fail expr)
         `(set ,(cadr expr) ,( (AnnotateHW3 #f) (precheck-set-body-or-fail expr))))


       ;for seq
       ((precheck-seq-body-or-fail expr)
         `(seq ,(append
                 (map (AnnotateHW3 #f)(rnd-list-withoutlast (precheck-seq-body-or-fail expr)))
                 (list ((AnnotateHW3 tp?)(rnd-list-onlylast (precheck-seq-body-or-fail expr)))))))
        ;for lambdas
        ((precheck-lambda-body-or-fail expr)
         (exp-reassemble expr ((AnnotateHW3  #t)(precheck-lambda-body-or-fail expr))))

        ;for if3, if2
        ((precheck-if3-return-or-fail expr)
         `(if3 ,((AnnotateHW3  #f)(cadr expr))
                         ,((AnnotateHW3  tp?)(caddr expr))
                         ,((AnnotateHW3  tp?)(cadddr expr))))
        
        ((precheck-if2-return-or-fail expr)
         `(if2 ,((AnnotateHW3  #f)(cadr expr))
                       ,((AnnotateHW3  tp?)(caddr expr))))

        ;for or
       ((precheck-or-return-or-fail expr)
         `(or ,(append
                (map (AnnotateHW3 #f)(rnd-list-withoutlast (cadr expr)))
               (list ((AnnotateHW3 tp?)(rnd-list-onlylast (cadr expr)))))))
      
        ;for list
        ((and(list? expr) (pair? expr))
             (if (eq? (car expr) 'applic)
                 (if tp?
                  `(tc-applic ,((AnnotateHW3 #f)(cadr expr)) ,(map (AnnotateHW3 #f) (caddr expr)))
                 `(applic ,((AnnotateHW3 #f)(cadr expr)) ,(map (AnnotateHW3 #f) (caddr expr))))
             (cons ((AnnotateHW3  #f)(car expr))((AnnotateHW3  #f)(cdr expr)))
             ))


        (else expr)
        ))))
  (define rnd-list-withoutlast
    (lambda (lst)
    (if (null? (cdr lst))
        '()
        (cons (car lst) (rnd-list-withoutlast (cdr lst))))))

    (define rnd-list-onlylast
    (lambda (lst)
    (if (null? (cdr lst))
        (car lst)
        (rnd-list-onlylast (cdr lst)))))
                  
(define annotate-tc
  (lambda (expr)
    ((AnnotateHW3  #f)expr)
    ;expr
    ))
  

