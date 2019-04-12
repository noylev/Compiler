 
(load "HW3/pattern-matcher.scm")
(load "HW3/compilerHW1.scm")


(define with (lambda (s f) (apply f s)))

(define nil? (lambda (v) (if (and (list? v) (not (pair? v))) #t #f)))

(define (flatten x)
  (cond ((null? x) '())
        ((pair? x) (append (flatten (car x)) (flatten (cdr x))))
        (else (list x))))

(define no-dup2?(lambda  (lst)
                 
  (cond
     ((null? lst) #t)
    ((member (car lst) (cdr lst)) #f)
     (else (no-dup2? (cdr lst)))))) 


(define no-dup?(lambda  (lst)
                 (no-dup2? (flatten lst))) 
 ) 




(define simple-const? (lambda (v)
                        (or
                                   (nil? v)
                                   (vector? v)
                                   (boolean? v)
                                   (char? v)
                                   (number? v)
                                   (string? v)
                                   (eq? (void) v)
                                   )))

(define *reserved-words*
'(and begin cond define do else if lambda
let let* letrec or quasiquote unquote
unquote-splicing quote set!))
(define contains? (lambda  (l i)
  (if (not(pair? l)) #f
      (or (eqv? (car l) i) (contains? (cdr l) i)))))




(define var? (lambda (v) (and (not(contains? *reserved-words* v))(not-list? v)(not-pair? v))))
(define vars? (lambda (lst) (cond  ((list? lst) (andmap var? lst) )
                                                       ((pair? lst) (andmap var? (flatten lst)))
                                                       (else (var? lst)))))
(define not-list? (lambda (l) (not (list? l))))
(define not-pair? (lambda (l) (not (pair? l))))
(define not-empty? (lambda (l) (and (list? l) (pair? l))))
(define p-const (pattern-rule
                 (? 'c simple-const?)
                 (lambda (c) `(const ,c))))
(define p-quote (pattern-rule
                 `(quote ,(? 'c))
                 (lambda (c) `(const ,c))))
(define p-var 	(pattern-rule
                 (? 'v  var?)
                 (lambda (v) `(var ,v))))
(define p-if2 (pattern-rule
              `(if ,(? 'test) ,(? 'dit))
             (lambda (test dit) `(if3 ,(parse test) ,(parse dit) ,(parse (void))))))
(define p-if3 (pattern-rule
              `(if ,(? 'test) ,(? 'dit) ,(? 'dif))
             (lambda (test dit dif) `(if3 ,(parse test) ,(parse dit) ,(parse dif)))))

(define p-disj 	(pattern-rule
                 `(or . ,(? 'EXPs))
                 (lambda (EXPs)
                   (cond ((null? EXPs) `(const ,#f))
                         ((= (length EXPs) 1) (parse (car EXPs)))
                         (else `(or ,(map parse EXPs)))))))
(define beginify
	(lambda (s)
		(cond
			((null? s) *void-object*)
			((null? (cdr s)) (car s))
			(else `(begin ,@(un-begin-exp s))))))
(define un-begin-exp
  (lambda (s)
    (fold-right (lambda (x y)
                  (if (list? x)
                      (if (equal? (car x) 'begin)
                          (un-begin-exp (append (cdr x) y))
                          (cons x y))
                      (cons x y))) '() s)))




(define p-lambda
  (pattern-rule
   `(lambda  ,(? 'args vars? no-dup?) . ,(? 'body))
   (lambda (args body)
     (identify-lambda
      args
      (lambda (s) `(lambda-simple ,s ,(parse(beginify body))))
      (lambda (s opt) `(lambda-opt ,s ,opt ,(parse(beginify body))))
      (lambda (var) `(lambda-var ,var ,(parse(beginify body))))
      )
     )))

(define identify-lambda
	(lambda (argl ret-simple ret-opt ret-var)
		(cond 
			((null? argl) (ret-simple '()))
			((var? argl) (ret-var argl))     ;;;TODO: var?
			(else (identify-lambda (cdr argl)
					(lambda (s) (ret-simple `(,(car argl) ,@s))) ;simple
					(lambda (s opt) (ret-opt `(,(car argl) ,@s) opt)) ;opt
                                        (lambda (var) (ret-opt `(,(car argl)) var)))))))

(define p-applic
  (pattern-rule
   `(,(? 'func) .,(? 'para list?))
   (lambda (func para)
     `(applic ,(parse func) ,(map parse para)))))

(define p-define
  (pattern-rule
   `(define ,(? 'var var?) .,(? 'body))
   (lambda (var body) `(def (var ,var) ,(parse (beginify body))))))



(define p-mit-define
  (pattern-rule 
   `(define ,(? 'funcname) .,(? 'funcbody))
   (lambda (funcname funcbody)
     `(def ,(parse (car funcname))
        ,(parse
          `(lambda ,(cdr funcname) ,(beginify funcbody) ))))))


(define p-set
  (pattern-rule
   `(set! ,(? 'var var?) ,(? 'body not-list?))
   (lambda (var body) `(set ,(parse var) ,(parse body)))))

(define p-setList
  (pattern-rule
   `(set! ,(? 'var var?) ,(? 'body list?))
   (lambda (var body) `(set ,(parse var) ,(parse body)))))

(define p-set!
  (compose-patterns
   p-set
   p-setList))


(define p-sequence
  (pattern-rule
   `(begin .,(? 'EXPs list?))
   (lambda (EXPs)
     (cond ((null? EXPs) `(const ,(void)))
           ((= (length EXPs) 1) (parse (car EXPs)))
           (else `(seq ( ,@(map parse (un-begin-exp EXPs)))))))))

(define p-let
  (pattern-rule
   `(let ,(? 'letvars list?) .,(? 'letbody))
   (lambda(letvars letbody)
     (parse `((lambda ,(map car letvars) ,@letbody) ,@(map cadr letvars))))))



(define p-let*empty
  (pattern-rule
   `(let* () . ,(? 'exprs list?))
   (lambda (exprs)
     (parse `(let () ,@exprs)))))


(define p-let*last
  (pattern-rule  
   `(let* ((,(? 'var var?) ,(? 'val)) . ,(? 'rest null?)) . ,(? 'exprs))
   (lambda (var val rest exprs)
     (parse `(let ((,var ,val)) ,@exprs)))))

(define p-let*mid
  (pattern-rule  
   `(let* ((,(? 'var var?) ,(? 'val)) . ,(? 'rest not-empty?)) . ,(? 'exprs))
   (lambda (var val rest exprs)
     (parse `(let ((,var ,val)) (let* ,rest . ,exprs))))))



(define p-let*
  (compose-patterns
   p-let*last
   p-let*mid
   p-let*empty))

(define p-letrec
  (pattern-rule
   `(letrec ,(? 'letvars list?) .,(? 'letbody))
   (lambda(letvars letbody)
     (parse
      `((lambda ,(map car letvars)
          ,(beginify (append
                      (map (lambda(set-item) `(set! ,(car set-item) ,(cadr set-item)))
                           letvars)
                      `(((lambda () ,@letbody))))))
           ,@(map (lambda(idk) #f) letvars)       
        )))))

(define p-and
  (pattern-rule
   `(and . ,(? 'exprs list?))
   (lambda (exprs)
     (letrec
         ((and-helper
           (lambda (exprs)
             (cond
               ((= (length exprs) 0)#t)
               ((= (length exprs) 1)(car exprs))
               ((= (length exprs) 2)`(if ,(car exprs) ,(cadr exprs) #f))
               (else `(if ,(car exprs) ,(and-helper  (cdr exprs)) #f))
               ))))
       (parse (and-helper exprs))
       ))))

(define p-cond
  (pattern-rule
   `(cond .,(? 'terms not-empty?))
   (lambda (terms)
     (letrec
         ((cond-helper
           (lambda (terms)
             (cond
               ((= (length terms) 1)`(if ,(caar terms) ,(beginify (cdar terms)) ,(void)))
               ((and (= (length terms) 2) (eq? 'else (caadr terms))) `(if ,(caar terms) ,(beginify (cdar terms)) ,(beginify (cdadr terms))))
               (else `(if ,(caar terms) ,(beginify(cdar terms)) ,(cond-helper  (cdr terms))))
               ))))
       (parse (cond-helper terms)))
     )))


(define ^quote?
  (lambda (tag)
    (lambda (e)
      (and (pair? e)
	   (eq? (car e) tag)
	   (pair? (cdr e))
	   (null? (cddr e))))))

(define quote? (^quote? 'quote))
(define unquote? (^quote? 'unquote))
(define unquote-splicing? (^quote? 'unquote-splicing))

(define const?
  (let ((simple-sexprs-predicates
	 (list boolean? char? number? string?)))
    (lambda (e)
      (or (ormap (lambda (p?) (p? e))
		 simple-sexprs-predicates)
	  (quote? e)))))

(define quotify
  (lambda (e)
    (if (or (null? e)
	    (pair? e)
	    (symbol? e)
	    (vector? e))
	`',e
	e)))

(define unquotify
  (lambda (e)
    (if (quote? e)
	(cadr e)
	e)))

(define const-pair?
  (lambda (e)
    (and (quote? e)
	 (pair? (cadr e)))))

(define expand-qq
  (letrec ((expand-qq
	    (lambda (e)
	      (cond ((unquote? e) (cadr e))
		    ((unquote-splicing? e)
		     (error 'expand-qq
		       "unquote-splicing here makes no sense!"))
		    ((pair? e)
		     (let ((a (car e))
			   (b (cdr e)))
		       (cond ((unquote-splicing? a)
			      `(append ,(cadr a) ,(expand-qq b)))
			     ((unquote-splicing? b)
			      `(cons ,(expand-qq a) ,(cadr b)))
			     (else `(cons ,(expand-qq a) ,(expand-qq b))))))
		    ((vector? e) `(list->vector ,(expand-qq (vector->list e))))
		    ((or (null? e) (symbol? e)) `',e)
		    (else e))))
	   (optimize-qq-expansion (lambda (e) (optimizer e (lambda () e))))
	   (optimizer
	    (compose-patterns
	     (pattern-rule
	      `(append ,(? 'e) '())
	      (lambda (e) (optimize-qq-expansion e)))
	     (pattern-rule
	      `(append ,(? 'c1 const-pair?) (cons ,(? 'c2 const?) ,(? 'e)))
	      (lambda (c1 c2 e)
		(let ((c (quotify `(,@(unquotify c1) ,(unquotify c2))))
		      (e (optimize-qq-expansion e)))
		  (optimize-qq-expansion `(append ,c ,e)))))
	     (pattern-rule
	      `(append ,(? 'c1 const-pair?) ,(? 'c2 const-pair?))
	      (lambda (c1 c2)
		(let ((c (quotify (append (unquotify c1) (unquotify c2)))))
		  c)))
	     (pattern-rule
	      `(append ,(? 'e1) ,(? 'e2))
	      (lambda (e1 e2)
		(let ((e1 (optimize-qq-expansion e1))
		      (e2 (optimize-qq-expansion e2)))
		  `(append ,e1 ,e2))))
	     (pattern-rule
	      `(cons ,(? 'c1 const?) (cons ,(? 'c2 const?) ,(? 'e)))
	      (lambda (c1 c2 e)
		(let ((c (quotify (list (unquotify c1) (unquotify c2))))
		      (e (optimize-qq-expansion e)))
		  (optimize-qq-expansion `(append ,c ,e)))))
	     (pattern-rule
	      `(cons ,(? 'e1) ,(? 'e2))
	      (lambda (e1 e2)
		(let ((e1 (optimize-qq-expansion e1))
		      (e2 (optimize-qq-expansion e2)))
		  (if (and (const? e1) (const? e2))
		      (quotify (cons (unquotify e1) (unquotify e2)))
		      `(cons ,e1 ,e2))))))))
    (lambda (e)
      (optimize-qq-expansion
       (expand-qq e)))))


(define p-quasiquote
  (pattern-rule
   `(quasiquote .,(? 'terms not-empty?))
   (lambda (terms)
     (parse (expand-qq (car terms))))))




(define run
  (compose-patterns
   p-quasiquote
   p-cond
   p-and
   p-set!
   p-sequence
   p-letrec
   p-let
   p-let*
   p-lambda
   p-const
   p-quote
   p-disj
   p-if2
   p-if3
   p-var
   p-define
   p-mit-define
   p-applic
   ))


(define parse (lambda (sexp)
                (run sexp
                     (lambda ()
                       (error 'parse
                              (format "I can't recognize this: ~s" sexp))))))




