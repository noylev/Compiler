
(define not-empty?
  (lambda (l)
    (and (pair? l) (list? l))))


(define dedupe
  (lambda (e)
    (if (null? e) '()
        (cons (car e) (dedupe (filter (lambda (x) (not (equal? x (car e)))) 
                                      (cdr e)))))))


(define list-length?
  (lambda (lst n)
    (= (length lst) n)))

(define quote?
  (lambda (v)
    (and (list-length? v 2)
         (equal? (car v) 'quote)
         (is-quotation? (cadr v)))))

(define is-quotation?
  (lambda (v)
    (or (number? v)
        (boolean? v)
        (char? v)
        (string? v)
        (symbol? v)
        (null? v)
        (and (pair? v)
             (is-quotation? (car v))
             (is-quotation? (cdr v))))))

(define my-inputer
  (lambda (table)
    (lambda (val)
      (if (and (list? val) (pair? val) (not (quote? val)))
          (if (hashtable-ref table (equal-hash val) #f)
              (hashtable-set! table (equal-hash val)
                              (cons
                               (car (hashtable-ref table (equal-hash val) #f))
                               ; (gensym)
                               1))
              (let ((my-gen (gensym)))
                (begin
                  (hashtable-set! table-rev (equal-hash my-gen) val)
                  (hashtable-set! table (equal-hash val) (cons my-gen 0))
                  )))))))


(define my-iter
  (lambda (table inputer)
    (lambda (val)
      (cond
        ((and (list? val)(pair? val) (not (quote? val)))
         (begin (map iter val) (inputer val)))
        ))))


(define my-replacer
  (lambda (table)
    (lambda (val)
      (if  (and (list? val) (pair? val) (not (quote? val)))
           (if(and (hashtable-ref table (equal-hash val) #f)
                   (= (cdr (hashtable-ref table (equal-hash val) #f)) 1))
              (cadr(hashtable-cell table (equal-hash val) 'idk))
              (optimize val))
           val))
    ))


(define table (make-eq-hashtable))
(define table-rev (make-eq-hashtable))

(define inputer (my-inputer table))

(define iter (my-iter table inputer))

(define replacer (my-replacer table))

(define runner (lambda (table lst)
                 (map  iter lst)))

(define optimize (lambda (lst)
                   (map replacer lst)))

(define let-list
  (lambda (table lst)
    (fold-right let-list-helper '() lst)))

(define contains?
  (lambda (l i)
    (if (not-empty? l) (or
                        (equal? (equal-hash(car l)) (equal-hash i))
                        (if(list? (car l))(contains? (car l) i) #f)(contains? (cdr l) i))
        #f
        )))

(define let-list-helper
  (lambda (val lst)
    (if  (and (list? val) (pair? val) (not (quote? val)))
         (if(and
             (hashtable-ref table (equal-hash val) #f)
             (= (cdr (hashtable-ref table (equal-hash val) #f)) 1)
             (not
              (contains? lst
                         (list (cadr(hashtable-cell table (equal-hash val) 'idk))
                               (optimize val)))))
            (cons (list (cadr(hashtable-cell table (equal-hash val) 'idk)) (optimize val)) lst)
            (append (let-list table val) lst))
         lst)))


(define in-optimize-helper
  (lambda (optlist)
    (lambda (item)
      (if(and (hashtable-ref table-rev (equal-hash item) #f)
              (not (contains? optlist item)))
         (hashtable-ref table-rev (equal-hash item) #f)
         (if  (and (list? item) (pair? item))
              (un-optimize item optlist)
              item)))))

(define un-optimize (lambda (lst optlist)
                      (map (in-optimize-helper optlist) lst)))


(define does-contain
  (lambda (optlist)
    (lambda (item lst)
      (if (contains? optlist (car item))
          (cons  (cons (car item) (un-optimize (cdr item) optlist)) lst)
          lst)
      )
    ))

(define remove-unused
  (lambda (letlist optlist)
    (list (fold-right (does-contain optlist) '() letlist))))




(define cse
  (lambda (lst)
    (let* ((x (runner table lst))
           (my-opt-list (optimize lst))
           (my-let-list (let-list table lst))
           (my-let-after-remove-dup (dedupe my-let-list))
           (my-let-optimized (remove-unused my-let-after-remove-dup my-opt-list)))
      
      (hashtable-clear! table)
      (hashtable-clear! table-rev)
      (cond ((list-length? (car my-let-optimized) 0)
             lst)
            ((list-length? (car my-let-optimized) 1)
             `(let
                  ,@my-let-optimized
                , my-opt-list))
            (else
             `(let*
                  ,@my-let-optimized
                ,my-opt-list)))
      
      )))










;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;                                       MY TESTS

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(define tester (lambda(test)
                 (newline)(display `(cse ,test))
                 (newline) (display  `===>) (display (cse-2 test)) (newline)
                 (newline) (display  `===>) (display (cse test)) (newline)))
