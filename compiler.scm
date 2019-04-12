(load "HW3/compilerHW3.scm")

(define func-prolog1 '(define map
                       (lambda (proc items)
                         (if (null? items)
                             (list)
                             (cons (proc (car items))
                                   (map proc (cdr items))))))
  )

(define global-sym-head -1)
(define prologue
(lambda () (string-append 
  "#include <stdio.h>\n"
  "#include <stdlib.h>\n"
  "#define DO_SHOW 0\n"
  "#define SOB_FALSE 5\n"
  "#define SOB_TRUE 3\n"
  "#define SOB_NIL 2\n"
  "#define SOB_VOID 1\n"
  "/* change to 0 for no debug info to be printed */\n"
  "#include \"cisc.h\"\n"
  "int main()\n"
  "{\n"
      "START_MACHINE;\n"
      "JUMP(CONTINUE);\n"
      "#include \"arch/io.lib\"\n"
      "#include \"arch/math.lib\"\n"
      "#include \"arch/string.lib\"\n"
      "#include \"arch/system.lib\"\n"
      "#include \"arch/char.lib\"\n"
      "#include \"arch/scheme.lib\"\n"
      "#include \"arch/error-detection.asm\"\n"
      "CONTINUE:\n"
      "CALL(MAKE_SOB_VOID);\n"
      "CALL(MAKE_SOB_NIL);\n"
      "PUSH(IMM(1));\n"
      "CALL(MAKE_SOB_BOOL);\n"
      "DROP(1);\n"
      "PUSH(IMM(0));\n"
      "CALL(MAKE_SOB_BOOL);\n"
      "DROP(1);\n"

      "PUSH(IMM(1));\n"
      "CALL(MALLOC);\n"
      "DROP(1);\n"
      
      "MOV(IND(R0),IMM("(format "~a" global-sym-head)"));\n"
  )))

(define epilogue
  (string-append
   ;"INFO;"
      "STOP_MACHINE;\n"
      "return 0;\n"
   "}\n"
  ))
(define global-label-counter 0)
(define global-sym-tracker -1)
(define get-global-sym-tracker
  (lambda(newval)
    (let ((oldval global-sym-tracker))
      (begin (set! global-sym-tracker newval)
             oldval))))
(define global-const-counter -1)
(define global-fvar-next-open-mem -1)
(define global-fvar-counter 0)
(define env-expand-counter 0)
(define global-const-table `())
(define set-const-table(lambda ()
                         (begin
                           (set! global-const-counter 8)
                           (set! global-const-table 
                                 
                               `(("T_BOOL" 5 #f)
                             ("T_BOOL" 3 #t)
                             ("T_NIL" 2 ())
                             ("T_VOID" 1 ,(void) )
                             ;(type address representation)
                             )))))
(define global-fvar-table `())
(define set-fvar-table(lambda ()
                                 (begin
                                   (set! global-fvar-next-open-mem (- global-const-counter 4))
                                   (set! global-fvar-table
                                         `((boolean? ,(added-core-fvar))
                                           (car ,(added-core-fvar))
                                           (cdr ,(added-core-fvar))
                                           (cons ,(added-core-fvar))
                                           (char? ,(added-core-fvar))
                                           (char->integer ,(added-core-fvar))
                                           (integer->char ,(added-core-fvar))
                                           (null? ,(added-core-fvar))
                                           (integer? ,(added-core-fvar))
                                           (pair? ,(added-core-fvar))
                                           (procedure? ,(added-core-fvar))
                                           (string? ,(added-core-fvar))
                                           (vector? ,(added-core-fvar))
                                           (zero? ,(added-core-fvar))
                                           (+ ,(added-core-fvar))
                                           (* ,(added-core-fvar))
                                           (denominator ,(added-core-fvar))
                                           (numerator ,(added-core-fvar))
                                           (= ,(added-core-fvar))
                                           (eq? ,(added-core-fvar))
                                           (list ,(added-core-fvar))
                                           (make-string ,(added-core-fvar))
                                           (make-vector ,(added-core-fvar))
                                           (string-ref ,(added-core-fvar))
                                           (vector-ref ,(added-core-fvar))
                                           (remainder ,(added-core-fvar))
                                           (/ ,(added-core-fvar))
                                           (> ,(added-core-fvar))
                                           (< ,(added-core-fvar))
                                           (string-length ,(added-core-fvar))
                                           (string ,(added-core-fvar))
                                           (vector-length ,(added-core-fvar))
                                           (vector ,(added-core-fvar))
                                           (not ,(added-core-fvar))
                                           (number? ,(added-core-fvar))
                                           (rational? ,(added-core-fvar))
                                           (string-set! ,(added-core-fvar))
                                           (vector-set! ,(added-core-fvar))
                                           (set-car! ,(added-core-fvar))
                                           (set-cdr! ,(added-core-fvar))
                                           (symbol? ,(added-core-fvar))
                                           (symbol->string ,(added-core-fvar))
                                           (apply ,(added-core-fvar))
                                           (- ,(added-core-fvar))
                                           (string->symbol,(added-core-fvar))
                                           (append ,(added-core-fvar))
                                           ))
                                   ;(representation address)
                                   )))
                                           
(define added-core-fvar
  (lambda ()
  (begin
    (set! global-fvar-next-open-mem (+ global-fvar-next-open-mem 4))
    global-fvar-next-open-mem)))
                                        
(define gen-label-num
  (lambda () (begin (set! global-label-counter (+ 1 global-label-counter))) (format "~a" global-label-counter)))

(define allTheHomeWork (lambda (sexpr)
                        (annotate-tc
                         (pe->lex-pe
                          (box-set
                           (remove-applic-lambda-nil
                            (eliminate-nested-defines
                             (parse sexpr))))))))

(define print-between-sexpr
  (lambda()
    (let ((num (gen-label-num)))
    (string-append "CMP(R0,IMM(SOB_VOID));\n"
    "JUMP_EQ(END_OF_sexpr"num");\n"
    "PUSH(R0);\n"
    "CALL(WRITE_SOB);\n"
    "DROP(1);\n"
    "CALL(NEWLINE);\n"
    "END_OF_sexpr"num":\n"
    ))))




(define compile-scheme-file
  (lambda (scheme-file c-file)
    (let* ((file-string (file->string scheme-file))
           (after-HW3-code (map (lambda (x)
            (annotate-tc
             (pe->lex-pe
              (box-set
               (remove-applic-lambda-nil
                (eliminate-nested-defines
                 (parse x)))))))
                  (get-from-strings-sexprs file-string)))
           (code-before-gen  (cons `(seq ,(list (allTheHomeWork func-prolog1)
                                                ;(allTheHomeWork func-prolog2)
                                                (car after-HW3-code)
                                                )) (cdr after-HW3-code)))
           ;(code-before-gen after-HW3-code)
           (reseter (set-const-table))
           (reseter (set-fvar-table))
           (core-fvar-tables   (code-gen-fvar-table global-fvar-table))
           (fvar-tables   (begin (set! global-fvar-next-open-mem (+ global-fvar-next-open-mem 4))
                                 (set! global-fvar-next-open-mem (- global-fvar-next-open-mem 1))
                                 (freevar-table-constructor   code-before-gen)
                                 (set! global-const-counter (+ global-fvar-next-open-mem 1))
                                 (code-gen-def-fvar-table)))
           (const-tables (begin (const-table-constructor code-before-gen) (code-gen-const-table global-const-table)))
           )
      (begin ;(display '~~~~~~~~code~input~~~~~~~~~~)
             ;(newline)
             ;(display code-before-gen)
             ;(newline)
             ;(display '~~~~~~~~const~table~~~~~~~~~~)
             ;(newline)
             ;(display global-const-table)
             ;(newline)
             ;(display '~~~~~~~~freevar~table~~~~~~~~~~)
             ;(newline)
             ;(display global-fvar-table)
             ;(newline)
             ;(newline)
             (string->file 
                           (string-append
                            (prologue)
                            core-fvar-tables
                            fvar-tables
                            const-tables
                            "//code-gen from here$$$$$$$$\n"
                             (apply string-append
                                   (map (lambda (x) (string-append (code-gen x -1)
                                                                 (print-between-sexpr)))
                                          code-before-gen))
                            "//code-gen end here $$$$$$$$\n"
                            epilogue
                            )
                         c-file
             )
      ))))

(define testTheFile
  (lambda (parser s)
    (parser s
	    (lambda (e s)
             
	      (cons e (testTheFile parser s)))
	    (lambda (w) w ))))

(define get-from-strings-sexprs
  (lambda (string)
    (testTheFile <sexpr> (string->list string))))


(define string->file
  (lambda (str out-file)
    (letrec ((out-port (open-output-file out-file 'truncate))
	  (run (lambda (lst)
		  (if (null? lst) #t
		      (begin (write-char (car lst) out-port)
			     (run (cdr lst)))))))
	(begin
	  (run (string->list str))
	  (close-output-port out-port)))
	    
))
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~TABLE-FREE-VAR~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(define freevar-table-constructor
  (lambda (sexprs)   
    (cond
      ((code-gen-tag-checker sexprs 'fvar) (add-to-fvar-table-constructor (cadr sexprs)))
      ((and (list? sexprs) (pair? sexprs)) (map freevar-table-constructor sexprs))
      (else '())
      )
    ))

(define add-to-fvar-table-constructor
  (lambda (sexpr)
    (if (fvar-table-lookup  global-fvar-table sexpr)
        (fvar-table-lookup  global-fvar-table sexpr)
        (begin
          (set! global-fvar-next-open-mem (+ global-fvar-next-open-mem 1))
          (set! global-fvar-counter (+ global-fvar-counter 1))
          (set! global-fvar-table (cons (list sexpr global-fvar-next-open-mem) global-fvar-table))
          global-fvar-next-open-mem)
        ))
  )

  (define (fvar-table-lookup l i)
  (if (null? l) #f
      (or (if (equal? (caar l) i) (cadar l) #f) (fvar-table-lookup (cdr l) i))))

(define code-gen-fvar-table
  (lambda(sexprs)
    (string-append
     "#include \"arch/fvar.lib\"\n"
     "//@@@@@@@@@@@ CORE FREE VARS @@@@@@@@@@@@@@@@@\n"
     (apply string-append (map assign-mem-for-fvar global-fvar-table))
     
    )))

(define code-gen-def-fvar-table
  (lambda ()
    (string-append
     "//@@@@@@@@@@@ DEFINED/UNDEFINED FREE VARS @@@@@@@@@@@@@@@@@\n"
     "PUSH("(format "~a" global-fvar-counter)");\n"
     "CALL(MALLOC);\n"
     "DROP(1);\n"))
  )

(define assign-mem-for-fvar
  (lambda (item)
    (string-append
     "MOV(IND("(format "~a" (cadr item))"),IMM("(format "~a" (+(cadr item) 1))"));\n")
    ))

;(define code-gen-const-table-helper
  ;(lambda(item)
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~TABLE-CONSTRUCTOR~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(define const-table-constructor
  (lambda (sexprs)
    (begin 
    (cond
      ((code-gen-tag-checker sexprs 'const) (add-to-const-table-constructor (cadr sexprs)))
      ((and (list? sexprs) (pair? sexprs)) (map const-table-constructor sexprs))
      (else '())
    ))))
(define add-to-const-table-constructor
  (lambda (sexpr)
    (begin
         (cond
           ((eq? (void) sexpr) 1)
           ((null? sexpr) 2)
           ((boolean? sexpr) (if sexpr 3 5))
           ((symbol? sexpr) (if (not(table-lookup sexpr))
                              (let ((mystr (add-to-const-table-constructor  (symbol->string sexpr))))
                                (begin
                                  (set! global-sym-head global-const-counter)
                                  (set! global-const-table (cons (list "T_SYMBOL" global-const-counter sexpr mystr (get-global-sym-tracker global-const-counter)) global-const-table))
                                  (set! global-const-counter (+ global-const-counter 3))
                                  (- global-const-counter 3)))
                              (table-lookup sexpr)
                              ))
           ((number? sexpr) (if (not(table-lookup sexpr))
                                (begin 
                                   (set! global-const-table (cons (list "T_INT" global-const-counter sexpr) global-const-table))
                                   (set! global-const-counter (+ global-const-counter 3))
                                   (- global-const-counter 3))
                                (table-lookup sexpr)
                                ))

           ((char? sexpr) (if (not(table-lookup sexpr))
                              (begin
                                   (set! global-const-table (cons (list "T_CHAR" global-const-counter sexpr) global-const-table))
                                   (set! global-const-counter (+ global-const-counter 2))
                                   (- global-const-counter 2))
                              (table-lookup sexpr)
                              ))

           ((pair? sexpr) (if (not(table-lookup sexpr))
                              (let ((mycar (add-to-const-table-constructor  (car sexpr)))
                                    (mycdr (add-to-const-table-constructor (cdr sexpr))))
                                (begin
                                   (set! global-const-table (cons (list "T_PAIR" global-const-counter sexpr mycar mycdr) global-const-table))
                                   (set! global-const-counter (+ global-const-counter 3))
                                   (- global-const-counter 3)))
                              (table-lookup sexpr)
                              ))
           ((vector? sexpr) (if (not(table-lookup sexpr))
                                (let ((vec-list (map add-to-const-table-constructor (vector->list sexpr))))
                                (begin
                                   (set! global-const-table (cons (list "T_VECTOR" global-const-counter sexpr vec-list (vector-length sexpr)) global-const-table))
                                   (set! global-const-counter (+ global-const-counter (+ (vector-length sexpr) 2)))
                                   (- global-const-counter (+ (vector-length sexpr) 2))))
                              (table-lookup sexpr))
                              )
           ((string? sexpr) (if (not(table-lookup sexpr))
                                (let ((string-list (map char->integer (string->list sexpr))))
                                  (begin
                                    (set! global-const-table (cons (list "T_STRING" global-const-counter sexpr string-list (string-length sexpr)) global-const-table))
                                    (set! global-const-counter (+ global-const-counter (+ (string-length sexpr) 2)))
                                    (- global-const-counter (+ (string-length sexpr) 2))))
                                (table-lookup sexpr))
                            )
    ))))

(define (my-contains? l i)
  (if (null? l) #f
      (or (if (equal? (caddar l) i) (cadar l) #f) (my-contains? (cdr l) i))))


(define table-lookup
  (lambda (sexpr) (my-contains? global-const-table sexpr)
        ))

(define code-gen-const-table
  (lambda(the-table)
    (apply string-append (map code-gen-const-table-helper (code-gen-reverse-lst the-table)))
    ))

(define code-gen-const-table-helper
  (lambda(item)
    (cond
      ((code-gen-tag-checker item "T_INT" )
    (string-append
     "PUSH(IMM("(format "~a"(denominator (caddr item)))"));\n"
     "PUSH(IMM("(format "~a"(numerator (caddr item)))"));\n"
     "CALL(MAKE_SOB_INTEGER);\n"
     "DROP(2);\n"))
      ((code-gen-tag-checker item "T_SYMBOL" )
    (string-append
     "PUSH(IMM("(format "~a"(cadddr (cdr item)))"));\n"
     "PUSH(IMM("(format "~a"(cadddr item))"));\n"
     "CALL(MAKE_SOB_SYMBOL);\n"
     "DROP(2);\n"))
      ((code-gen-tag-checker item "T_CHAR" )
    (string-append
     "PUSH(IMM("(format "~a"(char->integer(caddr item)))"));\n"
     "CALL(MAKE_SOB_CHAR);\n"
     "DROP(1);\n"))
    #;((code-gen-tag-checker item "T_BOOL")
     (let ((bool-val (if(caddr item)"1" "0")))
    (string-append
     "PUSH(IMM("bool-val"));\n"
     "CALL(MAKE_SOB_BOOL);\n"
     "DROP(1);\n")))
    #;((code-gen-tag-checker item "T_NIL")
     "CALL(MAKE_SOB_NIL);\n")
    #;((code-gen-tag-checker item "T_VOID")
     "CALL(MAKE_SOB_VOID);\n")
    ((code-gen-tag-checker item "T_PAIR")
     (let ((car-item (format "~a"(cadddr item)))
           (cdr-item (format "~a"(cadddr(cdr item)))))
    (string-append
     "PUSH(IMM("cdr-item"));\n"      ;car
     "PUSH(IMM("car-item"));\n"      ;cdr
     "CALL(MAKE_SOB_PAIR);\n"
     "DROP(2);\n")))
    ((code-gen-tag-checker item "T_VECTOR")
     (let ((length (format "~a"(vector-length (caddr item))))
           (length+1 (format "~a"(+ (vector-length(caddr item)) 1)))
           (item-list (cadddr item)))
       (string-append
    (apply string-append
     (map (lambda(item) (string-append "PUSH(IMM("(format "~a" item)"));\n"))
                               item-list))
     "PUSH(IMM("length"));\n"      
     "CALL(MAKE_SOB_VECTOR);\n"
     "DROP("length+1");\n")))
    ((code-gen-tag-checker item "T_STRING")
     (let ((length (format "~a"(string-length (caddr item))))
           (length+1 (format "~a"(+ (string-length(caddr item)) 1)))
           (item-list (cadddr item)))
       (string-append
    (apply string-append
     (map (lambda(item) (string-append "PUSH(IMM("(format "~a" item)"));\n"))
                               item-list))
     "PUSH(IMM("length"));\n"      
     "CALL(MAKE_SOB_STRING);\n"
     "DROP("length+1");\n")))
    (else "")
   )))
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~CODE-GEN~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(define code-gen-tag-checker
  (lambda (exp tag)
    (and (list? exp) (pair? exp) (equal? (car exp) tag))))

(define code-gen
  (lambda (sexprs env-counter)
    (cond ((code-gen-tag-checker sexprs 'if3) (if3-code-gen sexprs env-counter))
          ((code-gen-tag-checker sexprs 'or) (or-code-gen sexprs env-counter))
          ((code-gen-tag-checker sexprs 'const) (const-code-gen sexprs env-counter))
          ((code-gen-tag-checker sexprs 'seq) (seq-code-gen sexprs env-counter))
          ((code-gen-tag-checker sexprs 'applic) (applic-code-gen sexprs env-counter))
          ((code-gen-tag-checker sexprs 'tc-applic) (tc-applic-code-gen sexprs env-counter))
          ((code-gen-tag-checker sexprs 'fvar) (fvar-code-gen sexprs env-counter))
          ((code-gen-tag-checker sexprs 'lambda-simple) (lambda-simple-code-gen sexprs env-counter))
          ((code-gen-tag-checker sexprs 'lambda-var) (lambda-var-code-gen sexprs env-counter))
          ((code-gen-tag-checker sexprs 'lambda-opt) (lambda-opt-code-gen sexprs env-counter))
          ((code-gen-tag-checker sexprs 'pvar) (pvar-code-gen sexprs env-counter))
          ((code-gen-tag-checker sexprs 'bvar) (bvar-code-gen sexprs env-counter))
          ((code-gen-tag-checker sexprs 'def) (def-code-gen sexprs env-counter))
;set          
          ((and(code-gen-tag-checker sexprs 'set) (code-gen-tag-checker (cadr sexprs) 'pvar)) (set-pvar-code-gen sexprs env-counter))
          ((and(code-gen-tag-checker sexprs 'set) (code-gen-tag-checker (cadr sexprs) 'bvar)) (set-bvar-code-gen sexprs env-counter))
          ((and(code-gen-tag-checker sexprs 'set) (code-gen-tag-checker (cadr sexprs) 'fvar)) (set-fvar-code-gen sexprs env-counter))
;box-get
           
          ((and(code-gen-tag-checker sexprs 'box-get) (code-gen-tag-checker (cadr sexprs) 'pvar)) (box-get-pvar-code-gen sexprs env-counter))
          ((and(code-gen-tag-checker sexprs 'box-get) (code-gen-tag-checker (cadr sexprs) 'bvar)) (box-get-bvar-code-gen sexprs env-counter))
          ((and(code-gen-tag-checker sexprs 'box-get) (code-gen-tag-checker (cadr sexprs) 'fvar)) (box-get-fvar-code-gen sexprs env-counter))
           
;box
           ((and(code-gen-tag-checker sexprs 'box) (code-gen-tag-checker (cadr sexprs) 'pvar)) (box-pvar-code-gen sexprs env-counter))
           ((and(code-gen-tag-checker sexprs 'box-get) (code-gen-tag-checker (cadr sexprs) 'bvar)) (box-bvar-code-gen sexprs env-counter))
           ((and(code-gen-tag-checker sexprs 'box-get) (code-gen-tag-checker (cadr sexprs) 'fvar)) (box-fvar-code-gen sexprs env-counter))
;box-set
           ((and(code-gen-tag-checker sexprs 'box-set) (code-gen-tag-checker (cadr sexprs) 'pvar)) (box-set-pvar-code-gen sexprs env-counter))
           ((and(code-gen-tag-checker sexprs 'box-set) (code-gen-tag-checker (cadr sexprs) 'bvar)) (box-set-bvar-code-gen sexprs env-counter))
           ((and(code-gen-tag-checker sexprs 'box-get) (code-gen-tag-checker (cadr sexprs) 'fvar)) (box-set-fvar-code-gen sexprs env-counter))
          (else (begin (display `(something is'nt parsed!!! ,sexprs)) ""))
    )))
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~DEFINE~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(define def-code-gen
  (lambda (sexprs env-counter)
    (if (and (list? sexprs) (pair? sexprs)(equal? (car sexprs) 'def))
        (let ((the-defined (code-gen (cadr sexprs) env-counter))
              (the-value (code-gen (caddr sexprs) env-counter))
              (define-mem-value (cadar(filter (lambda (item) (equal? (car item) (cadr (cadr sexprs)))) global-fvar-table)))
              )
          (string-append 
                         the-value
                         "MOV(R1,"(format "~a" define-mem-value)");\n"
                         "MOV(IND(R1),R0)\n"
                         "MOV(R0,SOB_VOID)\n")
          ))))
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~OR~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define or-code-gen
  (lambda (sexprs env-counter)
    (if (and (list? sexprs) (pair? sexprs)(equal? (car sexprs) 'or))
        (let* ((label-num (gen-label-num))
               (helper (or-code-gen-helper label-num env-counter))
               (lst-last-item (take-last-code-gen (cadr sexprs)))
               (lst-without-last-item (remove-last-code-gen (cadr sexprs))))
          (string-append
           (apply string-append (map helper lst-without-last-item))
           (code-gen lst-last-item env-counter)
          "L_or_exit_"label-num":\n")
          ))))

(define (remove-last-code-gen lst)
    (if (null? (cdr lst))
        '()
        (cons (car lst) (remove-last-code-gen (cdr lst)))))

(define (take-last-code-gen lst)
    (if (null? (cdr lst))
        (car lst)
        (take-last-code-gen (cdr lst))))

(define or-code-gen-helper
  (lambda (label-numb env-counter)
  (lambda (sexpr)
    (string-append
     (code-gen sexpr env-counter)
     "CMP(R0,IMM(SOB_FALSE));\n"
     "JUMP_NE(L_or_exit_"label-numb");\n"
          ))))
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~IF~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(define if3-code-gen
  (lambda (sexprs env-counter)
    (if (and (list? sexprs) (pair? sexprs)(equal? (car sexprs) 'if3))
        (let ((test (code-gen (cadr sexprs) env-counter))
              (dit (code-gen (caddr sexprs) env-counter))
              (dif (code-gen (cadddr sexprs) env-counter))
              (label-num (gen-label-num)))
          (string-append test
                         "CMP(R0,IMM(SOB_FALSE));\n"
                         "JUMP_EQ(L_if3_else_"label-num");\n"
                         dit
                         "JUMP(L_if3_exit_"label-num");\n"
                         "L_if3_else_"label-num":\n"
                         dif
                         "L_if3_exit_"label-num":\n")
          ))))
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~IF~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~CONST~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(define const-code-gen
  (lambda (sexprs env-counter)
    (if (and (list? sexprs) (pair? sexprs)(equal? (car sexprs) 'const))
        (let ((the-imm (cadar(filter (lambda (item) (equal? (caddr item) (cadr sexprs))) global-const-table))))
        (string-append
        "MOV(R0,IMM("(format "~a" the-imm)"));\n")
        ))))


;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~SEQ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(define seq-code-gen
  (lambda (sexprs env-counter)
    (if (and (list? sexprs) (pair? sexprs)(equal? (car sexprs) 'seq))
           (apply string-append (map (lambda (item) (code-gen item env-counter )) (cadr sexprs)))
          )))
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~FREE-VAR~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(define fvar-code-gen
  (lambda (sexprs env-counter)
    (if (and (list? sexprs) (pair? sexprs)(equal? (car sexprs) 'fvar))
        (let ((the-imm (cadar(filter (lambda (item) (equal? (car item) (cadr sexprs))) global-fvar-table))))
        (string-append
        "MOV(R0,IND("(format "~a" the-imm)"));\n")
        ))))

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~BOUND-VAR~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(define bvar-code-gen
  (lambda (sexprs env-counter)
    (if (and (list? sexprs) (pair? sexprs)(equal? (car sexprs) 'bvar))
        (let ((bound-major (format "~a" (caddr sexprs)))
              (bound-minor (format "~a" (cadddr sexprs))))
          (string-append
          "MOV(R0,FPARG(0));\n"
          "MOV(R0,INDD(R0,"bound-major"));\n"
          "MOV(R0,INDD(R0,"bound-minor"));\n"
          )))))
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~PARAMETER-VAR~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(define pvar-code-gen
  (lambda (sexprs env-counter)
    (if (and (list? sexprs) (pair? sexprs)(equal? (car sexprs) 'pvar))
        (let ((arg-num (format "~a" (+ 2 (caddr sexprs)))))
        (string-append "MOV(R0,FPARG("arg-num"));\n")
          ))))

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~BOX-BVAR~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~NOTTTT WORKING YET, IN CONSTRUCTION~~~~~~~~~~~~~~~~~~~~~
#;(define box-bvar-code-gen
  (lambda (sexprs env-counter)
    (if (and (list? sexprs) (pair? sexprs)(equal? (car sexprs) 'box)(equal? (caadr sexprs) 'bvar))
         (let ((bound-major (format "~a" (caddr sexprs)))
              (bound-minor (format "~a" (cadddr sexprs))))
          (string-append
           "PUSH(IMM(1));\n"
           "CALL(MALLOC);\n"
           "DROP(1);\n"
           "MOV(R0,FPARG("(format "~a"(+ 2 (caddr(cadr sexprs))))"));\n"
          )))))
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~BOX-PVAR~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(define box-pvar-code-gen
  (lambda (sexprs env-counter)
    (if (and (list? sexprs) (pair? sexprs)(equal? (car sexprs) 'box)(equal? (caadr sexprs) 'pvar))
          (string-append
           "PUSH(IMM(1));\n"
           "CALL(MALLOC);\n"
           "DROP(1);\n"
           "MOV(IND(R0),FPARG("(format "~a"(+ 2 (caddr(cadr sexprs))))"));\n"
           "MOV(FPARG("(format "~a"(+ 2 (caddr(cadr sexprs))))"),R0);\n"
          ))))
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~BOX-SET-PVAR~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(define box-set-pvar-code-gen
  (lambda (sexprs env-counter)
    (if (and (list? sexprs) (pair? sexprs)(equal? (car sexprs) 'box-set)(equal? (caadr sexprs) 'pvar))
        (let ((setValue (code-gen (caddr sexprs) env-counter)))
          (string-append setValue
                         "MOV(IND(FPARG("(format "~a"(+ 2 (caddr(cadr sexprs))))")),R0);\n"
                         "MOV(R0,SOB_VOID);\n"
          )))))
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~BOX-SET-BVAR~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(define box-set-bvar-code-gen
  (lambda (sexprs env-counter)
    (if (and (list? sexprs) (pair? sexprs) (equal? (car sexprs) 'box-set)(equal? (caadr sexprs) 'bvar))
        (let ((setValue (code-gen (caddr sexprs) env-counter))
              (bound-major (format "~a" (caddr (cadr sexprs))))
              (bound-minor (format "~a" (cadddr (cadr sexprs)))))
          (string-append setValue
                         "PUSH(R0);\n"
                         "MOV(R0,FPARG(0));\n"
                         "MOV(R0,INDD(R0,"bound-major"));\n"
                         "MOV(R0,INDD(R0,"bound-minor"));\n"
                         "POP(R1);\n"
                         "MOV(IND(R0),R1);\n"
                         "MOV(R0,SOB_VOID);\n"
          )))))
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~BOX-SET-FVAR~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;;;;;;;;;;;;;;;;;;;;;;;;TO DO
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~BOX-GET-PVAR~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(define box-get-pvar-code-gen
  (lambda (sexprs env-counter)
    (if (and (equal? (car sexprs) 'box-get)(equal? (caadr sexprs) 'pvar))
        (let ((arg-num (format "~a" (+ 2 (caddr (cadr sexprs))))))
        (string-append "MOV(R0,FPARG("arg-num"));\n"
                       "MOV(R0,IND(R0));\n")
          
          ))))
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~BOX-GET-BVAR~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(define box-get-bvar-code-gen
  (lambda (sexprs env-counter)
    (if (and (list? sexprs) (pair? sexprs)(equal? (car sexprs) 'box-get)(equal? (caadr sexprs) 'bvar))
        (let ((bound-major (format "~a" (caddr (cadr sexprs))))
              (bound-minor (format "~a" (cadddr (cadr sexprs)))))
          (string-append
          "MOV(R0,FPARG(0));\n"
          "MOV(R0,INDD(R0,"bound-major"));\n"
          "MOV(R0,INDD(R0,"bound-minor"));\n"
          "MOV(R0,IND(R0));\n"
          )))))
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~BOX-GET-FVAR~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(define box-get-fvar-code-gen
  (lambda (sexprs env-counter)
    (if (and (list? sexprs) (pair? sexprs)(equal? (car sexprs) 'box-get)(equal? (caadr sexprs) 'fvar))
        (let ((the-imm (cadar(filter (lambda (item) (equal? (car item) (cadr sexprs))) global-fvar-table))))
        (string-append
        "MOV(R0,IND("(format "~a" the-imm)"));\n"
        "MOV(R0,INDD(R0));\n")
        ))))
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~SET-PVAR~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(define set-pvar-code-gen
  (lambda (sexprs env-counter)
    (if (and (list? sexprs) (pair? sexprs)(equal? (car sexprs) 'set)(equal? (caadr sexprs) 'pvar))
        (let ((setValue (code-gen (caddr sexprs) env-counter)))
          (string-append setValue
                         "MOV(FPARG("(format "~a"(+ 2 (caddr(cadr sexprs))))"),R0);\n"
                         "MOV(R0,SOB_VOID);\n"
          )))))
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~SET-FVAR~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(define set-fvar-code-gen
  (lambda (sexprs env-counter)
    (if (and (list? sexprs) (pair? sexprs)(equal? (car sexprs) 'set)(equal? (caadr sexprs) 'fvar))
        (let ((setValue (code-gen (caddr sexprs) env-counter))
              (the-imm (cadar(filter (lambda (item) (equal? (car item) (cadr (cadr sexprs)))) global-fvar-table))))
          (string-append setValue
                         "MOV(IND("(format "~a" the-imm)"),R0);\n"
                         "MOV(R0,SOB_VOID);\n"
          )))))
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~SET-BVAR~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(define set-bvar-code-gen
  (lambda (sexprs env-counter)
    (if (and (list? sexprs) (pair? sexprs)(equal? (car sexprs) 'set)(equal? (caadr sexprs) 'bvar))
        (let ((setValue (code-gen (caddr sexprs) env-counter))
              (bound-major (format "~a" (caddr (cadr sexprs))))
              (bound-minor (format "~a" (cadddr (cadr sexprs)))))
          (string-append setValue
                         "PUSH(R0);\n"
                         "MOV(R0,FPARG(0));\n"
                         "MOV(R0,INDD(R0,"bound-major"));\n"
                         "MOV(R0,INDD(R0,"bound-minor"));\n"
                         "POP(R1);\n"
                         "MOV(R0,R1);\n"
                         "MOV(R0,SOB_VOID);\n"
          )))))
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~APPLIC~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(define (code-gen-reverse-lst lst)
  (let loop ((lst lst) (lst-reversed '()))
    (if (null? lst)
        lst-reversed
        (loop (cdr lst) (cons (car lst) lst-reversed)))))
(define applic-code-gen
  (lambda (sexprs env-counter)
    (if (and (list? sexprs) (pair? sexprs)(equal? (car sexprs) 'applic))
           (let* ((label-num (gen-label-num))
                  (proc (cadr sexprs))
                 (args (caddr sexprs))
                 (args-num (format "~a"(length args)))
                 (rev-args (code-gen-reverse-lst args)))
             (string-append
              "PUSH(IMM(9999999999));\n" ; for later handling if no args go into lambda-var
              (apply string-append (map (applic-code-gen-helper  env-counter) rev-args))
              "PUSH(IMM("args-num"));\n"
              "//code-gen applic\n"
              (code-gen proc env-counter)
              "//code-gen applic -end\n" 
              "CMP(INDD(R0,0),IMM(T_CLOSURE));\n"
              "JUMP_NE(L_error_cannot_apply_non_clos);\n"
              "PUSH(INDD(R0,1));\n"
              "CALLA(INDD(R0,2));\n"
              "DROP(1);\n"
              "POP(R1);\n" 
              "DROP(R1);\n"
              "DROP(1);\n"
             )
          ))))
  (define applic-code-gen-helper
    (lambda (env-counter)
    (lambda (arg)
      (string-append
       (code-gen arg env-counter)
       "PUSH(R0);\n"
       ))))
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~TAIL - POSITION - APPLIC~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  (define tc-applic-code-gen
  (lambda (sexprs env-counter)
    (if (and (list? sexprs) (pair? sexprs)(equal? (car sexprs) 'tc-applic))
           (let* ((label-num (gen-label-num))
                  (proc (cadr sexprs))
                 (args (caddr sexprs))
                 (args-num (format "~a"(length args)))
                 (args-num+2 (format "~a"(+(length args)2)))
                 (args-num+4 (format "~a"(+(length args)2)))
                 (rev-args (code-gen-reverse-lst args)))
             (string-append
              "PUSH(IMM(99));\n" ; for later handling if no args go into lambda-var
              (apply string-append (map (applic-code-gen-helper  env-counter) rev-args))
              "PUSH(IMM("args-num"));\n"
              (code-gen proc env-counter)
              "CMP(INDD(R0,0),IMM(T_CLOSURE));\n"
              "JUMP_NE(L_error_cannot_apply_non_clos);\n"
              "PUSH(INDD(R0,1));\n"
              
              
              ;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~the same till this point

              "PUSH(FPARG(-1));\n"
              "MOV(R5,FPARG(-2));\n"
              
              "MOV(R2,"args-num+4")\n"
              "MOV(R4,FPARG(1));\n"
              "ADD(R4,IMM(5));\n"
              "ADD(R2,R4);\n"
              "DECR(R4);\n"
              "MOV(R3,"args-num+2");\n"
            ;   "INFO;\n"
               

              
              "L_TC_APPLIC_Body_loop_"label-num":\n"          
              "CMP(R2,R4);\n"
              "JUMP_LT(L_TC_APPLIC_Body_loop_exit_"label-num");\n"
              "MOV(STARG(R2),STARG(R3));\n"
              "DECR(R3);\n"
              "DECR(R2);\n"
              "JUMP(L_TC_APPLIC_Body_loop_"label-num");\n"
              "L_TC_APPLIC_Body_loop_exit_"label-num":\n"

         

              
              "INCR(R4);\n"
              "DROP(R4);\n"
              "MOV(FP,R5);\n"
            ;  "INFO;\n"
              "JUMPA(INDD(R0,IMM(2)));\n"
             )
          ))))
  (define applic-code-gen-helper
    (lambda (env-counter)
    (lambda (arg)
      (string-append
       (code-gen arg env-counter)
       "PUSH(R0);\n"
       ))))
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~LAMBDA-SIMPLE~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(define lambda-simple-code-gen
  (lambda (sexprs  env-counter)
    (if (and (list? sexprs) (pair? sexprs)(equal? (car sexprs) 'lambda-simple))
        (let ((label-num (gen-label-num))
              (args-num (format "~a" (length (cadr sexprs))))
              )
        (string-append
         "CMP(IMM(-1),IMM("(format "~a" env-counter)"));\n"
         "JUMP_EQ(L_lambda_pvar_copy_loop_exit_"label-num");\n"
         "PUSH(IMM("(format "~a" (+ env-counter 1))"));\n"
         "CALL(MALLOC);\n"
         "DROP(1);\n"
         "MOV(IND(R0),IMM(777));\n"
         "MOV(R2,R0);\n"
         "MOV(R1,FPARG(0));\n"
         "MOV(R3,IMM(0));\n" ;;R3 = i 
         "MOV(R4,IMM(1));\n" ;;R4 = j
         "L_lambda_swallow_copy_loop_"label-num":\n"
         "CMP(R3,IMM("(format "~a" env-counter)"));\n"
         "JUMP_EQ(L_lambda_swallow_copy_loop_exit_"label-num");\n"
         "MOV(INDD(R2,R4),INDD(R1,R3));"
         "INCR(R3);\n"
         "INCR(R4);\n"
         "JUMP(L_lambda_swallow_copy_loop_"label-num");\n"
         "L_lambda_swallow_copy_loop_exit_"label-num":\n"
         "MOV(R3,FPARG(1));\n"
         "PUSH(R3);\n"
         "CALL(MALLOC);\n"
         "DROP(1);\n"
         "MOV(R3,R0);\n"
         "MOV(INDD(R2,0),R3);\n"
         "MOV(R4,IMM(0));\n" ;;R4 = i 
         "MOV(R5,IMM(2));\n" ;;R5 = j
         "MOV(R6,FPARG(1));\n" ;;R6 =n
         "L_lambda_pvar_copy_loop_"label-num":\n"
         "CMP(R4,R6);\n"
         "JUMP_EQ(L_lambda_pvar_copy_loop_exit_"label-num");\n"
         "MOV(INDD(R3,R4),FPARG(R5));"
         "INCR(R4);\n"
         "INCR(R5);\n"
         "JUMP(L_lambda_pvar_copy_loop_"label-num");\n"
         "L_lambda_pvar_copy_loop_exit_"label-num":\n"
         "PUSH(IMM(3));\n"
         "CALL(MALLOC);\n"
         "DROP(1);\n"
         "MOV(INDD(R0,0),IMM(T_CLOSURE));\n"
         "MOV(INDD(R0,1),R2);\n"
         "MOV(INDD(R0,2),LABEL(L_clos_body_"label-num"));\n"
         "JUMP(L_clos_exit_"label-num");\n"
         "L_clos_body_"label-num":\n"
        ; "INFO;\n"
         "PUSH(FP);\n"
         "MOV(FP,SP);\n"
         "CMP(FPARG(1),IMM("args-num"));\n"
         "JUMP_NE(L_error_lambda_args_count);\n"
         "//code-gen lambda-simple\n" 
         (code-gen (caddr sexprs) (+ env-counter 1))
         "//code-gen lambda-simple - end\n" 
         "POP(FP);\n"
       ;  "INFO;\n"
         "RETURN;\n"
         "L_clos_exit_"label-num":\n"
          )))))

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~LAMBDA-VAR~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(define lambda-var-code-gen
  (lambda (sexprs env-counter)
    (if (and (list? sexprs) (pair? sexprs)(equal? (car sexprs) 'lambda-var))
        (let ((label-num (gen-label-num))
              (args-num-1 "0")
              (args-num "1")
              (args-num+1 "2")
              (args-num+2 "3")
              )
        (string-append
         "CMP(IMM(-1),IMM("(format "~a" env-counter)"));\n"
         "JUMP_EQ(L_lambda_pvar_copy_loop_exit_"label-num");\n"
         "PUSH(IMM("(format "~a" (+ env-counter 1))"));\n"
         "CALL(MALLOC);\n"
         "DROP(1);\n"
         "MOV(IND(R0),IMM(777));\n"
         "MOV(R2,R0);\n"
         "MOV(R1,FPARG(0));\n"
         "MOV(R3,IMM(0));\n" ;;R3 = i 
         "MOV(R4,IMM(1));\n" ;;R4 = j
         "L_lambda_swallow_copy_loop_"label-num":\n"
         "CMP(R3,IMM("(format "~a" env-counter)"));\n"
         "JUMP_EQ(L_lambda_swallow_copy_loop_exit_"label-num");\n"
         "MOV(INDD(R2,R4),INDD(R1,R3));"
         "INCR(R3);\n"
         "INCR(R4);\n"
         "JUMP(L_lambda_swallow_copy_loop_"label-num");\n"
         "L_lambda_swallow_copy_loop_exit_"label-num":\n"
         "MOV(R3,FPARG(1));\n"
         "PUSH(R3);\n"
         "CALL(MALLOC);\n"
         "DROP(1);\n"
         "MOV(R3,R0);\n"
         "MOV(INDD(R2,0),R3);\n"
         "MOV(R4,IMM(0));\n" ;;R4 = i 
         "MOV(R5,IMM(2));\n" ;;R5 = j
         "MOV(R6,FPARG(1));\n" ;;R6 =n
         "L_lambda_pvar_copy_loop_"label-num":\n"
         "CMP(R4,R6);\n"
         "JUMP_EQ(L_lambda_pvar_copy_loop_exit_"label-num");\n"
         "MOV(INDD(R3,R4),FPARG(R5));"
         "INCR(R4);\n"
         "INCR(R5);\n"
         "JUMP(L_lambda_pvar_copy_loop_"label-num");\n"
         "L_lambda_pvar_copy_loop_exit_"label-num":\n"
         "PUSH(IMM(3));\n"
         "CALL(MALLOC);\n"
         "DROP(1);\n"
         "MOV(INDD(R0,0),IMM(T_CLOSURE));\n"
         "MOV(INDD(R0,1),R2);\n"
         "MOV(INDD(R0,2),LABEL(L_clos_body_"label-num"));\n"
         "JUMP(L_clos_exit_"label-num");\n"
         ;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~body~~~~~~~~~~~~~~~~~~~~~~~
         "L_clos_body_"label-num":\n"
         "PUSH(FP);\n"
         "MOV(FP,SP);\n"
         "CMP(FPARG(1),IMM("args-num-1"));\n"
         "JUMP_LT(L_error_lambda_opt_args_count);\n"
         ;~~~~~~~~~~~~~~~~~ THE FIX FOR OPT LAMBDA
         ;~~~~~~~~~~~~~~~~~ R2 number of arguments
         ;~~~~~~~~~~~~~~~~~ R1 object in process (starting as NULL)
         "MOV(R1,IMM(SOB_NIL));\n"
         "MOV(R2,FPARG(1))"
         "ADD(R2,IMM(1));\n"
         "L_clos_fix_loop_"label-num":\n"
         "CMP(R2,IMM("args-num"));\n"
         "JUMP_EQ(L_clos_fix_loop_exit_"label-num");\n"
         "PUSH(R1);\n" ;CDR
         "PUSH(FPARG(R2));\n" ;CAR
         "CALL(MAKE_SOB_PAIR);\n"
         "DROP(2);\n"
         "MOV(R1,R0);\n"
         "DECR(R2);\n"
         "JUMP(L_clos_fix_loop_"label-num");\n"
         "L_clos_fix_loop_exit_"label-num":\n"
         "MOV(FPARG("args-num+1"),R1);\n"
         ;~~~~~~~~~~~~~~~~~ END OF THE FIX FOR OPT LAMBDA
         (code-gen (caddr sexprs) (+ env-counter 1))
         "POP(FP);\n"
         "RETURN;\n"
         ;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~end of body~~~~~~~~~~~~~~~~~~~~~~~
         "L_clos_exit_"label-num":\n"
          )))))
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~LAMBDA-OPT~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(define lambda-opt-code-gen
  (lambda (sexprs env-counter)
    (if (and (list? sexprs) (pair? sexprs)(equal? (car sexprs) 'lambda-opt))
        (let ((label-num (gen-label-num))
              (args-num-1 (format "~a" (length (cadr sexprs))))
              (args-num (format "~a" (+(length (cadr sexprs))1)))
              (args-num+1 (format "~a" (+(length (cadr sexprs))2)))
              (args-num+2 (format "~a" (+(length (cadr sexprs))3)))
              )
          ;(if (> env-counter -1)
        (string-append
         "CMP(IMM(-1),IMM("(format "~a" env-counter)"));\n"
         "JUMP_EQ(L_lambda_pvar_copy_loop_exit_"label-num");\n"
         "PUSH(IMM("(format "~a" (+ env-counter 1))"));\n"
         "CALL(MALLOC);\n"
         "DROP(1);\n"
         "MOV(IND(R0),IMM(777));\n"
         "MOV(R2,R0);\n"
         "MOV(R1,FPARG(0));\n"
         "MOV(R3,IMM(0));\n" ;;R3 = i 
         "MOV(R4,IMM(1));\n" ;;R4 = j
         "L_lambda_swallow_copy_loop_"label-num":\n"
         "CMP(R3,IMM("(format "~a" env-counter)"));\n"
         "JUMP_EQ(L_lambda_swallow_copy_loop_exit_"label-num");\n"
         "MOV(INDD(R2,R4),INDD(R1,R3));"
         "INCR(R3);\n"
         "INCR(R4);\n"
         "JUMP(L_lambda_swallow_copy_loop_"label-num");\n"
         "L_lambda_swallow_copy_loop_exit_"label-num":\n"
         "MOV(R3,FPARG(1));\n"
         "PUSH(R3);\n"
         "CALL(MALLOC);\n"
         "DROP(1);\n"
         "MOV(R3,R0);\n"
         "MOV(INDD(R2,0),R3);\n"
         "MOV(R4,IMM(0));\n" ;;R4 = i 
         "MOV(R5,IMM(2));\n" ;;R5 = j
         "MOV(R6,FPARG(1));\n" ;;R6 =n
         "L_lambda_pvar_copy_loop_"label-num":\n"
         "CMP(R4,R6);\n"
         "JUMP_EQ(L_lambda_pvar_copy_loop_exit_"label-num");\n"
         "MOV(INDD(R3,R4),FPARG(R5));"
         "INCR(R4);\n"
         "INCR(R5);\n"
         "JUMP(L_lambda_pvar_copy_loop_"label-num");\n"
         "L_lambda_pvar_copy_loop_exit_"label-num":\n"
         "PUSH(IMM(3));\n"
         "CALL(MALLOC);\n"
         "DROP(1);\n"
         "MOV(INDD(R0,0),IMM(T_CLOSURE));\n"
         "MOV(INDD(R0,1),R2);\n"
         "MOV(INDD(R0,2),LABEL(L_clos_body_"label-num"));\n"
         "JUMP(L_clos_exit_"label-num");\n"
         ;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~body~~~~~~~~~~~~~~~~~~~~~~~
         "L_clos_body_"label-num":\n"
         "PUSH(FP);\n"
         "MOV(FP,SP);\n"
         "CMP(FPARG(1),IMM("args-num-1"));\n"
         "JUMP_LT(L_error_lambda_opt_args_count);\n"
         ;~~~~~~~~~~~~~~~~~ THE FIX FOR OPT LAMBDA
         ;~~~~~~~~~~~~~~~~~ R2 number of arguments
         ;~~~~~~~~~~~~~~~~~ R1 object in process (starting as NULL)
         "MOV(R1,IMM(SOB_NIL));\n"
         "MOV(R2,FPARG(1))"
         "ADD(R2,IMM(1));\n"
         "L_clos_fix_loop_"label-num":\n"
         "CMP(R2,IMM("args-num"));\n"
         "JUMP_EQ(L_clos_fix_loop_exit_"label-num");\n"
         "PUSH(R1);\n" ;CDR
         "PUSH(FPARG(R2));\n" ;CAR
         "CALL(MAKE_SOB_PAIR);\n"
         "DROP(2);\n"
         "MOV(R1,R0);\n"
         "DECR(R2);\n"
         "JUMP(L_clos_fix_loop_"label-num");\n"
         "L_clos_fix_loop_exit_"label-num":\n"
         "MOV(FPARG("args-num+1"),R1);\n"
         ;~~~~~~~~~~~~~~~~~ END OF THE FIX FOR OPT LAMBDA
         (code-gen (cadddr sexprs) (+ env-counter 1))
         "POP(FP);\n"
         "RETURN;\n"
         ;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~end of body~~~~~~~~~~~~~~~~~~~~~~~
         "L_clos_exit_"label-num":\n"
          )))))



