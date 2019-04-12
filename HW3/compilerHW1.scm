(load "pc.scm")
(define <digit-0-9>
  (range #\0 #\9))
(define <digit-1-9>
  (range #\1 #\9))
(define <white-space>
  (const
   (lambda (ch)
     (char<=? ch #\space))))

(define <Natural>
  (new  (*parser <digit-0-9>) *plus
       (*pack
	(lambda (s)
	  (string->number
	   (list->string
	    `(,@s)))))
       done))


(define (flatten x)
  (cond ((null? x) '())
        ((pair? x) (append (flatten (car x)) (flatten (cdr x))))
        (else (list x))))
(define <Integer>
(new (*parser (char #\+))
       (*parser <Natural>)
       (*caten 2)
       (*pack-with
	(lambda (++ n) n))

       (*parser (char #\-))
       (*parser <Natural>)
       (*caten 2)
       (*pack-with
	(lambda (gh n) (- n)))

       (*parser <Natural>)

       (*disj 3)

       done))

(define <Fraction>
  (new (*parser <Integer>)
       (*parser (char #\/))
       (*parser <Natural>)
       (*guard (lambda (n) (not (zero? n))))
       (*caten 3)
       (*pack-with
	(lambda (num div den)
	  (/ num den)))
       done))
(define <not-infix-Symbol>
  (new(*parser
      (range #\a #\z))
      (*parser
      (range #\A #\Z))
      (*disj 2)
  done))
(define <Number4infix>
  (new (*parser <Fraction>)
       (*parser <Integer>)
       (*disj 2)
       (*delayed (lambda () <not-infix-Symbol>))
       *not-followed-by
       done))
(define <Number>
  (new (*parser <Fraction>)
       (*parser <Integer>)
       (*disj 2)
       (*delayed (lambda () <Symbol>))
       *not-followed-by
       done))

(define ^<meta-char>
  (lambda (str ch)
    (new (*parser (word str))
	 (*pack (lambda (_) ch))
	 done)))
(define <CharPrefix>
       (new(*parser (word "#\\"))
  (*pack(lambda (_) "#\\"))done))
(define <char-which-isnt-space>
  (new
      (*parser <any-char>)
   (*parser <white-space>)
            *diff
            done))
(define <char-which-isnt-space-orparen>
  (new
      (*parser <any-char>)
   (*parser <white-space>)
   (*parser (word ")"))
	(*disj 2)
            *diff
            done))
(define <VisibleSimpleChar>
  (new
   (*parser <char-which-isnt-space>)
   (*parser <char-which-isnt-space-orparen>)
       *not-followed-by
            done))

(define <NamedChar>
  (new
   (*parser (^<meta-char> "lambda" (integer->char 955)))
   (*parser (^<meta-char> "newline" #\newline))
   (*parser (^<meta-char> "nul" #\nul))
   (*parser (^<meta-char> "page" #\page))
   (*parser (^<meta-char> "return" #\return))
   (*parser (^<meta-char> "tab" #\tab))
   (*parser (^<meta-char> "space" #\space))
  (*disj 7)
  done))

(define <HexChar>
    (new (*parser (range #\0 #\9))
	 (*parser (range #\a #\f))
	 (*parser (range #\A #\F))
	 (*disj 3)
	 done))

(define <StringHexChar>
  (new  (*parser (char #\\))
         (*parser (char #\x))
       (*parser <HexChar>) *star
       (*guard (lambda (lst) (let ((num (string->number (string-append "#" (list->string `(#\x ,@lst))))))
                              (or(and(>= num 0)(<= num 55295)) (and(>= num 57344)(<= num 1114111)))
                               )))
       (*parser (char #\;))
       (*caten 4)
       (*pack-with
        (lambda (a b c d)
          (integer->char
           (string->number (string-append "#" (list->string `(#\x ,@c)))))))
  done))
(define <HexUnicodeChar>
  (new  
         (*parser (char #\x))
       (*parser <HexChar>) *plus
       (*guard (lambda (lst) (let ((num (string->number (string-append "#" (list->string `(#\x ,@lst))))))
                              (or(and(>= num 0)(<= num 55295)) (and(>= num 57344)(<= num 1114111)))
                               )))
       (*caten 2)
       (*pack-with
        (lambda (b c)
          (integer->char (string->number (string-append "#" (list->string `(#\x ,@c)))))))
  done))

(define <Boolean>
  (new (*parser (word-ci "#t"))
       (*pack(lambda (_) #t))
       (*parser (word-ci "#f"))
       (*pack(lambda (_) #f))
  (*disj 2)done))
(define <string-meta-char>
  (new
       (*parser <StringHexChar>)
       (*parser (^<meta-char> "\\\\" #\\))
       (*parser (^<meta-char> "\\\"" #\"))
       (*parser (^<meta-char> "\\n" #\newline))
       (*parser (^<meta-char> "\\r" #\return))
       (*parser (^<meta-char> "\\t" #\tab))
       (*parser (^<meta-char> "\\f" #\page)) ; formfeed
       (*parser (^<meta-char> "\\{lambda}" (integer->char 955)))
       (*parser (^<meta-char> "\\{alef}" (integer->char 1488)))
       (*parser (^<meta-char> "\\{bismillah}" (integer->char 65021)))
       (*parser (^<meta-char> "\\{smiley}" (integer->char 9786)))
       (*disj 11)
       done))

(define <string-char>
  (new (*parser <string-meta-char>)

       (*parser <any-char>)

       (*parser (char #\"))
       (*parser (char #\\))
       (*disj 2)

       *diff
       (*disj 2)
       done))

(define <string>
  (new (*parser (char #\"))
       (*parser <string-char>) *star
       (*parser (char #\"))
       (*caten 3)

       (*pack-with
	(lambda (open-delim chars close-delim)
	  (list->string chars)))

       done))

(define <Char> (new
                (*parser <CharPrefix>)
               (*parser <NamedChar>)
                (*parser <HexUnicodeChar>)
               (*parser <VisibleSimpleChar>)
               (*disj 3)
               (*caten 2)
               (*pack-with (lambda  (prefix char)
                             char))
               done))



(define <SymbolChar>
  (new
   (*parser (range #\0 #\9))
   (*parser (range-ci #\a #\z))
   (*parser (char #\!))
   (*parser (char #\$))
   (*parser (char #\^))
   (*parser (char #\-))
   (*parser (char #\*))
   (*parser (char #\_))
   (*parser (char #\=))
   (*parser (char #\+))
   (*parser (char #\<))
   (*parser (char #\>))
   (*parser (char #\?))
   (*parser (char #\/))
   (*disj 14)
   done))

(define <Symbol>
  (new (*parser <SymbolChar>) *plus
       (*pack(lambda  (chars)
                             (string->symbol(string-downcase(list->string chars)))))
       done
       ))




(define <line-comment>
  (let ((<end-of-line-comment>
	 (new (*parser (char #\newline))
	      (*parser <end-of-input>)
	      (*disj 2)
	      done)))
    (new (*parser (char #\;))
	 
	 (*parser <any-char>)
	 (*parser <end-of-line-comment>)
	 *diff *star

	 (*parser <end-of-line-comment>)
	 (*caten 3)
	 done)))

(define <sexpr-comment>
  (new (*parser (word "#;"))
       (*delayed (lambda () <sexpr>))
       (*caten 2)
       done))

(define <comment>
  (disj <line-comment>
	<sexpr-comment>))

(define <skip>
  (disj <comment>
	<white-space>))

(define ^^<wrapped>
  (lambda (<wrapper>)
    (lambda (<p>)
      (new (*parser <wrapper>) 
	   (*parser <p>)
	   (*parser <wrapper>)
	   (*caten 3)
	   (*pack-with
	    (lambda (_left e _right) e))
	   done))))

(define ^<skipped*> (^^<wrapped> (star <skip>)))





(define ^<plus-delay-separator>
  (lambda (sexp func)
    (let((sexpr sexp))
    (new
   sexpr
   sexpr
	 *star
	 
	 (*caten 2)
	 (*pack-with func)
       done
       ))))


(define <plus-sexpr>
  (^<plus-delay-separator> (*delayed (lambda () <sexpr>)) cons)
  )
(define <star-sexpr>
  (new
   (*parser <plus-sexpr>)
   (*parser <epsilon>)
   (*disj 2)
   done))

(define <ProperList>
  (new
   (*parser (char #\())
   (*parser <star-sexpr>)
   (*parser (char #\)))
                  (*caten 3)
                     (*pack-with
      (lambda (open sexp close) sexp))
       done
       ))


(define <ImproperList>
  (new
   (*parser (char #\())
   (*parser <plus-sexpr>)
   (*parser (char #\.))
   (*delayed (lambda () <sexpr>))
   (*parser (char #\)))
                  (*caten 5)
                     (*pack-with
      (lambda (open1 sexps separator sexp close)
        (fold-right cons sexp sexps)))
       done
       ))



(define <Vector>
  (new (*parser (word "#"))
       (*parser (char #\())
       (*parser <star-sexpr>)
       (*parser (char #\)))
                  (*caten 4)
                     (*pack-with
      (lambda (open1 open2 sexp close) (list->vector sexp)))
       done
       ))

(define <Quoted>
  (new (*parser (word "'"))
       (*delayed (lambda() <sexpr>))
       (*caten 2)
       (*pack-with (lambda (start exp)
                     `',exp))
       done
       ))


(define <QuasiQuoted>
  (new (*parser (word "`"))
       (*delayed (lambda() <sexpr>))
       (*caten 2)
       (*pack-with (lambda (start exp)
                     (list 'quasiquote exp)))
       done
       ))


(define <UnQuoted>
  (new (*parser (word ","))
   (*delayed (lambda() <sexpr>))
   (*caten 2)
        (*pack-with (lambda (start exp)
                     (list 'unquote exp)))
       done
       ))


(define <UnQuotedAndSpliced>
  (new (*parser (char #\,))
       (*parser (char #\@))
   (*delayed (lambda() <sexpr>))
   (*caten 3)
       (*pack-with (lambda (start1 start2 exp) (list 'unquote-splicing exp))) 
       done
       ))

(define <wrappers>
  (new
                    (*parser (char #\space)) *star
                 (*parser <skip>) *star
                 (*caten 2)
                 (*parser <epsilon>)
                 (*disj 2)
                 done))
;_____________________________________________________________infix:::
;_____________________________________________________________infix:::


(define <InfixPrefixExtensionPrefix>
  (new
   (*parser (word "##"))
   (*parser (word "#%"))
   (*disj 2)
   done))
(define <InfixSymbolChar>
  (new
   (*parser <SymbolChar>)
   (*parser (char #\+))
   (*parser (char #\-))
   (*parser (char #\*))
   (*parser (char #\^))
   (*parser (char #\/))
   (*disj 5)
   *diff
   done))
(define <SymbolInfix>
  (new (*parser <InfixSymbolChar>) *plus
       (*pack(lambda  (chars)
                             (string->symbol(string-downcase(list->string chars)))))
       done
       ))
(define <InfixSexprEscape>
  (new
   (*parser <wrappers>)
   (*parser <InfixPrefixExtensionPrefix>)
   (*parser <wrappers>)
   (*delayed (lambda ()<sexpr>))
   (*parser <wrappers>)
   (*caten 5)
   (*pack-with (lambda (x y a b c) b))
   done))
(define <NumorInfixSym>
  (new
   
  (*parser <Number4infix>)
  (*parser <SymbolInfix>)
  (*disj 2)done))



(define <InfixParen>
  (new
   (*parser <wrappers>)
   (*parser <NumorInfixSym>)
   (*parser <wrappers>)
   (*parser <InfixSexprEscape>)
   (*pack (lambda (a) (cons a '() )))
 
   (*parser <wrappers>)
    (*caten 2)
   (*parser <epsilon>)
   (*disj 2)
   (*caten 4)
     (*pack-with (lambda(a func c args) (fold-left (lambda(x y) (cons func (car args))) func  args)))
           
    (*parser <wrappers>)
   (*parser (char #\( ))
   (*parser <wrappers>)
   (*delayed (lambda () <InfixSub>))
   (*parser <wrappers>)
   (*parser (char #\) ))
   (*parser <wrappers>)
   (*caten 7)
   (*pack-with (lambda(j1 a j2 b j3 d j4) `,b ))

   (*disj 2)
     
   done))

 (define <InfixArgList>
 (new
  (*delayed (lambda () <InfixSub>))
  (*parser <wrappers>)
 (*parser (char #\,))
 (*parser <wrappers>)
 (*delayed (lambda () <InfixSub>))
 (*caten 4)
 (*pack-with (lambda (a b c d)  d))
 *star
 (*caten 2)
 (*pack-with (lambda(c b) (fold-left (lambda(x y) (cons c b)) (cons c '())  b)))
 (*parser <epsilon>)
 (*disj 2)
 done)) 

 (define <InfixFuncall>
   (new
    (*parser <InfixParen>)
    (*parser <wrappers>)
    (*parser (char #\())
    (*parser <wrappers>)
    (*parser <InfixArgList>)
    (*parser <wrappers>)
    (*parser (char #\)))
    (*parser <wrappers>)
    (*caten 7)
    (*pack-with (lambda (j1 a j2 b j3 c j4)  b))
    *star
    (*caten 2)
    (*pack-with (lambda(func args) (fold-left (lambda(x y) (cons func (car args))) func  args)))
    done))
 

 (define  <InfixArrayGet>
   (new
    (*parser <InfixFuncall>)
    (*parser <wrappers>) 
    (*parser (char #\[))
    (*parser <wrappers>)
    (*delayed (lambda () <InfixSub>))
    (*parser <wrappers>)
    (*parser (char #\]))
    (*parser <wrappers>)
    (*caten 7)
    (*pack-with (lambda (j1 b j2 c j3 d j4) (lambda (first) `(vector-ref ,first ,c) )))
    *plus
    (*caten 2)
    (*pack-with
    (lambda (first lambda_rest)
      (fold-left (lambda (op elem) (elem op)) first lambda_rest) ))
    (*parser <InfixFuncall>)
    (*disj 2)
    
    done))

(define <InfixPow>
  (new
   (*delayed (lambda () <InfixArrayGet>))
   
   (*parser <wrappers>)
   (*parser (char #\^))
   (*parser (word "**"))
   (*disj 2)
   (*parser <wrappers>)
   (*delayed (lambda () <InfixPow>))
   (*caten 4)
   (*pack-with (lambda (j1 sign j2 rest) (lambda (first) `(expt ,first ,rest) )))
   *star
   (*caten 2)
   (*pack-with
    (lambda (first lambda_rest)
      (fold-left (lambda (op elem) (elem op)) first lambda_rest) ))
   done))


(define <InfixNeg> 
  (new
   (*delayed (lambda () <InfixPow>))
   
   (*parser <wrappers>)
   (*parser (char #\-))
   (*parser <wrappers>)
   (*delayed (lambda () <InfixPow>))
   (*caten 4)
   (*pack-with (lambda (j1 fun j2 num ) `(,(string->symbol (string fun)) ,num)))
   
   (*disj 2)
   done))



(define <InfixDiv>
  (new
   (*delayed (lambda () <InfixNeg>))
   (*parser <wrappers>)
   (*parser (char #\/))
   (*parser (char #\*))
   (*parser (char #\*))
   *not-followed-by
   (*disj 2)
   (*parser <wrappers>)
   (*delayed (lambda () <InfixNeg>))
   (*caten 4)
   (*pack-with (lambda(j1 fun j2 num) (cons fun num)))
   *star
   (*caten 2)
   (*pack-with (lambda(a lst) (fold-left (lambda(num next) `(,(string->symbol (string (car next))) ,num ,(cdr next))) a lst)))
   done))



(define <InfixSub> 
  (new
   
   (*delayed (lambda () <InfixDiv>))
   (*parser <wrappers>)
   (*parser (char #\-))
   (*parser (char #\+))
   (*disj 2)
   (*parser <wrappers>)
   (*delayed (lambda () <InfixDiv>))
   (*caten 4)
   (*pack-with (lambda (j1 fun j2 num ) (cons fun num)))
   *star
   (*caten 2)
   (*pack-with (lambda( a lst) (fold-left (lambda(num next) `(,(string->symbol (string (car next))) ,num ,(cdr next))) a lst)))
   done))





;-----------------------------------------------------------------



(define <InfixExpression>
  (new
    (*parser <InfixSexprEscape>)

   (*parser <InfixSub>)
   (*disj 2)
   done))

(define <InfixExtension>
  (new
   (*parser <InfixPrefixExtensionPrefix>)
   (*parser <InfixExpression>)
   (*caten 2)
   (*pack-with (lambda (infixP infixE) infixE))
   done))

;_____________________________________________________________sexpr:::
(define <sexpr> (new
                (*parser <wrappers>)


                (*parser <InfixExtension>)
                
                (*parser <Boolean>)
                (*parser <Number>)
                (*parser <string>)
                (*parser <Symbol>)
                (*parser <ProperList>)
                (*parser <ImproperList>)
                (*parser <Vector>)

                (*parser <Quoted>)
                (*parser <QuasiQuoted>)
                (*parser <UnQuoted>)
		(*parser <Char>)
                (*parser <UnQuotedAndSpliced>)
                (*disj 13)
                (*parser <wrappers>)
                (*caten 3)
                (*pack-with (lambda ( a b c) b))done))
