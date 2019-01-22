;Jen Robertson
#lang racket

(struct token (type repr) #:transparent
  #:guard (λ (type repr struct-name)
            (if (not (is-token-type? type))
                (error "expected a proper token-type which is-token-type? returns true from, got" type)
                (if (and (not (eq? eof repr))
                         (not (char? repr)))
                   (error "expected a string? or eof? for token-repr, got" repr)
                    (values type repr)))))
(provide (all-defined-out))

(define (is-token-type? typ)
  (cond ([or (eq? typ 'op)
             (eq? typ 'lparen)
             (eq? typ 'rparen)
             (eq? typ 'digit)
             (eq? typ 'eof)]
         true)
         (else false)))

(define (get-next-token input-port)
  (let ([c (read-char input-port)])
    (cond ([eq? c #\) ] (token 'rparen c))
          ([eq? c #\( ] (token 'lparen c))
          ([eq? c #\+ ] (token 'op c))
          ([eq? c #\* ] (token 'op c))
          ([eq? c eof ] (token 'eof c))
          ([or (eq? c #\0)
               (eq? c #\1)
               (eq? c #\2)
               (eq? c #\3)
               (eq? c #\4)
               (eq? c #\5)
               (eq? c #\6)
               (eq? c #\7)
               (eq? c #\8)
               (eq? c #\9)] (token 'digit c))
          (else (raise-syntax-error #f
                                    (string-append "Unexpected syntax: " (string c) )))
          )))

(define (lexstr str)
  (let ([input (open-input-string str)])
    (λ () (get-next-token input))))

(define (get-all-tokens lex)
  (let* ([tok (lex)]
        [typ (token-type tok)])
    (if (eq? typ 'eof)
        '()
        (cons tok (get-all-tokens lex)))))

;This is the lexer (above) now we need a parser (below)!
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;Want a function that looks at list of tokens (maybe one at a time by calling lexstr and decides what to do)

;;(define (parser-digit lex)


(define (parser lex)
  (let* ([tok (lex)]
         [typ (token-type tok)]
         [val (token-repr tok)])
    (cond ([eq? typ 'eof] '())
          ([eq? typ 'digit] (ast-node (get-num-help val)))
          ([eq? typ 'lparen] (parser-expr-op-expr lex))
          (else (error "Invalid Token: " val)))))

(define (get-num-help v)
  (cond ([eq? v #\0] 0)
        ([eq? v #\1] 1)
        ([eq? v #\2] 2)
        ([eq? v #\3] 3)
        ([eq? v #\4] 4)
        ([eq? v #\5] 5)
        ([eq? v #\6] 6)
        ([eq? v #\7] 7)
        ([eq? v #\8] 8)
        ([eq? v #\9] 9)))



; value node for numbers
(struct ast-node (val) #:transparent)
; expression nodes for operators 
(struct ast-expr-node (operator left-child right-child) #:transparent)

(define (eval ast)
  (match ast
    ([ast-node v] v)
    ([ast-expr-node #\+ lc rc] (+ (eval lc) (eval rc)))
    ([ast-expr-node #\* lc rc] (* (eval lc) (eval rc)))))
     


(define (parser-expr-op-expr lex)
  (let* ([e1 (parser lex)]
         [op (parser-operator lex)]
         [e2 (parser lex)]
         [tok (lex)]
         [typ (token-type tok)]
         [val (token-repr tok)])
    (if (eq? typ 'rparen)
        (ast-expr-node op e1 e2)
        (error "Syntax Error: Invalid Parentheses"))))

(define (parser-operator lex)
  (let* ([tok (lex)]
         [typ (token-type tok)]
         [val (token-repr tok)])
    (if (eq? typ 'op)
        val
        (error "Invalid or No given Operation: " val))))
  

(define (evals ast)
  (cond ([number? ast] ast)
        ([list? ast] (if (eq? (length ast) 1) (first ast)
                         (if (eq? (first ast) #\+)
                         (+ (eval (first (rest ast)))
                            (eval (rest (rest ast))))
                         (* (eval (first (rest ast)))
                            (eval (rest (rest ast)))))))))
(define (evalstr s)
  (eval (parser (lexstr s))))


;Not error checking properly





















  