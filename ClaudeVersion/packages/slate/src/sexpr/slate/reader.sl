;; Slate Reader - Written in S-expressions
;;
;; Transforms tokens into syntax objects (S-expressions with source locations).
;; This is a Pratt parser that handles Slate's expression syntax.
;;
;; Syntax objects: (syntax datum line col)
;; - datum can be: number, string, symbol, or list of syntax objects

;; ============ Syntax Object Constructors ============

(define (make-syntax datum line col)
  (list 'syntax datum line col))

(define (syntax-datum stx)
  (if (and (pair? stx) (equal? (car stx) 'syntax))
      (nth stx 1)
      stx))

(define (syntax-line stx)
  (if (and (pair? stx) (equal? (car stx) 'syntax))
      (nth stx 2)
      0))

(define (syntax-col stx)
  (if (and (pair? stx) (equal? (car stx) 'syntax))
      (nth stx 3)
      0))

(define (make-stx-sym name line col)
  (make-syntax (list 'symbol name) line col))

(define (make-stx-num value line col)
  (make-syntax value line col))

(define (make-stx-str value line col)
  (make-syntax value line col))

(define (make-stx-list elements line col)
  (make-syntax elements line col))

;; ============ Token Access ============

(define (tok-type tok) (nth tok 0))
(define (tok-lexeme tok) (nth tok 1))
(define (tok-line tok) (nth tok 2))
(define (tok-col tok) (nth tok 3))

;; Check if token type matches (token types are stored as ("symbol" "TYPE_NAME"))
(define (tok-type-is? tok type)
  (let ((t (tok-type tok)))
    (and (pair? t)
         (string=? (car t) "symbol")
         (string=? (nth t 1) type))))

;; ============ Parser State ============

;; State: (tokens pos)
(define (make-parser tokens)
  (list tokens 0))

(define (parser-tokens p) (nth p 0))
(define (parser-pos p) (nth p 1))

(define (parser-set-pos p pos)
  (list (parser-tokens p) pos))

(define (current-token p)
  (nth (parser-tokens p) (parser-pos p)))

(define (peek-token p)
  (current-token p))

(define (at-end? p)
  (tok-type-is? (current-token p) "EOF"))

(define (advance p)
  (cons (current-token p) (parser-set-pos p (+ (parser-pos p) 1))))

(define (check p type)
  (tok-type-is? (current-token p) type))

(define (consume p type msg)
  (if (check p type)
      (advance p)
      (error (string-append msg " at line " (number->string (tok-line (current-token p)))))))

;; Skip NEWLINE tokens
(define (skip-newlines p)
  (if (check p "NEWLINE")
      (skip-newlines (cdr (advance p)))
      p))

;; ============ Operator Precedence ============

(define (prefix-precedence type)
  (cond
    ((string=? type "MINUS") 14)
    ((string=? type "NOT") 3)
    ((string=? type "BANG") 14)
    (else 0)))

(define (infix-precedence type)
  (cond
    ((string=? type "EQUALS") 0)  ; Assignment has lowest precedence
    ((string=? type "OR") 1)
    ((string=? type "AND") 2)
    ((string=? type "DOUBLE_EQUALS") 4)
    ((string=? type "NOT_EQUALS") 4)
    ((string=? type "LESS") 5)
    ((string=? type "LESS_EQUALS") 5)
    ((string=? type "GREATER") 5)
    ((string=? type "GREATER_EQUALS") 5)
    ((string=? type "DOUBLE_QUESTION") 6)
    ((string=? type "RANGE") 7)
    ((string=? type "RANGE_INCLUSIVE") 7)
    ((string=? type "PLUS") 8)
    ((string=? type "MINUS") 8)
    ((string=? type "STAR") 9)
    ((string=? type "SLASH") 9)
    ((string=? type "PERCENT") 9)
    ((string=? type "CARET") 10)
    ((string=? type "DOT") 12)
    ((string=? type "QUESTION_DOT") 12)
    ((string=? type "QUESTION_BRACKET") 12)
    ((string=? type "LBRACKET") 12)
    ((string=? type "LPAREN") 12)
    (else 0)))

(define (is-right-assoc type)
  (or (string=? type "CARET")
      (string=? type "DOUBLE_QUESTION")
      (string=? type "EQUALS")))

;; ============ Core Parser ============

;; Parse a program (list of top-level statements)
(define (parse-program p)
  (define (parse-stmts p acc)
    (let ((p1 (skip-newlines p)))
      (if (at-end? p1)
          (cons (reverse acc) p1)
          (let ((result (parse-statement p1)))
            (parse-stmts (cdr result) (cons (car result) acc))))))
  (parse-stmts p '()))

;; Parse a single statement
(define (parse-statement p)
  (let ((p1 (skip-newlines p)))
    (cond
      ((check p1 "LET") (parse-let p1))
      ((check p1 "VAR") (parse-var p1))
      ((check p1 "FN") (parse-fn p1))
      ((check p1 "IF") (parse-if p1))
      ((check p1 "WHILE") (parse-while p1))
      ((check p1 "FOR") (parse-for p1))
      ((check p1 "MATCH") (parse-match p1))
      ((check p1 "RETURN") (parse-return p1))
      ((check p1 "BREAK") (parse-break p1))
      ((check p1 "CONTINUE") (parse-continue p1))
      ((check p1 "IMPORT") (parse-import p1))
      ((check p1 "EMIT") (parse-emit p1))
      ((check p1 "ON") (parse-on p1))
      (else (parse-expression-stmt p1)))))

;; Parse optional type annotation: `: TypeName`
(define (parse-type-annotation p)
  (if (check p "COLON")
      (let* ((p1 (cdr (advance p)))  ; consume :
             (result (consume p1 "IDENTIFIER" "Expected type name after ':'"))
             (type-tok (car result))
             (p2 (cdr result)))
        ;; Check for generic parameters: Type[T, U]
        (if (check p2 "LBRACKET")
            (let* ((p3 (cdr (advance p2)))  ; consume [
                   (params-result (parse-type-params p3))
                   (type-params (car params-result))
                   (p4 (cdr params-result))
                   (result2 (consume p4 "RBRACKET" "Expected ']'")))
              (cons (make-stx-list
                     (cons (make-stx-sym (tok-lexeme type-tok) (tok-line type-tok) (tok-col type-tok))
                           type-params)
                     (tok-line type-tok) (tok-col type-tok))
                    (cdr result2)))
            (cons (make-stx-sym (tok-lexeme type-tok) (tok-line type-tok) (tok-col type-tok)) p2)))
      (cons #f p)))

;; Parse type parameters inside brackets
(define (parse-type-params p)
  (define (parse-params p acc)
    (let* ((result (consume p "IDENTIFIER" "Expected type parameter"))
           (type-tok (car result))
           (p1 (cdr result))
           (type-sym (make-stx-sym (tok-lexeme type-tok) (tok-line type-tok) (tok-col type-tok))))
      (if (check p1 "COMMA")
          (parse-params (cdr (advance p1)) (cons type-sym acc))
          (cons (reverse (cons type-sym acc)) p1))))
  (parse-params p '()))

;; Parse let binding (with optional type annotation)
(define (parse-let p)
  (let* ((result1 (advance p))  ; consume LET
         (let-tok (car result1))
         (p1 (cdr result1))
         (result2 (consume p1 "IDENTIFIER" "Expected identifier after 'let'"))
         (name-tok (car result2))
         (p2 (cdr result2))
         ;; Check for type annotation
         (type-result (parse-type-annotation p2))
         (type-ann (car type-result))
         (p3 (cdr type-result))
         (result3 (consume p3 "EQUALS" "Expected '=' after identifier"))
         (p4 (cdr result3))
         (result4 (parse-expression p4 0)))
    (if type-ann
        (cons (make-stx-list
               (list (make-stx-sym "let" (tok-line let-tok) (tok-col let-tok))
                     (make-stx-sym (tok-lexeme name-tok) (tok-line name-tok) (tok-col name-tok))
                     type-ann
                     (car result4))
               (tok-line let-tok) (tok-col let-tok))
              (cdr result4))
        (cons (make-stx-list
               (list (make-stx-sym "let" (tok-line let-tok) (tok-col let-tok))
                     (make-stx-sym (tok-lexeme name-tok) (tok-line name-tok) (tok-col name-tok))
                     (car result4))
               (tok-line let-tok) (tok-col let-tok))
              (cdr result4)))))

;; Parse var binding
(define (parse-var p)
  (let* ((result1 (advance p))  ; consume VAR
         (var-tok (car result1))
         (p1 (cdr result1))
         (result2 (consume p1 "IDENTIFIER" "Expected identifier after 'var'"))
         (name-tok (car result2))
         (p2 (cdr result2))
         (result3 (consume p2 "EQUALS" "Expected '=' after identifier"))
         (p3 (cdr result3))
         (result4 (parse-expression p3 0)))
    (cons (make-stx-list
           (list (make-stx-sym "var" (tok-line var-tok) (tok-col var-tok))
                 (make-stx-sym (tok-lexeme name-tok) (tok-line name-tok) (tok-col name-tok))
                 (car result4))
           (tok-line var-tok) (tok-col var-tok))
          (cdr result4))))

;; Parse function definition
(define (parse-fn p)
  (let* ((result1 (advance p))  ; consume FN
         (fn-tok (car result1))
         (p1 (cdr result1))
         (result2 (consume p1 "IDENTIFIER" "Expected function name"))
         (name-tok (car result2))
         (p2 (cdr result2))
         ;; Parse parameters (zero or more identifiers before colon)
         (params-result (parse-params p2))
         (params (car params-result))
         (p3 (cdr params-result))
         (result3 (consume p3 "COLON" "Expected ':' after function params"))
         (p4 (cdr result3))
         ;; Parse body (indented block)
         (body-result (parse-block p4))
         (body (car body-result))
         (p5 (cdr body-result)))
    (cons (make-stx-list
           (list (make-stx-sym "fn" (tok-line fn-tok) (tok-col fn-tok))
                 (make-stx-sym (tok-lexeme name-tok) (tok-line name-tok) (tok-col name-tok))
                 (make-stx-list params (tok-line fn-tok) (tok-col fn-tok))
                 (make-stx-list (cons (make-stx-sym "begin" (tok-line fn-tok) (tok-col fn-tok)) body)
                               (tok-line fn-tok) (tok-col fn-tok)))
           (tok-line fn-tok) (tok-col fn-tok))
          p5)))

;; Parse function parameters (including ...rest)
(define (parse-params p)
  (define (parse-param-list p acc)
    (cond
      ;; Rest parameter: ...name
      ((check p "SPREAD")
       (let* ((result1 (advance p))  ; consume ...
              (spread-tok (car result1))
              (p1 (cdr result1))
              (result2 (consume p1 "IDENTIFIER" "Expected identifier after '...'"))
              (name-tok (car result2))
              (p2 (cdr result2))
              (rest-param (make-stx-list
                           (list (make-stx-sym "rest" (tok-line spread-tok) (tok-col spread-tok))
                                 (make-stx-sym (tok-lexeme name-tok) (tok-line name-tok) (tok-col name-tok)))
                           (tok-line spread-tok) (tok-col spread-tok))))
         ;; Rest must be last parameter
         (cons (reverse (cons rest-param acc)) p2)))
      ;; Regular parameter
      ((check p "IDENTIFIER")
       (let* ((result (advance p))
              (param-tok (car result))
              (p1 (cdr result)))
         (parse-param-list p1 (cons (make-stx-sym (tok-lexeme param-tok)
                                                  (tok-line param-tok)
                                                  (tok-col param-tok))
                                   acc))))
      ;; End of params
      (else
       (cons (reverse acc) p))))
  (parse-param-list p '()))

;; Parse indented block
(define (parse-block p)
  (let ((p1 (skip-newlines p)))
    (if (check p1 "INDENT")
        (let ((p2 (cdr (advance p1))))  ; consume INDENT
          (define (parse-block-stmts p acc)
            (let ((p1 (skip-newlines p)))
              (cond
                ((check p1 "DEDENT")
                 (cons (reverse acc) (cdr (advance p1))))
                ((at-end? p1)
                 (cons (reverse acc) p1))
                (else
                 (let ((result (parse-statement p1)))
                   (parse-block-stmts (cdr result) (cons (car result) acc)))))))
          (parse-block-stmts p2 '()))
        ;; Single expression on same line
        (let ((result (parse-expression p1 0)))
          (cons (list (car result)) (cdr result))))))

;; Parse if expression
(define (parse-if p)
  (let* ((result1 (advance p))  ; consume IF
         (if-tok (car result1))
         (p1 (cdr result1))
         (cond-result (parse-expression p1 0))
         (condition (car cond-result))
         (p2 (cdr cond-result))
         (result2 (consume p2 "COLON" "Expected ':' after condition"))
         (p3 (cdr result2))
         (then-result (parse-block p3))
         (then-body (car then-result))
         (p4 (cdr then-result))
         (p5 (skip-newlines p4)))
    (if (check p5 "ELSE")
        (let* ((p6 (cdr (advance p5)))  ; consume ELSE
               (p7 (skip-newlines p6)))
          (if (check p7 "IF")
              ;; else if
              (let ((elif-result (parse-if p7)))
                (cons (make-stx-list
                       (list (make-stx-sym "if" (tok-line if-tok) (tok-col if-tok))
                             condition
                             (make-stx-list (cons (make-stx-sym "begin" (tok-line if-tok) (tok-col if-tok)) then-body)
                                           (tok-line if-tok) (tok-col if-tok))
                             (car elif-result))
                       (tok-line if-tok) (tok-col if-tok))
                      (cdr elif-result)))
              ;; else block
              (let* ((result3 (consume p7 "COLON" "Expected ':' after else"))
                     (p8 (cdr result3))
                     (else-result (parse-block p8))
                     (else-body (car else-result))
                     (p9 (cdr else-result)))
                (cons (make-stx-list
                       (list (make-stx-sym "if" (tok-line if-tok) (tok-col if-tok))
                             condition
                             (make-stx-list (cons (make-stx-sym "begin" (tok-line if-tok) (tok-col if-tok)) then-body)
                                           (tok-line if-tok) (tok-col if-tok))
                             (make-stx-list (cons (make-stx-sym "begin" (tok-line if-tok) (tok-col if-tok)) else-body)
                                           (tok-line if-tok) (tok-col if-tok)))
                       (tok-line if-tok) (tok-col if-tok))
                      p9))))
        (cons (make-stx-list
               (list (make-stx-sym "if" (tok-line if-tok) (tok-col if-tok))
                     condition
                     (make-stx-list (cons (make-stx-sym "begin" (tok-line if-tok) (tok-col if-tok)) then-body)
                                   (tok-line if-tok) (tok-col if-tok)))
               (tok-line if-tok) (tok-col if-tok))
              p5))))

;; Parse while loop
(define (parse-while p)
  (let* ((result1 (advance p))  ; consume WHILE
         (while-tok (car result1))
         (p1 (cdr result1))
         (cond-result (parse-expression p1 0))
         (condition (car cond-result))
         (p2 (cdr cond-result))
         (result2 (consume p2 "COLON" "Expected ':' after condition"))
         (p3 (cdr result2))
         (body-result (parse-block p3))
         (body (car body-result))
         (p4 (cdr body-result)))
    (cons (make-stx-list
           (list (make-stx-sym "while" (tok-line while-tok) (tok-col while-tok))
                 condition
                 (make-stx-list (cons (make-stx-sym "begin" (tok-line while-tok) (tok-col while-tok)) body)
                               (tok-line while-tok) (tok-col while-tok)))
           (tok-line while-tok) (tok-col while-tok))
          p4)))

;; Parse for loop
(define (parse-for p)
  (let* ((result1 (advance p))  ; consume FOR
         (for-tok (car result1))
         (p1 (cdr result1))
         (result2 (consume p1 "IDENTIFIER" "Expected loop variable"))
         (var-tok (car result2))
         (p2 (cdr result2))
         (result3 (consume p2 "IN" "Expected 'in' after loop variable"))
         (p3 (cdr result3))
         (iter-result (parse-expression p3 0))
         (iterable (car iter-result))
         (p4 (cdr iter-result))
         (result4 (consume p4 "COLON" "Expected ':' after iterable"))
         (p5 (cdr result4))
         (body-result (parse-block p5))
         (body (car body-result))
         (p6 (cdr body-result)))
    (cons (make-stx-list
           (list (make-stx-sym "for" (tok-line for-tok) (tok-col for-tok))
                 (make-stx-sym (tok-lexeme var-tok) (tok-line var-tok) (tok-col var-tok))
                 iterable
                 (make-stx-list (cons (make-stx-sym "begin" (tok-line for-tok) (tok-col for-tok)) body)
                               (tok-line for-tok) (tok-col for-tok)))
           (tok-line for-tok) (tok-col for-tok))
          p6)))

;; Parse match expression
(define (parse-match p)
  (let* ((result1 (advance p))  ; consume MATCH
         (match-tok (car result1))
         (p1 (cdr result1))
         (subject-result (parse-expression p1 0))
         (subject (car subject-result))
         (p2 (cdr subject-result))
         (result2 (consume p2 "COLON" "Expected ':' after match subject"))
         (p3 (skip-newlines (cdr result2)))
         (result3 (consume p3 "INDENT" "Expected indented match arms"))
         (p4 (cdr result3))
         ;; Parse arms
         (arms-result (parse-match-arms p4))
         (arms (car arms-result))
         (p5 (cdr arms-result)))
    (cons (make-stx-list
           (cons (make-stx-sym "match" (tok-line match-tok) (tok-col match-tok))
                 (cons subject arms))
           (tok-line match-tok) (tok-col match-tok))
          p5)))

;; Parse match arms
(define (parse-match-arms p)
  (define (parse-arms p acc)
    (let ((p1 (skip-newlines p)))
      (cond
        ((check p1 "DEDENT")
         (cons (reverse acc) (cdr (advance p1))))
        ((at-end? p1)
         (cons (reverse acc) p1))
        (else
         (let ((arm-result (parse-match-arm p1)))
           (parse-arms (cdr arm-result) (cons (car arm-result) acc)))))))
  (parse-arms p '()))

;; Parse a single match arm: pattern [if guard] => body
(define (parse-match-arm p)
  (let* ((pattern-result (parse-pattern p))
         (pattern (car pattern-result))
         (p1 (cdr pattern-result))
         ;; Check for guard
         (has-guard (check p1 "IF"))
         (guard-result (if has-guard
                          (let* ((p2 (cdr (advance p1)))  ; consume IF
                                 (g (parse-expression p2 0)))
                            (cons (car g) (cdr g)))
                          (cons #f p1)))
         (guard (car guard-result))
         (p2 (cdr guard-result))
         (result2 (consume p2 "ARROW" "Expected '=>' after pattern"))
         (p3 (cdr result2))
         (body-result (parse-expression p3 0))
         (body (car body-result))
         (p4 (cdr body-result)))
    (if guard
        (cons (make-stx-list (list pattern guard body) 0 0) p4)
        (cons (make-stx-list (list pattern body) 0 0) p4))))

;; Parse a pattern (simplified for now)
(define (parse-pattern p)
  (cond
    ((check p "IDENTIFIER")
     (let* ((result (advance p))
            (tok (car result)))
       (if (string=? (tok-lexeme tok) "_")
           (cons (make-stx-sym "_" (tok-line tok) (tok-col tok)) (cdr result))
           (cons (make-stx-sym (tok-lexeme tok) (tok-line tok) (tok-col tok)) (cdr result)))))
    ((check p "NUMBER")
     (let* ((result (advance p))
            (tok (car result)))
       (cons (make-stx-num (string->number (tok-lexeme tok)) (tok-line tok) (tok-col tok))
             (cdr result))))
    ((check p "STRING")
     (let* ((result (advance p))
            (tok (car result)))
       (cons (make-stx-str (tok-lexeme tok) (tok-line tok) (tok-col tok))
             (cdr result))))
    ((check p "TRUE")
     (let* ((result (advance p))
            (tok (car result)))
       (cons (make-stx-sym "true" (tok-line tok) (tok-col tok)) (cdr result))))
    ((check p "FALSE")
     (let* ((result (advance p))
            (tok (car result)))
       (cons (make-stx-sym "false" (tok-line tok) (tok-col tok)) (cdr result))))
    ((check p "LBRACKET")
     (parse-list-pattern p))
    ((check p "LBRACE")
     (parse-record-pattern p))
    (else
     (error "Expected pattern"))))

;; Parse list pattern [a, b, ..rest]
(define (parse-list-pattern p)
  (let* ((result1 (advance p))  ; consume [
         (tok (car result1))
         (p1 (cdr result1)))
    (if (check p1 "RBRACKET")
        (cons (make-stx-list (list (make-stx-sym "list-pattern" (tok-line tok) (tok-col tok)))
                            (tok-line tok) (tok-col tok))
              (cdr (advance p1)))
        (let* ((elems-result (parse-pattern-list p1))
               (elems (car elems-result))
               (p2 (cdr elems-result))
               (result2 (consume p2 "RBRACKET" "Expected ']'")))
          (cons (make-stx-list
                 (cons (make-stx-sym "list-pattern" (tok-line tok) (tok-col tok)) elems)
                 (tok-line tok) (tok-col tok))
                (cdr result2))))))

(define (parse-pattern-list p)
  (define (parse-elems p acc)
    (let ((p1 (skip-newlines p)))
      ;; Check for rest pattern ..name
      (if (check p1 "RANGE")
          (let* ((p2 (cdr (advance p1)))  ; consume ..
                 (result (consume p2 "IDENTIFIER" "Expected identifier after '..'"))
                 (rest-tok (car result))
                 (p3 (cdr result)))
            (cons (reverse (cons (make-stx-list
                                  (list (make-stx-sym "rest" 0 0)
                                        (make-stx-sym (tok-lexeme rest-tok) (tok-line rest-tok) (tok-col rest-tok)))
                                  0 0)
                                acc))
                  p3))
          (let* ((pattern-result (parse-pattern p1))
                 (pattern (car pattern-result))
                 (p2 (cdr pattern-result)))
            (if (check p2 "COMMA")
                (parse-elems (cdr (advance p2)) (cons pattern acc))
                (cons (reverse (cons pattern acc)) p2))))))
  (parse-elems p '()))

;; Parse record pattern {x, y: pattern}
(define (parse-record-pattern p)
  (let* ((result1 (advance p))  ; consume {
         (tok (car result1))
         (p1 (cdr result1)))
    (if (check p1 "RBRACE")
        (cons (make-stx-list (list (make-stx-sym "record-pattern" (tok-line tok) (tok-col tok)))
                            (tok-line tok) (tok-col tok))
              (cdr (advance p1)))
        (let* ((fields-result (parse-record-pattern-fields p1))
               (fields (car fields-result))
               (p2 (cdr fields-result))
               (result2 (consume p2 "RBRACE" "Expected '}'")))
          (cons (make-stx-list
                 (cons (make-stx-sym "record-pattern" (tok-line tok) (tok-col tok)) fields)
                 (tok-line tok) (tok-col tok))
                (cdr result2))))))

(define (parse-record-pattern-fields p)
  (define (parse-fields p acc)
    (let* ((result1 (consume p "IDENTIFIER" "Expected field name"))
           (name-tok (car result1))
           (p1 (cdr result1)))
      (if (check p1 "COLON")
          ;; key: pattern
          (let* ((p2 (cdr (advance p1)))
                 (pattern-result (parse-pattern p2))
                 (pattern (car pattern-result))
                 (p3 (cdr pattern-result))
                 (field (make-stx-list
                         (list (make-stx-sym (tok-lexeme name-tok) (tok-line name-tok) (tok-col name-tok))
                               pattern)
                         0 0)))
            (if (check p3 "COMMA")
                (parse-fields (cdr (advance p3)) (cons field acc))
                (cons (reverse (cons field acc)) p3)))
          ;; Shorthand: key (pattern is identifier with same name)
          (let ((field (make-stx-list
                        (list (make-stx-sym (tok-lexeme name-tok) (tok-line name-tok) (tok-col name-tok))
                              (make-stx-sym (tok-lexeme name-tok) (tok-line name-tok) (tok-col name-tok)))
                        0 0)))
            (if (check p1 "COMMA")
                (parse-fields (cdr (advance p1)) (cons field acc))
                (cons (reverse (cons field acc)) p1))))))
  (parse-fields p '()))

;; Parse return statement
(define (parse-return p)
  (let* ((result1 (advance p))  ; consume RETURN
         (ret-tok (car result1))
         (p1 (cdr result1)))
    (if (or (check p1 "NEWLINE") (check p1 "DEDENT") (at-end? p1))
        (cons (make-stx-list
               (list (make-stx-sym "return" (tok-line ret-tok) (tok-col ret-tok)))
               (tok-line ret-tok) (tok-col ret-tok))
              p1)
        (let ((expr-result (parse-expression p1 0)))
          (cons (make-stx-list
                 (list (make-stx-sym "return" (tok-line ret-tok) (tok-col ret-tok))
                       (car expr-result))
                 (tok-line ret-tok) (tok-col ret-tok))
                (cdr expr-result))))))

;; Parse break
(define (parse-break p)
  (let* ((result (advance p))
         (tok (car result)))
    (cons (make-stx-sym "break" (tok-line tok) (tok-col tok))
          (cdr result))))

;; Parse continue
(define (parse-continue p)
  (let* ((result (advance p))
         (tok (car result)))
    (cons (make-stx-sym "continue" (tok-line tok) (tok-col tok))
          (cdr result))))

;; Parse import
(define (parse-import p)
  (let* ((result1 (advance p))  ; consume IMPORT
         (imp-tok (car result1))
         (p1 (cdr result1))
         (result2 (consume p1 "IDENTIFIER" "Expected module name"))
         (name-tok (car result2))
         (p2 (cdr result2)))
    (cons (make-stx-list
           (list (make-stx-sym "import" (tok-line imp-tok) (tok-col imp-tok))
                 (make-stx-str (tok-lexeme name-tok) (tok-line name-tok) (tok-col name-tok)))
           (tok-line imp-tok) (tok-col imp-tok))
          p2)))

;; Parse signal path: @name.name.name -> (signal "name" "name" "name")
(define (parse-signal-path p)
  (let* ((result1 (consume p "AT" "Expected '@' for signal"))
         (at-tok (car result1))
         (p1 (cdr result1))
         (result2 (consume p1 "IDENTIFIER" "Expected signal name after '@'"))
         (first-tok (car result2))
         (p2 (cdr result2)))
    ;; Parse any dot-separated parts
    (define (parse-parts p parts)
      (if (check p "DOT")
          (let* ((p1 (cdr (advance p)))  ; consume DOT
                 (result (consume p1 "IDENTIFIER" "Expected identifier after '.'"))
                 (name-tok (car result))
                 (p2 (cdr result)))
            (parse-parts p2 (cons (make-stx-str (tok-lexeme name-tok) (tok-line name-tok) (tok-col name-tok)) parts)))
          (cons (reverse parts) p)))
    (let* ((parts-result (parse-parts p2 (list (make-stx-str (tok-lexeme first-tok) (tok-line first-tok) (tok-col first-tok)))))
           (parts (car parts-result))
           (p3 (cdr parts-result)))
      (cons (make-stx-list
             (cons (make-stx-sym "signal" (tok-line at-tok) (tok-col at-tok))
                   parts)
             (tok-line at-tok) (tok-col at-tok))
            p3))))

;; Parse emit statement: emit @signal data?
(define (parse-emit p)
  (let* ((result1 (advance p))  ; consume EMIT
         (emit-tok (car result1))
         (p1 (cdr result1))
         (signal-result (parse-signal-path p1))
         (signal (car signal-result))
         (p2 (cdr signal-result)))
    ;; Check for optional data expression (not newline, not at end)
    (if (or (check p2 "NEWLINE") (at-end? p2))
        (cons (make-stx-list
               (list (make-stx-sym "emit" (tok-line emit-tok) (tok-col emit-tok))
                     signal)
               (tok-line emit-tok) (tok-col emit-tok))
              p2)
        (let ((data-result (parse-expression p2 0)))
          (cons (make-stx-list
                 (list (make-stx-sym "emit" (tok-line emit-tok) (tok-col emit-tok))
                       signal
                       (car data-result))
                 (tok-line emit-tok) (tok-col emit-tok))
                (cdr data-result))))))

;; Parse on statement: on @signal filter?: body
(define (parse-on p)
  (let* ((result1 (advance p))  ; consume ON
         (on-tok (car result1))
         (p1 (cdr result1))
         (signal-result (parse-signal-path p1))
         (signal (car signal-result))
         (p2 (cdr signal-result)))
    ;; Check for optional filter expression before colon
    (if (check p2 "COLON")
        ;; No filter
        (let* ((p3 (cdr (advance p2)))  ; consume COLON
               (body-result (parse-block p3))
               (body (car body-result))
               (p4 (cdr body-result)))
          (cons (make-stx-list
                 (list (make-stx-sym "on" (tok-line on-tok) (tok-col on-tok))
                       signal
                       (make-stx-list (cons (make-stx-sym "begin" (tok-line on-tok) (tok-col on-tok)) body)
                                     (tok-line on-tok) (tok-col on-tok)))
                 (tok-line on-tok) (tok-col on-tok))
                p4))
        ;; Has filter
        (let* ((filter-result (parse-expression p2 0))
               (filter (car filter-result))
               (p3 (cdr filter-result))
               (result2 (consume p3 "COLON" "Expected ':' after signal filter"))
               (p4 (cdr result2))
               (body-result (parse-block p4))
               (body (car body-result))
               (p5 (cdr body-result)))
          (cons (make-stx-list
                 (list (make-stx-sym "on" (tok-line on-tok) (tok-col on-tok))
                       signal
                       filter
                       (make-stx-list (cons (make-stx-sym "begin" (tok-line on-tok) (tok-col on-tok)) body)
                                     (tok-line on-tok) (tok-col on-tok)))
                 (tok-line on-tok) (tok-col on-tok))
                p5)))))

;; Parse expression statement
(define (parse-expression-stmt p)
  (let ((result (parse-expression p 0)))
    ;; Check for assignment
    (if (check (cdr result) "EQUALS")
        (let* ((p1 (cdr (advance (cdr result))))
               (value-result (parse-expression p1 0)))
          (cons (make-stx-list
                 (list (make-stx-sym "set!" 0 0)
                       (car result)
                       (car value-result))
                 0 0)
                (cdr value-result)))
        result)))

;; ============ Expression Parser (Pratt) ============

(define (parse-expression p min-prec)
  (let ((result (parse-prefix p)))
    (parse-infix (cdr result) (car result) min-prec)))

(define (parse-prefix p)
  (let ((tok (current-token p)))
    (cond
      ((check p "NUMBER")
       (let ((result (advance p)))
         (cons (make-stx-num (string->number (tok-lexeme tok))
                            (tok-line tok) (tok-col tok))
               (cdr result))))
      ((check p "STRING")
       (let ((result (advance p)))
         (cons (make-stx-str (tok-lexeme tok) (tok-line tok) (tok-col tok))
               (cdr result))))
      ((check p "TRUE")
       (let ((result (advance p)))
         (cons (make-stx-sym "true" (tok-line tok) (tok-col tok))
               (cdr result))))
      ((check p "FALSE")
       (let ((result (advance p)))
         (cons (make-stx-sym "false" (tok-line tok) (tok-col tok))
               (cdr result))))
      ((check p "NULL")
       (let ((result (advance p)))
         (cons (make-stx-sym "null" (tok-line tok) (tok-col tok))
               (cdr result))))
      ((check p "IDENTIFIER")
       (let ((result (advance p)))
         (cons (make-stx-sym (tok-lexeme tok) (tok-line tok) (tok-col tok))
               (cdr result))))
      ((check p "LPAREN")
       (parse-grouped p))
      ((check p "LBRACKET")
       (parse-list-literal p))
      ((check p "LBRACE")
       (parse-record-literal p))
      ((check p "MINUS")
       (parse-unary p "MINUS" "-"))
      ((check p "NOT")
       (parse-unary p "NOT" "not"))
      ((check p "BANG")
       (parse-unary p "BANG" "!"))
      ((check p "PIPE")
       (parse-lambda p))
      (else
       (error (string-append "Unexpected token: " (tok-lexeme tok)))))))

(define (parse-unary p type op-name)
  (let* ((result1 (advance p))
         (tok (car result1))
         (p1 (cdr result1))
         (prec (prefix-precedence type))
         (operand-result (parse-expression p1 prec)))
    (cons (make-stx-list
           (list (make-stx-sym op-name (tok-line tok) (tok-col tok))
                 (car operand-result))
           (tok-line tok) (tok-col tok))
          (cdr operand-result))))

(define (parse-grouped p)
  (let* ((result1 (advance p))  ; consume (
         (p1 (cdr result1))
         (expr-result (parse-expression p1 0))
         (result2 (consume (cdr expr-result) "RPAREN" "Expected ')'")))
    (cons (car expr-result) (cdr result2))))

(define (parse-list-literal p)
  (let* ((result1 (advance p))  ; consume [
         (tok (car result1))
         (p1 (cdr result1)))
    (if (check p1 "RBRACKET")
        (cons (make-stx-list
               (list (make-stx-sym "list" (tok-line tok) (tok-col tok)))
               (tok-line tok) (tok-col tok))
              (cdr (advance p1)))
        (let* ((elems-result (parse-expr-list p1))
               (elems (car elems-result))
               (p2 (cdr elems-result))
               (result2 (consume p2 "RBRACKET" "Expected ']'")))
          (cons (make-stx-list
                 (cons (make-stx-sym "list" (tok-line tok) (tok-col tok)) elems)
                 (tok-line tok) (tok-col tok))
                (cdr result2))))))

(define (parse-expr-list p)
  (define (parse-elems p acc)
    ;; Check for spread: ...expr
    (if (check p "SPREAD")
        (let* ((result1 (advance p))  ; consume ...
               (spread-tok (car result1))
               (p1 (cdr result1))
               (expr-result (parse-expression p1 0))
               (expr (car expr-result))
               (p2 (cdr expr-result))
               (spread-expr (make-stx-list
                             (list (make-stx-sym "spread" (tok-line spread-tok) (tok-col spread-tok))
                                   expr)
                             (tok-line spread-tok) (tok-col spread-tok))))
          (if (check p2 "COMMA")
              (parse-elems (cdr (advance p2)) (cons spread-expr acc))
              (cons (reverse (cons spread-expr acc)) p2)))
        ;; Regular expression
        (let* ((expr-result (parse-expression p 0))
               (expr (car expr-result))
               (p1 (cdr expr-result)))
          (if (check p1 "COMMA")
              (parse-elems (cdr (advance p1)) (cons expr acc))
              (cons (reverse (cons expr acc)) p1)))))
  (parse-elems p '()))

(define (parse-record-literal p)
  (let* ((result1 (advance p))  ; consume {
         (tok (car result1))
         (p1 (cdr result1)))
    (if (check p1 "RBRACE")
        (cons (make-stx-list
               (list (make-stx-sym "record" (tok-line tok) (tok-col tok)))
               (tok-line tok) (tok-col tok))
              (cdr (advance p1)))
        (let* ((fields-result (parse-record-fields p1))
               (fields (car fields-result))
               (p2 (cdr fields-result))
               (result2 (consume p2 "RBRACE" "Expected '}'")))
          (cons (make-stx-list
                 (cons (make-stx-sym "record" (tok-line tok) (tok-col tok)) fields)
                 (tok-line tok) (tok-col tok))
                (cdr result2))))))

(define (parse-record-fields p)
  (define (parse-fields p acc)
    (let* ((result1 (consume p "IDENTIFIER" "Expected field name"))
           (name-tok (car result1))
           (p1 (cdr result1))
           (result2 (consume p1 "COLON" "Expected ':'"))
           (p2 (cdr result2))
           (expr-result (parse-expression p2 0))
           (expr (car expr-result))
           (p3 (cdr expr-result))
           (field (make-stx-list
                   (list (make-stx-sym (tok-lexeme name-tok) (tok-line name-tok) (tok-col name-tok))
                         expr)
                   0 0)))
      (if (check p3 "COMMA")
          (parse-fields (cdr (advance p3)) (cons field acc))
          (cons (reverse (cons field acc)) p3))))
  (parse-fields p '()))

(define (parse-lambda p)
  (let* ((result1 (advance p))  ; consume |
         (tok (car result1))
         (p1 (cdr result1))
         ;; Parse params until |
         (params-result (parse-lambda-params p1))
         (params (car params-result))
         (p2 (cdr params-result))
         (result2 (consume p2 "PIPE" "Expected '|'"))
         (p3 (cdr result2))
         ;; Parse body expression
         (body-result (parse-expression p3 0))
         (body (car body-result))
         (p4 (cdr body-result)))
    (cons (make-stx-list
           (list (make-stx-sym "lambda" (tok-line tok) (tok-col tok))
                 (make-stx-list params (tok-line tok) (tok-col tok))
                 body)
           (tok-line tok) (tok-col tok))
          p4)))

(define (parse-lambda-params p)
  (define (parse-params p acc)
    (if (check p "PIPE")
        (cons (reverse acc) p)
        (let* ((result (consume p "IDENTIFIER" "Expected parameter name"))
               (tok (car result))
               (p1 (cdr result))
               (param (make-stx-sym (tok-lexeme tok) (tok-line tok) (tok-col tok))))
          (if (check p1 "COMMA")
              (parse-params (cdr (advance p1)) (cons param acc))
              (cons (reverse (cons param acc)) p1)))))
  (parse-params p '()))

(define (parse-infix p left min-prec)
  (let ((tok (current-token p)))
    (cond
      ;; Binary operators
      ((and (check p "PLUS") (>= (infix-precedence "PLUS") min-prec))
       (parse-binary p left "PLUS" "+" min-prec))
      ((and (check p "MINUS") (>= (infix-precedence "MINUS") min-prec))
       (parse-binary p left "MINUS" "-" min-prec))
      ((and (check p "STAR") (>= (infix-precedence "STAR") min-prec))
       (parse-binary p left "STAR" "*" min-prec))
      ((and (check p "SLASH") (>= (infix-precedence "SLASH") min-prec))
       (parse-binary p left "SLASH" "/" min-prec))
      ((and (check p "PERCENT") (>= (infix-precedence "PERCENT") min-prec))
       (parse-binary p left "PERCENT" "%" min-prec))
      ((and (check p "CARET") (>= (infix-precedence "CARET") min-prec))
       (parse-binary p left "CARET" "^" min-prec))
      ((and (check p "LESS") (>= (infix-precedence "LESS") min-prec))
       (parse-binary p left "LESS" "<" min-prec))
      ((and (check p "LESS_EQUALS") (>= (infix-precedence "LESS_EQUALS") min-prec))
       (parse-binary p left "LESS_EQUALS" "<=" min-prec))
      ((and (check p "GREATER") (>= (infix-precedence "GREATER") min-prec))
       (parse-binary p left "GREATER" ">" min-prec))
      ((and (check p "GREATER_EQUALS") (>= (infix-precedence "GREATER_EQUALS") min-prec))
       (parse-binary p left "GREATER_EQUALS" ">=" min-prec))
      ((and (check p "DOUBLE_EQUALS") (>= (infix-precedence "DOUBLE_EQUALS") min-prec))
       (parse-binary p left "DOUBLE_EQUALS" "==" min-prec))
      ((and (check p "NOT_EQUALS") (>= (infix-precedence "NOT_EQUALS") min-prec))
       (parse-binary p left "NOT_EQUALS" "!=" min-prec))
      ((and (check p "AND") (>= (infix-precedence "AND") min-prec))
       (parse-binary p left "AND" "and" min-prec))
      ((and (check p "OR") (>= (infix-precedence "OR") min-prec))
       (parse-binary p left "OR" "or" min-prec))
      ((and (check p "RANGE") (>= (infix-precedence "RANGE") min-prec))
       (parse-binary p left "RANGE" "range" min-prec))
      ((and (check p "RANGE_INCLUSIVE") (>= (infix-precedence "RANGE_INCLUSIVE") min-prec))
       (parse-binary p left "RANGE_INCLUSIVE" "range-inclusive" min-prec))
      ((and (check p "DOUBLE_QUESTION") (>= (infix-precedence "DOUBLE_QUESTION") min-prec))
       (parse-binary p left "DOUBLE_QUESTION" "??" min-prec))

      ;; Assignment
      ((and (check p "EQUALS") (>= (infix-precedence "EQUALS") min-prec))
       (parse-binary p left "EQUALS" "assign" min-prec))

      ;; Member access
      ((and (check p "DOT") (>= (infix-precedence "DOT") min-prec))
       (parse-member-access p left #f min-prec))

      ;; Optional member access
      ((and (check p "QUESTION_DOT") (>= (infix-precedence "QUESTION_DOT") min-prec))
       (parse-member-access p left #t min-prec))

      ;; Index access
      ((and (check p "LBRACKET") (>= (infix-precedence "LBRACKET") min-prec))
       (parse-index-access p left #f min-prec))

      ;; Optional index access
      ((and (check p "QUESTION_BRACKET") (>= (infix-precedence "QUESTION_BRACKET") min-prec))
       (parse-index-access p left #t min-prec))

      ;; Function call
      ((and (check p "LPAREN") (>= (infix-precedence "LPAREN") min-prec))
       (parse-call p left min-prec))

      (else (cons left p)))))

(define (parse-binary p left type op-name min-prec)
  (let* ((result1 (advance p))  ; consume operator
         (tok (car result1))
         (p1 (cdr result1))
         (prec (infix-precedence type))
         (next-prec (if (is-right-assoc type) prec (+ prec 1)))
         (right-result (parse-expression p1 next-prec))
         (right (car right-result))
         (p2 (cdr right-result))
         (expr (make-stx-list
                (list (make-stx-sym op-name (tok-line tok) (tok-col tok))
                      left
                      right)
                (tok-line tok) (tok-col tok))))
    (parse-infix p2 expr min-prec)))

(define (parse-member-access p left optional min-prec)
  (let* ((result1 (advance p))  ; consume . or ?.
         (tok (car result1))
         (p1 (cdr result1))
         (result2 (consume p1 "IDENTIFIER" "Expected property name"))
         (name-tok (car result2))
         (p2 (cdr result2))
         (op-name (if optional "?." "."))
         (expr (make-stx-list
                (list (make-stx-sym op-name (tok-line tok) (tok-col tok))
                      left
                      (make-stx-sym (tok-lexeme name-tok) (tok-line name-tok) (tok-col name-tok)))
                (tok-line tok) (tok-col tok))))
    (parse-infix p2 expr min-prec)))

(define (parse-index-access p left optional min-prec)
  (let* ((result1 (advance p))  ; consume [ or ?[
         (tok (car result1))
         (p1 (cdr result1))
         (index-result (parse-expression p1 0))
         (index (car index-result))
         (p2 (cdr index-result))
         (result2 (consume p2 "RBRACKET" "Expected ']'"))
         (p3 (cdr result2))
         (op-name (if optional "?index" "index"))
         (expr (make-stx-list
                (list (make-stx-sym op-name (tok-line tok) (tok-col tok))
                      left
                      index)
                (tok-line tok) (tok-col tok))))
    (parse-infix p3 expr min-prec)))

(define (parse-call p left min-prec)
  (let* ((result1 (advance p))  ; consume (
         (tok (car result1))
         (p1 (cdr result1)))
    (if (check p1 "RPAREN")
        (let ((p2 (cdr (advance p1)))
              (expr (make-stx-list
                     (list (make-stx-sym "call" (tok-line tok) (tok-col tok))
                           left)
                     (tok-line tok) (tok-col tok))))
          (parse-infix p2 expr min-prec))
        (let* ((args-result (parse-expr-list p1))
               (args (car args-result))
               (p2 (cdr args-result))
               (result2 (consume p2 "RPAREN" "Expected ')'"))
               (p3 (cdr result2))
               (expr (make-stx-list
                      (cons (make-stx-sym "call" (tok-line tok) (tok-col tok))
                            (cons left args))
                      (tok-line tok) (tok-col tok))))
          (parse-infix p3 expr min-prec)))))

;; ============ Main Entry Point ============

;; Parse tokens into syntax objects
(define (parse tokens)
  (let* ((parser (make-parser tokens))
         (result (parse-program parser)))
    (car result)))

parse
