;; Slate Macro Expander - Written in S-expressions
;;
;; Takes syntax objects from the reader and expands macros into core forms.
;; Supports pattern-based macros (syntax-rules style).
;;
;; Core forms (not expandable):
;;   let, var, fn, lambda, if, begin, set!, quote
;;   and, or, list, record, call, index, .
;;   while, for, match, return, break, continue, import
;;
;; Syntax objects: (('symbol 'syntax) datum line col)
;; Symbol syntax: (('symbol 'symbol) name)

;; ============ Syntax Object Helpers ============

;; Safe string comparison
(define (str=? a b)
  (and (string? a) (string? b) (string=? a b)))

;; Check if a value is a syntax object
;; Structure: (("symbol" "syntax") datum line col)
(define (syntax? x)
  (and (pair? x)
       (>= (length x) 4)
       (let ((tag (car x)))
         (and (pair? tag)
              (= (length tag) 2)
              (str=? (car tag) "symbol")
              (str=? (nth tag 1) "syntax")))))

;; Get the datum from a syntax object
(define (syntax-datum stx)
  (if (syntax? stx)
      (nth stx 1)
      stx))

;; Get source location
(define (syntax-line stx)
  (if (syntax? stx) (nth stx 2) 0))

(define (syntax-col stx)
  (if (syntax? stx) (nth stx 3) 0))

;; Make a syntax object
(define (make-syntax datum line col)
  (list (list "symbol" "syntax") datum line col))

;; Check if datum is a symbol
;; Symbol datum structure: (("symbol" "symbol") name)
(define (syntax-symbol? stx)
  (let ((d (syntax-datum stx)))
    (and (pair? d)
         (>= (length d) 2)
         (pair? (car d))
         (= (length (car d)) 2)
         (str=? (car (car d)) "symbol")
         (str=? (nth (car d) 1) "symbol"))))

;; Get symbol name from syntax
(define (syntax-symbol-name stx)
  (let ((d (syntax-datum stx)))
    (if (and (pair? d) (>= (length d) 2))
        (nth d 1)
        #f)))

;; Check if datum is a list (compound form)
;; A list datum is a pair that doesn't start with a ("symbol" ...) tag
(define (syntax-list? stx)
  (let ((d (syntax-datum stx)))
    (and (pair? d)
         (not (and (pair? (car d))
                   (str=? (car (car d)) "symbol"))))))

;; Get elements from a syntax list
(define (syntax-list-elements stx)
  (let ((d (syntax-datum stx)))
    (if (syntax-list? stx)
        d
        (list))))

;; Make a syntax symbol
(define (make-stx-sym name line col)
  (make-syntax (list (list "symbol" "symbol") name) line col))

;; Make a syntax list
(define (make-stx-list elements line col)
  (make-syntax elements line col))

;; ============ Core Forms ============

(define core-forms
  '("let" "var" "fn" "lambda" "if" "begin" "set!" "quote"
    "and" "or" "list" "record" "call" "index" "."
    "while" "for" "match" "return" "break" "continue" "import"
    "+" "-" "*" "/" "%" "^" "<" "<=" ">" ">=" "==" "!="
    "range" "range-inclusive" "??" "true" "false" "null"))

(define (core-form? name)
  (member? name core-forms))

(define (member? x lst)
  (cond
    ((null? lst) #f)
    ((str=? x (car lst)) #t)
    (else (member? x (cdr lst)))))

;; ============ Macro Environment ============

;; Macro table: list of (name . transformer) pairs
;; Transformer is a function: syntax -> syntax

(define (make-macro-env)
  '())

(define (macro-env-define env name transformer)
  (cons (cons name transformer) env))

(define (macro-env-lookup env name)
  (cond
    ((null? env) #f)
    ((str=? (car (car env)) name) (cdr (car env)))
    (else (macro-env-lookup (cdr env) name))))

;; ============ Pattern Matching ============

;; Pattern variables start with $
(define (pattern-var? name)
  (and (> (string-length name) 0)
       (string=? (substring name 0 1) "$")))

;; Ellipsis pattern ends with ...
(define (ellipsis-var? name)
  (and (> (string-length name) 3)
       (string=? (substring name (- (string-length name) 3) (string-length name)) "...")))

;; Get base name from ellipsis pattern
(define (ellipsis-base-name name)
  (substring name 0 (- (string-length name) 3)))

;; Match a pattern against a syntax object
;; Returns bindings alist or #f on failure
(define (match-pattern pattern stx)
  (match-pattern-aux pattern stx '()))

(define (match-pattern-aux pattern stx bindings)
  (cond
    ;; Pattern is a syntax object - unwrap it
    ((syntax? pattern)
     (match-pattern-aux (syntax-datum pattern) stx bindings))

    ;; Wildcard matches anything
    ((and (pair? pattern) (pair? (car pattern))
          (str=? (car (car pattern)) "symbol")
          (str=? (nth (car pattern) 1) "symbol")
          (str=? (nth pattern 1) "_"))
     bindings)

    ;; Pattern variable - bind to stx
    ((and (pair? pattern) (pair? (car pattern))
          (str=? (car (car pattern)) "symbol")
          (str=? (nth (car pattern) 1) "symbol")
          (pattern-var? (nth pattern 1)))
     (let ((var-name (nth pattern 1)))
       (if (ellipsis-var? var-name)
           ;; Ellipsis - this shouldn't happen at top level
           #f
           ;; Regular variable
           (cons (cons var-name stx) bindings))))

    ;; Literal symbol - match by name
    ((and (pair? pattern) (pair? (car pattern))
          (str=? (car (car pattern)) "symbol")
          (str=? (nth (car pattern) 1) "symbol"))
     (let ((pat-name (nth pattern 1)))
       (if (syntax-symbol? stx)
           (if (str=? pat-name (syntax-symbol-name stx))
               bindings
               #f)
           #f)))

    ;; Number pattern
    ((number? pattern)
     (let ((d (syntax-datum stx)))
       (if (and (number? d) (= pattern d))
           bindings
           #f)))

    ;; String pattern
    ((string? pattern)
     (let ((d (syntax-datum stx)))
       (if (and (string? d) (string=? pattern d))
           bindings
           #f)))

    ;; List pattern
    ((pair? pattern)
     (if (syntax-list? stx)
         (match-list-pattern pattern (syntax-list-elements stx) bindings)
         #f))

    ;; Empty list pattern
    ((null? pattern)
     (if (syntax-list? stx)
         (if (null? (syntax-list-elements stx))
             bindings
             #f)
         #f))

    (else #f)))

;; Match a list pattern against list of syntax elements
(define (match-list-pattern pattern elements bindings)
  (cond
    ;; Both empty - success
    ((and (null? pattern) (null? elements))
     bindings)

    ;; Pattern empty but elements remain - fail
    ((null? pattern)
     #f)

    ;; Check for ellipsis in pattern
    ((has-ellipsis-next? pattern)
     (match-ellipsis-pattern pattern elements bindings))

    ;; Elements empty but pattern remains - fail (unless pattern is all ellipsis)
    ((null? elements)
     #f)

    ;; Match first element, continue with rest
    (else
     (let ((new-bindings (match-pattern-aux (car pattern) (car elements) bindings)))
       (if new-bindings
           (match-list-pattern (cdr pattern) (cdr elements) new-bindings)
           #f)))))

;; Check if next pattern element is ellipsis marker
(define (has-ellipsis-next? pattern)
  (and (pair? pattern)
       (pair? (cdr pattern))
       (let ((next (nth pattern 1)))
         (and (syntax? next)
              (syntax-symbol? next)
              (str=? (syntax-symbol-name next) "...")))))

;; Match ellipsis pattern: collect remaining elements
(define (match-ellipsis-pattern pattern elements bindings)
  (let ((var-pattern (car pattern))
        (rest-pattern (cdr (cdr pattern))))  ; skip ellipsis marker
    ;; Determine how many elements the rest pattern needs
    (let ((rest-len (length rest-pattern)))
      (if (< (length elements) rest-len)
          #f
          (let* ((split-pos (- (length elements) rest-len))
                 (ellipsis-elems (take elements split-pos))
                 (rest-elems (drop elements split-pos)))
            ;; Match rest pattern first
            (let ((rest-bindings (match-list-pattern rest-pattern rest-elems bindings)))
              (if rest-bindings
                  ;; Bind ellipsis variable to list of matches
                  (match-ellipsis-elements var-pattern ellipsis-elems rest-bindings)
                  #f)))))))

;; Match a pattern variable against multiple elements for ellipsis
(define (match-ellipsis-elements var-pattern elements bindings)
  (if (and (syntax? var-pattern) (syntax-symbol? var-pattern))
      (let ((var-name (syntax-symbol-name var-pattern)))
        (if (pattern-var? var-name)
            (cons (cons (string-append var-name "...") elements) bindings)
            #f))
      #f))

;; List utilities
(define (take lst n)
  (if (or (null? lst) (<= n 0))
      '()
      (cons (car lst) (take (cdr lst) (- n 1)))))

(define (drop lst n)
  (if (or (null? lst) (<= n 0))
      lst
      (drop (cdr lst) (- n 1))))

;; ============ Template Application ============

;; Apply template with bindings to produce new syntax
(define (apply-template template bindings line col)
  (apply-template-aux template bindings line col))

(define (apply-template-aux template bindings line col)
  (cond
    ;; Syntax object - unwrap and process
    ((syntax? template)
     (let ((result (apply-template-aux (syntax-datum template) bindings
                                       (syntax-line template) (syntax-col template))))
       (if (syntax? result)
           result
           (make-syntax result (syntax-line template) (syntax-col template)))))

    ;; Symbol - check if it's a pattern variable
    ((and (pair? template) (pair? (car template))
          (str=? (car (car template)) "symbol")
          (str=? (nth (car template) 1) "symbol"))
     (let ((name (nth template 1)))
       (if (pattern-var? name)
           (let ((binding (assoc-lookup name bindings)))
             (if binding
                 binding
                 (make-stx-sym name line col)))
           (make-stx-sym name line col))))

    ;; List - expand each element
    ((pair? template)
     (make-stx-list (expand-template-list template bindings line col) line col))

    ;; Primitives pass through
    (else template)))

;; Expand a list template, handling ellipsis
(define (expand-template-list template bindings line col)
  (cond
    ((null? template) '())

    ;; Check for ellipsis: (elem ...)
    ((and (pair? (cdr template))
          (let ((next (nth template 1)))
            (and (syntax? next)
                 (syntax-symbol? next)
                 (str=? (syntax-symbol-name next) "..."))))
     ;; Expand ellipsis
     (let* ((elem-template (car template))
            (rest-template (cdr (cdr template)))
            (expanded-ellipsis (expand-ellipsis elem-template bindings line col))
            (expanded-rest (expand-template-list rest-template bindings line col)))
       (append expanded-ellipsis expanded-rest)))

    ;; Regular element
    (else
     (cons (apply-template-aux (car template) bindings line col)
           (expand-template-list (cdr template) bindings line col)))))

;; Expand an ellipsis pattern
(define (expand-ellipsis elem-template bindings line col)
  (if (and (syntax? elem-template) (syntax-symbol? elem-template))
      (let* ((var-name (syntax-symbol-name elem-template))
             (ellipsis-name (string-append var-name "..."))
             (bound-list (assoc-lookup ellipsis-name bindings)))
        (if bound-list
            bound-list
            '()))
      ;; Complex template - need to expand for each binding
      ;; For now, just return empty (would need more sophisticated handling)
      '()))

;; Association list lookup
(define (assoc-lookup key alist)
  (cond
    ((null? alist) #f)
    ((str=? (car (car alist)) key) (cdr (car alist)))
    (else (assoc-lookup key (cdr alist)))))

;; ============ Expander ============

;; Main expand function
;; Takes syntax and macro environment, returns expanded syntax
(define (expand stx env)
  (expand-aux stx env 0))

(define max-expansion-depth 1000)

(define (expand-aux stx env depth)
  (if (>= depth max-expansion-depth)
      (error "Maximum macro expansion depth exceeded"))

  (cond
    ;; Not a syntax object - return as is
    ((not (syntax? stx)) stx)

    ;; Symbol - return as is (identifier)
    ((syntax-symbol? stx) stx)

    ;; List - check for forms and macros
    ((syntax-list? stx)
     (let ((elems (syntax-list-elements stx)))
       (if (null? elems)
           stx
           (let ((head (car elems)))
             (if (syntax-symbol? head)
                 (let ((name (syntax-symbol-name head)))
                   (cond
                     ;; Quote - don't expand contents
                     ((str=? name "quote") stx)

                     ;; Core form - expand sub-expressions as appropriate
                     ((core-form? name)
                      (expand-core-form name stx env depth))

                     ;; Check for macro
                     (else
                      (let ((transformer (macro-env-lookup env name)))
                        (if transformer
                            ;; Apply macro and re-expand result
                            (let ((expanded (transformer stx)))
                              (expand-aux expanded env (+ depth 1)))
                            ;; Not a macro - expand as call
                            (expand-call stx env depth))))))
                 ;; Head is not a symbol - expand as call
                 (expand-call stx env depth))))))

    ;; Other - return as is
    (else stx)))

;; Expand a core form
(define (expand-core-form name stx env depth)
  (let ((elems (syntax-list-elements stx))
        (line (syntax-line stx))
        (col (syntax-col stx)))
    (cond
      ;; let/var: (let name value)
      ((or (str=? name "let") (str=? name "var"))
       (if (>= (length elems) 3)
           (make-stx-list
            (list (car elems)
                  (nth elems 1)
                  (expand-aux (nth elems 2) env depth))
            line col)
           stx))

      ;; fn: (fn name params body)
      ((str=? name "fn")
       (if (>= (length elems) 4)
           (make-stx-list
            (list (car elems)
                  (nth elems 1)
                  (nth elems 2)
                  (expand-aux (nth elems 3) env depth))
            line col)
           stx))

      ;; lambda: (lambda params body)
      ((str=? name "lambda")
       (if (>= (length elems) 3)
           (make-stx-list
            (list (car elems)
                  (nth elems 1)
                  (expand-aux (nth elems 2) env depth))
            line col)
           stx))

      ;; if: (if cond then else?)
      ((str=? name "if")
       (let ((expanded-elems
              (cons (car elems)
                    (map-expand (cdr elems) env depth))))
         (make-stx-list expanded-elems line col)))

      ;; begin: (begin expr...)
      ((str=? name "begin")
       (make-stx-list
        (cons (car elems)
              (map-expand (cdr elems) env depth))
        line col))

      ;; set!: (set! target value)
      ((str=? name "set!")
       (if (>= (length elems) 3)
           (make-stx-list
            (list (car elems)
                  (nth elems 1)
                  (expand-aux (nth elems 2) env depth))
            line col)
           stx))

      ;; match: (match subject clauses...)
      ((str=? name "match")
       (if (>= (length elems) 2)
           (make-stx-list
            (cons (car elems)
                  (cons (expand-aux (nth elems 1) env depth)
                        (map-expand-match-clause (drop elems 2) env depth)))
            line col)
           stx))

      ;; while: (while cond body)
      ((str=? name "while")
       (if (>= (length elems) 3)
           (make-stx-list
            (list (car elems)
                  (expand-aux (nth elems 1) env depth)
                  (expand-aux (nth elems 2) env depth))
            line col)
           stx))

      ;; for: (for var iterable body)
      ((str=? name "for")
       (if (>= (length elems) 4)
           (make-stx-list
            (list (car elems)
                  (nth elems 1)
                  (expand-aux (nth elems 2) env depth)
                  (expand-aux (nth elems 3) env depth))
            line col)
           stx))

      ;; Operators and other forms - expand all arguments
      (else
       (make-stx-list
        (cons (car elems)
              (map-expand (cdr elems) env depth))
        line col)))))

;; Expand a function call
(define (expand-call stx env depth)
  (let ((elems (syntax-list-elements stx))
        (line (syntax-line stx))
        (col (syntax-col stx)))
    (make-stx-list
     (map-expand elems env depth)
     line col)))

;; Map expand over a list of syntax objects
(define (map-expand lst env depth)
  (if (null? lst)
      '()
      (cons (expand-aux (car lst) env depth)
            (map-expand (cdr lst) env depth))))

;; Expand match clauses (pattern and body)
(define (map-expand-match-clause clauses env depth)
  (if (null? clauses)
      '()
      (let ((clause (car clauses)))
        (if (syntax-list? clause)
            (let ((clause-elems (syntax-list-elements clause)))
              (cons
               (make-stx-list
                (cons (car clause-elems)  ; pattern - don't expand
                      (map-expand (cdr clause-elems) env depth))  ; body - expand
                (syntax-line clause) (syntax-col clause))
               (map-expand-match-clause (cdr clauses) env depth)))
            (cons clause (map-expand-match-clause (cdr clauses) env depth))))))

;; ============ Syntax Rules (Pattern Macros) ============

;; Create a syntax-rules transformer
;; rules is a list of (pattern . template) pairs
(define (make-syntax-rules rules)
  (lambda (stx)
    (try-rules stx rules)))

;; Try each rule until one matches
(define (try-rules stx rules)
  (if (null? rules)
      (error (string-append "No matching macro rule for: " (syntax-to-string stx)))
      (let* ((rule (car rules))
             (pattern (car rule))
             (template (cdr rule))
             (bindings (match-pattern pattern stx)))
        (if bindings
            (apply-template template bindings (syntax-line stx) (syntax-col stx))
            (try-rules stx (cdr rules))))))

;; Convert syntax to string for error messages
(define (syntax-to-string stx)
  (cond
    ((syntax? stx)
     (datum-to-string (syntax-datum stx)))
    (else (datum-to-string stx))))

(define (datum-to-string d)
  (cond
    ((number? d) (number->string d))
    ((string? d) d)
    ((boolean? d) (if d "true" "false"))
    ((null? d) "()")
    ((and (pair? d) (pair? (car d))
          (str=? (car (car d)) "symbol")
          (str=? (nth (car d) 1) "symbol"))
     (nth d 1))
    ((pair? d)
     (string-append "(" (list-to-string d) ")"))
    (else "<?>")))

(define (list-to-string lst)
  (if (null? lst)
      ""
      (if (null? (cdr lst))
          (syntax-to-string (car lst))
          (string-append (syntax-to-string (car lst)) " " (list-to-string (cdr lst))))))

;; ============ Program Expansion ============

;; Expand a program (list of top-level forms)
(define (expand-program stx-list env)
  (if (null? stx-list)
      '()
      (cons (expand (car stx-list) env)
            (expand-program (cdr stx-list) env))))

;; ============ Main Entry Point ============

;; The expander is a function that takes:
;; - tokens: the token list from the lexer
;; - parse: the parse function from the reader
;; Returns: expanded syntax list

;; For now, return a simple expand function
(lambda (stx-list)
  (expand-program stx-list (make-macro-env)))
