;; Slate Evaluator - Written in S-expressions
;;
;; Interprets the expanded Slate AST (S-expressions).
;; The AST is produced by: source -> lexer.sl -> reader.sl -> expander.sl
;;
;; This evaluator is self-hosted: it runs on the S-expr TypeScript evaluator.

;; ============ Value Types ============
;; Values are tagged lists:
;;   Numbers: bare numbers
;;   Strings: bare strings
;;   Booleans: #t, #f
;;   Null: nil
;;   Lists: (list-val elem1 elem2 ...)
;;   Records: (record-val (key1 val1) (key2 val2) ...)
;;   Closures: (closure params body env)
;;   Signals: (signal-path "part1" "part2" ...)

;; ============ Environment ============
;; Environment is: (env parent bindings)
;; where bindings is: ((name . value) (name . value) ...)

(define (make-env parent)
  (list 'env parent '()))

(define (env-parent env)
  (nth env 1))

(define (env-bindings env)
  (nth env 2))

(define (env-set-bindings env bindings)
  (list 'env (env-parent env) bindings))

;; Look up a variable in the environment chain
(define (env-lookup env name)
  (if (null? env)
      (error (string-append "Undefined variable: " name))
      (let ((binding (assoc name (env-bindings env))))
        (if binding
            (nth binding 1)  ; Get value from (key value) pair
            (env-lookup (env-parent env) name)))))

;; Check if a variable exists
(define (env-has? env name)
  (if (null? env)
      #f
      (let ((binding (assoc name (env-bindings env))))
        (if binding
            #t
            (env-has? (env-parent env) name)))))

;; Define a new variable (in current scope only)
(define (env-define env name value)
  (let ((bindings (env-bindings env)))
    (env-set-bindings env (cons (list name value) bindings))))

;; Assign to an existing variable (search up the chain)
(define (env-assign env name value)
  (if (null? env)
      (error (string-append "Cannot assign to undefined variable: " name))
      (let ((bindings (env-bindings env)))
        (if (assoc name bindings)
            ;; Found in current scope - update it
            (env-set-bindings env (update-binding bindings name value))
            ;; Not in current scope - search parent
            (let ((new-parent (env-assign (env-parent env) name value)))
              (list 'env new-parent bindings))))))

;; Update a binding in a list of bindings
(define (update-binding bindings name value)
  (if (null? bindings)
      '()
      (let ((binding (car bindings)))
        (if (string=? (car binding) name)
            (cons (list name value) (cdr bindings))
            (cons binding (update-binding (cdr bindings) name value))))))

;; ============ Utility Functions ============

;; Local map function (to avoid dependency on primitives at runtime)
(define (local-map f lst)
  (if (null? lst)
      '()
      (cons (f (car lst)) (local-map f (cdr lst)))))

;; Check if a value is the tagged symbol ("symbol" "name")
(define (is-tagged-symbol? expr name)
  (and (pair? expr)
       (pair? (car expr))
       (string? (car (car expr)))
       (string=? (car (car expr)) "symbol")
       (string=? (nth (car expr) 1) name)))

;; Check if expression is a symbol (either format)
(define (is-symbol-expr? u)
  (or (and (pair? u) (equal? (car u) 'symbol))
      (is-tagged-symbol? u "symbol")))

;; Get the tag (first element) of a syntax object or symbol
;; Handles both ('syntax ...) and (("symbol" "syntax") ...) formats
(define (get-tag expr)
  (cond
    ;; Tagged syntax object: (("symbol" "syntax") datum line col)
    ((is-tagged-symbol? expr "syntax")
     (get-tag (nth expr 1)))
    ;; Raw syntax object: (syntax datum line col)
    ((and (pair? expr) (equal? (car expr) 'syntax))
     (get-tag (nth expr 1)))
    ;; Tagged symbol: (("symbol" "symbol") name)
    ((is-tagged-symbol? expr "symbol")
     (nth expr 1))
    ;; Raw symbol: (symbol name)
    ((and (pair? expr) (equal? (car expr) 'symbol))
     (nth expr 1))
    ;; List: look at first element
    ((pair? expr)
     (get-tag (car expr)))
    ;; String (direct symbol name)
    ((string? expr) expr)
    (else #f)))

;; Unwrap syntax object - handles both formats
(define (unwrap expr)
  (cond
    ((is-tagged-symbol? expr "syntax")
     (nth expr 1))
    ((and (pair? expr) (equal? (car expr) 'syntax))
     (nth expr 1))
    (else expr)))

;; Extract symbol name - handles both formats
(define (symbol-name expr)
  (let ((u (unwrap expr)))
    (cond
      ;; Tagged symbol: (("symbol" "symbol") name)
      ((is-tagged-symbol? u "symbol")
       (nth u 1))
      ;; Raw symbol: (symbol name)
      ((and (pair? u) (equal? (car u) 'symbol))
       (nth u 1))
      ;; Direct string
      ((string? u) u)
      (else (error "Expected symbol")))))

;; Check if expression is a specific symbol
(define (is-symbol? expr name)
  (let ((tag (get-tag expr)))
    (and tag (string=? tag name))))

;; Is value truthy?
(define (truthy? val)
  (cond
    ((null? val) #f)
    ((boolean? val) val)
    ((and (pair? val) (equal? (car val) 'list-val))
     (> (length val) 1))  ; non-empty list
    (else #t)))

;; ============ Control Flow Exceptions ============
;; We use special tagged values for break/continue/return
;; Since we can't throw exceptions, we return tagged values and check for them

(define (make-break)
  (list 'control-break))

(define (make-continue)
  (list 'control-continue))

(define (make-return value)
  (list 'control-return value))

(define (is-break? val)
  (and (pair? val) (equal? (car val) 'control-break)))

(define (is-continue? val)
  (and (pair? val) (equal? (car val) 'control-continue)))

(define (is-return? val)
  (and (pair? val) (equal? (car val) 'control-return)))

(define (return-value val)
  (nth val 1))

;; ============ Core Evaluator ============

;; Evaluate a program (list of statements)
(define (eval-program stmts env)
  (eval-stmts stmts env '()))

;; Evaluate a list of statements, return last value
(define (eval-stmts stmts env last-val)
  (if (null? stmts)
      (cons last-val env)
      (let* ((result (eval-stmt (car stmts) env))
             (val (car result))
             (new-env (cdr result)))
        ;; Check for control flow
        (if (or (is-break? val) (is-continue? val) (is-return? val))
            result  ; propagate control flow
            (eval-stmts (cdr stmts) new-env val)))))

;; Safe string comparison that handles nil
(define (tag=? tag name)
  (and (string? tag) (string=? tag name)))

;; Evaluate a single statement
;; Returns (value . new-env)
(define (eval-stmt stmt env)
  (let ((tag (get-tag stmt)))
    (cond
      ((tag=? tag "let") (eval-let stmt env))
      ((tag=? tag "var") (eval-var stmt env))
      ((tag=? tag "fn") (eval-fn stmt env))
      ((tag=? tag "if") (eval-if stmt env))
      ((tag=? tag "while") (eval-while stmt env))
      ((tag=? tag "for") (eval-for stmt env))
      ((tag=? tag "match") (eval-match stmt env))
      ((tag=? tag "return") (eval-return stmt env))
      ((tag=? tag "break") (cons (make-break) env))
      ((tag=? tag "continue") (cons (make-continue) env))
      ((tag=? tag "on") (eval-on stmt env))
      ((tag=? tag "emit") (eval-emit stmt env))
      ((tag=? tag "set!") (eval-set! stmt env))
      ((tag=? tag "begin") (eval-begin stmt env))
      (else (eval-expr stmt env)))))

;; Evaluate let binding: (let name [type] value)
(define (eval-let stmt env)
  (let* ((u (unwrap stmt))
         (elems (cdr u))  ; skip 'let
         (name (symbol-name (car elems)))
         (value-expr (if (= (length elems) 3)
                         (nth elems 2)  ; with type
                         (nth elems 1)))  ; without type
         (result (eval-expr value-expr env))
         (val (car result))
         (env1 (cdr result)))
    (cons val (env-define env1 name val))))

;; Evaluate var binding (same as let for now)
(define (eval-var stmt env)
  (eval-let stmt env))

;; Evaluate function definition: (fn name (params...) body)
;; Closures store their own name for recursive calls
(define (eval-fn stmt env)
  (let* ((u (unwrap stmt))
         (elems (cdr u))  ; skip 'fn
         (name (symbol-name (car elems)))
         (params-stx (nth elems 1))
         (params (unwrap params-stx))
         (body (nth elems 2))
         (param-names (local-map symbol-name (if (list? params) params '())))
         ;; Create closure with self-reference info: (closure name params body env)
         (closure (list 'closure name param-names body env)))
    (cons closure (env-define env name closure))))

;; Evaluate if expression: (if cond then [else])
(define (eval-if stmt env)
  (let* ((u (unwrap stmt))
         (elems (cdr u))
         (cond-expr (car elems))
         (then-expr (nth elems 1))
         (cond-result (eval-expr cond-expr env))
         (cond-val (car cond-result))
         (env1 (cdr cond-result)))
    (if (truthy? cond-val)
        (eval-stmt then-expr env1)
        (if (>= (length elems) 3)
            (eval-stmt (nth elems 2) env1)
            (cons '() env1)))))

;; Evaluate while loop: (while cond body)
(define (eval-while stmt env)
  (let* ((u (unwrap stmt))
         (elems (cdr u))
         (cond-expr (car elems))
         (body-expr (nth elems 1)))
    (define (loop env last-val)
      (let* ((cond-result (eval-expr cond-expr env))
             (cond-val (car cond-result))
             (env1 (cdr cond-result)))
        (if (truthy? cond-val)
            (let* ((body-result (eval-stmt body-expr env1))
                   (body-val (car body-result))
                   (env2 (cdr body-result)))
              (cond
                ((is-break? body-val) (cons '() env2))
                ((is-continue? body-val) (loop env2 last-val))
                ((is-return? body-val) body-result)
                (else (loop env2 body-val))))
            (cons last-val env1))))
    (loop env '())))

;; Evaluate for loop: (for var iterable body)
;; Must thread the modified parent environment through iterations
(define (eval-for stmt env)
  (let* ((u (unwrap stmt))
         (elems (cdr u))
         (var-name (symbol-name (car elems)))
         (iter-expr (nth elems 1))
         (body-expr (nth elems 2))
         (iter-result (eval-expr iter-expr env))
         (iter-val (car iter-result))
         (env1 (cdr iter-result)))
    ;; Get items to iterate over
    (let ((items (cond
                   ((and (pair? iter-val) (equal? (car iter-val) 'list-val))
                    (cdr iter-val))
                   ((list? iter-val) iter-val)
                   (else (error "for loop requires iterable")))))
      (define (iterate items env last-val)
        (if (null? items)
            (cons last-val env)
            (let* ((item (car items))
                   (body-env (make-env env))
                   (body-env2 (env-define body-env var-name item))
                   (body-result (eval-stmt body-expr body-env2))
                   (body-val (car body-result))
                   ;; Extract potentially modified parent environment
                   (new-parent-env (env-parent (cdr body-result))))
              (cond
                ((is-break? body-val) (cons last-val new-parent-env))
                ((is-continue? body-val) (iterate (cdr items) new-parent-env last-val))
                ((is-return? body-val) body-result)
                (else (iterate (cdr items) new-parent-env body-val))))))
      (iterate items env1 '()))))

;; Evaluate match expression: (match subject arm1 arm2 ...)
(define (eval-match stmt env)
  (let* ((u (unwrap stmt))
         (elems (cdr u))
         (subject-expr (car elems))
         (arms (cdr elems))
         (subject-result (eval-expr subject-expr env))
         (subject-val (car subject-result))
         (env1 (cdr subject-result)))
    (define (try-arms arms)
      (if (null? arms)
          (error "No matching pattern")
          (let* ((arm (car arms))
                 (arm-u (unwrap arm))
                 (pattern (car arm-u))
                 (body (if (= (length arm-u) 2)
                           (nth arm-u 1)
                           (nth arm-u 2)))  ; with guard
                 (bindings (pattern-match pattern subject-val)))
            (if bindings
                (let ((arm-env (apply-bindings (make-env env1) bindings)))
                  (eval-expr body arm-env))
                (try-arms (cdr arms))))))
    (try-arms arms)))

;; Pattern matching - returns bindings or #f
(define (pattern-match pattern value)
  (let* ((p (unwrap pattern))
         (tag (get-tag p)))
    (cond
      ;; Wildcard
      ((and tag (string=? tag "_")) '())
      ;; Identifier - binds the value
      ((and (pair? p) (equal? (car p) 'symbol))
       (list (cons (nth p 1) value)))
      ;; Number literal
      ((number? p)
       (if (and (number? value) (= p value)) '() #f))
      ;; String literal
      ((string? p)
       (if (and (string? value) (string=? p value)) '() #f))
      ;; Boolean
      ((boolean? p)
       (if (equal? p value) '() #f))
      ;; Other
      (else '()))))

;; Apply bindings to environment
(define (apply-bindings env bindings)
  (if (null? bindings)
      env
      (let ((binding (car bindings)))
        (apply-bindings (env-define env (car binding) (nth binding 1))
                        (cdr bindings)))))

;; Evaluate return: (return [value])
(define (eval-return stmt env)
  (let* ((u (unwrap stmt))
         (elems (cdr u)))
    (if (null? elems)
        (cons (make-return '()) env)
        (let ((result (eval-expr (car elems) env)))
          (cons (make-return (car result)) (cdr result))))))

;; Evaluate begin block: (begin stmt1 stmt2 ...)
;; Block creates new scope, but modifications to parent variables must persist
(define (eval-begin stmt env)
  (let* ((u (unwrap stmt))
         (stmts (cdr u))  ; skip 'begin
         (block-env (make-env env)))
    (let ((result (eval-stmts stmts block-env '())))
      ;; Return to modified parent env (preserving outer-scope changes)
      (cons (car result) (env-parent (cdr result))))))

;; Evaluate assignment: (set! target value)
(define (eval-set! stmt env)
  (let* ((u (unwrap stmt))
         (elems (cdr u))
         (target (car elems))
         (value-expr (nth elems 1))
         (value-result (eval-expr value-expr env))
         (val (car value-result))
         (env1 (cdr value-result))
         (target-u (unwrap target))
         (target-tag (get-tag target)))
    (cond
      ;; Variable assignment - handle both formats
      ((is-symbol-expr? target-u)
       (let ((name (symbol-name target)))
         (cons val (env-assign env1 name val))))
      ;; Member assignment: (. obj field)
      ((tag=? target-tag ".")
       (let* ((t (unwrap target))
              (obj-result (eval-expr (nth t 1) env1))
              (obj (car obj-result))
              (field (symbol-name (nth t 2))))
         ;; Update record field
         (cons val env1)))  ; TODO: proper record update
      ;; Index assignment: (index obj idx)
      ((tag=? target-tag "index")
       (cons val env1))  ; TODO: proper index update
      (else (error "Invalid assignment target")))))

;; Evaluate on statement: (on signal [filter] body)
(define (eval-on stmt env)
  ;; For now, just return a handler record
  (let* ((u (unwrap stmt))
         (elems (cdr u))
         (signal (car elems))
         (body (if (= (length elems) 2)
                   (nth elems 1)
                   (nth elems 2))))
    (cons (list 'handler signal body env) env)))

;; Evaluate emit statement: (emit signal [data])
(define (eval-emit stmt env)
  (let* ((u (unwrap stmt))
         (elems (cdr u))
         (signal (car elems)))
    (if (>= (length elems) 2)
        (let ((data-result (eval-expr (nth elems 1) env)))
          (cons (list 'emitted signal (car data-result)) (cdr data-result)))
        (cons (list 'emitted signal '()) env))))

;; ============ Expression Evaluator ============

;; Evaluate an expression
;; Returns (value . env)
(define (eval-expr expr env)
  (let* ((u (unwrap expr))
         (tag (get-tag expr)))
    (cond
      ;; Literals
      ((number? u) (cons u env))
      ((string? u) (cons u env))
      ((boolean? u) (cons u env))
      ((null? u) (cons '() env))

      ;; Symbol (variable reference) - handle both formats
      ((is-symbol-expr? u)
       (let ((name (symbol-name u)))
         (cond
           ((string=? name "true") (cons #t env))
           ((string=? name "false") (cons #f env))
           ((string=? name "null") (cons '() env))
           (else (cons (env-lookup env name) env)))))

      ;; Special forms
      ((tag=? tag "begin") (eval-begin expr env))
      ((tag=? tag "if") (eval-if expr env))
      ((tag=? tag "let") (eval-let expr env))
      ((tag=? tag "fn") (eval-fn expr env))
      ((tag=? tag "lambda") (eval-lambda expr env))

      ;; Binary operations
      ((tag=? tag "+") (eval-binary-arith expr env + 0))
      ((tag=? tag "-") (eval-binary-arith expr env - #f))
      ((tag=? tag "*") (eval-binary-arith expr env * 1))
      ((tag=? tag "/") (eval-binary-arith expr env / #f))
      ((tag=? tag "%") (eval-binary-mod expr env))
      ((tag=? tag "^") (eval-binary-pow expr env))

      ;; Comparison
      ((tag=? tag "<") (eval-comparison expr env <))
      ((tag=? tag "<=") (eval-comparison expr env <=))
      ((tag=? tag ">") (eval-comparison expr env >))
      ((tag=? tag ">=") (eval-comparison expr env >=))
      ((tag=? tag "==") (eval-equality expr env))
      ((tag=? tag "!=") (eval-inequality expr env))

      ;; Logical
      ((tag=? tag "and") (eval-and expr env))
      ((tag=? tag "or") (eval-or expr env))
      ((tag=? tag "not") (eval-not expr env))
      ((tag=? tag "!") (eval-not expr env))

      ;; Null coalescing
      ((tag=? tag "??") (eval-null-coalesce expr env))

      ;; Range
      ((tag=? tag "range") (eval-range expr env #f))
      ((tag=? tag "range-inclusive") (eval-range expr env #t))

      ;; Member access
      ((tag=? tag ".") (eval-member expr env))
      ((tag=? tag "?.") (eval-optional-member expr env))

      ;; Index access
      ((tag=? tag "index") (eval-index expr env))
      ((tag=? tag "?index") (eval-optional-index expr env))

      ;; Function call
      ((tag=? tag "call") (eval-call expr env))

      ;; List literal
      ((tag=? tag "list") (eval-list-literal expr env))

      ;; Record literal
      ((tag=? tag "record") (eval-record-literal expr env))

      ;; Assignment
      ((tag=? tag "assign") (eval-assign expr env))

      ;; Unknown
      (else (error (string-append "Unknown expression type: " (if tag tag "?")))))))

;; Evaluate lambda: (lambda (params) body)
;; Creates closure with empty name (anonymous function)
(define (eval-lambda expr env)
  (let* ((u (unwrap expr))
         (elems (cdr u))
         (params-stx (car elems))
         (params (unwrap params-stx))
         (body (nth elems 1))
         (param-names (local-map symbol-name (if (list? params) params '()))))
    ;; Closure format: (closure name params body env) - name is "" for lambdas
    (cons (list 'closure "" param-names body env) env)))

;; Evaluate binary arithmetic
(define (eval-binary-arith expr env op identity)
  (let* ((u (unwrap expr))
         (elems (cdr u)))
    (if (= (length elems) 1)
        ;; Unary minus
        (let ((result (eval-expr (car elems) env)))
          (cons (op 0 (car result)) (cdr result)))
        ;; Binary operation
        (let* ((left-result (eval-expr (car elems) env))
               (left-val (car left-result))
               (env1 (cdr left-result))
               (right-result (eval-expr (nth elems 1) env1))
               (right-val (car right-result))
               (env2 (cdr right-result)))
          (cons (op left-val right-val) env2)))))

;; Evaluate modulo
(define (eval-binary-mod expr env)
  (let* ((u (unwrap expr))
         (elems (cdr u))
         (left-result (eval-expr (car elems) env))
         (right-result (eval-expr (nth elems 1) (cdr left-result))))
    (cons (% (car left-result) (car right-result)) (cdr right-result))))

;; Evaluate power
(define (eval-binary-pow expr env)
  (let* ((u (unwrap expr))
         (elems (cdr u))
         (left-result (eval-expr (car elems) env))
         (right-result (eval-expr (nth elems 1) (cdr left-result))))
    ;; Use repeated multiplication for integer powers
    (define (pow-int base exp acc)
      (if (<= exp 0) acc (pow-int base (- exp 1) (* acc base))))
    (cons (pow-int (car left-result) (car right-result) 1) (cdr right-result))))

;; Evaluate comparison
(define (eval-comparison expr env op)
  (let* ((u (unwrap expr))
         (elems (cdr u))
         (left-result (eval-expr (car elems) env))
         (right-result (eval-expr (nth elems 1) (cdr left-result))))
    (cons (op (car left-result) (car right-result)) (cdr right-result))))

;; Evaluate equality
(define (eval-equality expr env)
  (let* ((u (unwrap expr))
         (elems (cdr u))
         (left-result (eval-expr (car elems) env))
         (right-result (eval-expr (nth elems 1) (cdr left-result))))
    (cons (equal? (car left-result) (car right-result)) (cdr right-result))))

;; Evaluate inequality
(define (eval-inequality expr env)
  (let* ((u (unwrap expr))
         (elems (cdr u))
         (left-result (eval-expr (car elems) env))
         (right-result (eval-expr (nth elems 1) (cdr left-result))))
    (cons (not (equal? (car left-result) (car right-result))) (cdr right-result))))

;; Evaluate short-circuit and
(define (eval-and expr env)
  (let* ((u (unwrap expr))
         (elems (cdr u))
         (left-result (eval-expr (car elems) env)))
    (if (truthy? (car left-result))
        (eval-expr (nth elems 1) (cdr left-result))
        (cons (car left-result) (cdr left-result)))))

;; Evaluate short-circuit or
(define (eval-or expr env)
  (let* ((u (unwrap expr))
         (elems (cdr u))
         (left-result (eval-expr (car elems) env)))
    (if (truthy? (car left-result))
        (cons (car left-result) (cdr left-result))
        (eval-expr (nth elems 1) (cdr left-result)))))

;; Evaluate not
(define (eval-not expr env)
  (let* ((u (unwrap expr))
         (elems (cdr u))
         (result (eval-expr (car elems) env)))
    (cons (not (truthy? (car result))) (cdr result))))

;; Evaluate null coalescing
(define (eval-null-coalesce expr env)
  (let* ((u (unwrap expr))
         (elems (cdr u))
         (left-result (eval-expr (car elems) env)))
    (if (null? (car left-result))
        (eval-expr (nth elems 1) (cdr left-result))
        (cons (car left-result) (cdr left-result)))))

;; Evaluate range
(define (eval-range expr env inclusive)
  (let* ((u (unwrap expr))
         (elems (cdr u))
         (start-result (eval-expr (car elems) env))
         (end-result (eval-expr (nth elems 1) (cdr start-result)))
         (start (car start-result))
         (end (car end-result))
         (final-end (if inclusive (+ end 1) end)))
    (define (make-range n acc)
      (if (>= n final-end)
          (reverse acc)
          (make-range (+ n 1) (cons n acc))))
    (cons (cons 'list-val (make-range start '())) (cdr end-result))))

;; Evaluate member access: (. obj field)
(define (eval-member expr env)
  (let* ((u (unwrap expr))
         (elems (cdr u))
         (obj-result (eval-expr (car elems) env))
         (obj (car obj-result))
         (field (symbol-name (nth elems 1))))
    (cond
      ;; Record
      ((and (pair? obj) (equal? (car obj) 'record-val))
       (let ((val (record-get obj field)))
         (cons val (cdr obj-result))))
      ;; List length pseudo-property
      ((and (pair? obj) (equal? (car obj) 'list-val))
       (if (string=? field "length")
           (cons (- (length obj) 1) (cdr obj-result))
           (error "Lists only have 'length' property")))
      (else (error "Cannot access property of non-record")))))

;; Get value from record
;; Record fields are stored as (key val) pairs (2-element lists)
(define (record-get record field)
  (define (find-field fields)
    (if (null? fields)
        '()
        (let ((f (car fields)))
          (if (string=? (car f) field)
              (nth f 1)  ; Get value from (key val) pair
              (find-field (cdr fields))))))
  (find-field (cdr record)))

;; Evaluate optional member: (?. obj field)
(define (eval-optional-member expr env)
  (let* ((u (unwrap expr))
         (elems (cdr u))
         (obj-result (eval-expr (car elems) env))
         (obj (car obj-result)))
    (if (null? obj)
        (cons '() (cdr obj-result))
        (eval-member expr env))))

;; Evaluate index access: (index obj idx)
(define (eval-index expr env)
  (let* ((u (unwrap expr))
         (elems (cdr u))
         (obj-result (eval-expr (car elems) env))
         (obj (car obj-result))
         (idx-result (eval-expr (nth elems 1) (cdr obj-result)))
         (idx (car idx-result)))
    (cond
      ;; List
      ((and (pair? obj) (equal? (car obj) 'list-val))
       (let ((items (cdr obj)))
         (if (and (number? idx) (>= idx 0) (< idx (length items)))
             (cons (nth items idx) (cdr idx-result))
             (error "Index out of bounds"))))
      ;; Record (string key)
      ((and (pair? obj) (equal? (car obj) 'record-val))
       (cons (record-get obj idx) (cdr idx-result)))
      (else (error "Cannot index non-list/record")))))

;; Evaluate optional index: (?index obj idx)
(define (eval-optional-index expr env)
  (let* ((u (unwrap expr))
         (elems (cdr u))
         (obj-result (eval-expr (car elems) env))
         (obj (car obj-result)))
    (if (null? obj)
        (cons '() (cdr obj-result))
        (eval-index expr env))))

;; Evaluate function call: (call fn arg1 arg2 ...)
;; Closure format: (closure name params body env)
(define (eval-call expr env)
  (let* ((u (unwrap expr))
         (elems (cdr u))
         (fn-result (eval-expr (car elems) env))
         (fn (car fn-result))
         (env1 (cdr fn-result))
         (args-result (eval-args (cdr elems) env1 '()))
         (args (car args-result))
         (env2 (cdr args-result)))
    (cond
      ;; Closure: (closure name params body env)
      ((and (pair? fn) (equal? (car fn) 'closure))
       (let* ((fn-name (nth fn 1))      ; Function name (empty for lambdas)
              (params (nth fn 2))        ; Parameter names
              (body (nth fn 3))          ; Function body
              (fn-env (nth fn 4))        ; Captured environment
              (call-env (make-env fn-env))
              ;; Add self-binding for recursive calls (if function has a name)
              (call-env1 (if (and (string? fn-name) (> (string-length fn-name) 0))
                             (env-define call-env fn-name fn)
                             call-env))
              (call-env2 (bind-params call-env1 params args))
              (result (eval-stmt body call-env2))
              (val (car result)))
         (if (is-return? val)
             (cons (return-value val) env2)
             (cons val env2))))
      ;; Native function would need special handling
      (else (error "Cannot call non-function")))))

;; Evaluate arguments
(define (eval-args exprs env acc)
  (if (null? exprs)
      (cons (reverse acc) env)
      (let* ((result (eval-expr (car exprs) env))
             (val (car result))
             (env1 (cdr result)))
        (eval-args (cdr exprs) env1 (cons val acc)))))

;; Bind parameters to arguments
(define (bind-params env params args)
  (if (null? params)
      env
      (bind-params (env-define env (car params) (car args))
                   (cdr params)
                   (cdr args))))

;; Evaluate list literal: (list elem1 elem2 ...)
(define (eval-list-literal expr env)
  (let* ((u (unwrap expr))
         (elems (cdr u))  ; skip 'list
         (result (eval-args elems env '())))
    (cons (cons 'list-val (car result)) (cdr result))))

;; Evaluate record literal: (record (key1 val1) (key2 val2) ...)
(define (eval-record-literal expr env)
  (let* ((u (unwrap expr))
         (fields (cdr u)))  ; skip 'record
    (define (eval-fields fields env acc)
      (if (null? fields)
          (cons (cons 'record-val (reverse acc)) env)
          (let* ((field (unwrap (car fields)))
                 (key (symbol-name (car field)))
                 (val-result (eval-expr (nth field 1) env))
                 (val (car val-result))
                 (env1 (cdr val-result)))
            (eval-fields (cdr fields) env1 (cons (cons key val) acc)))))
    (eval-fields fields env '())))

;; Evaluate assignment: (assign target value)
(define (eval-assign expr env)
  (eval-set! expr env))

;; ============ Main Entry Point ============

;; Create an evaluator that takes a Slate AST and runs it
(define (make-evaluator)
  (let ((global-env (make-env '())))
    ;; Return a function that evaluates AST
    (lambda (ast)
      (let ((result (eval-program (if (list? ast) ast (list ast)) global-env)))
        (car result)))))

make-evaluator
