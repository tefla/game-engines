;; Slate Lexer - Written in S-expressions
;;
;; Tokenizes Slate source code into a list of tokens.
;; Each token is: (type lexeme line column)
;;
;; Token types:
;;   NUMBER, STRING, IDENTIFIER, TRUE, FALSE, NULL
;;   PLUS, MINUS, STAR, SLASH, PERCENT, CARET
;;   EQUALS, DOUBLE_EQUALS, NOT_EQUALS, BANG
;;   LESS, LESS_EQUALS, GREATER, GREATER_EQUALS
;;   AND, OR, NOT
;;   LPAREN, RPAREN, LBRACKET, RBRACKET, LBRACE, RBRACE
;;   COMMA, DOT, COLON, ARROW, PIPE
;;   LET, VAR, FN, IF, ELSE, WHILE, FOR, IN, RETURN, MATCH
;;   IMPORT, FROM, AS, BREAK, CONTINUE, RANGE, RANGE_INCLUSIVE
;;   INDENT, DEDENT, NEWLINE, EOF, COLOR, LANG_DIRECTIVE

;; Check if character is a hex digit
(define (char-hex-digit? ch)
  (or (char-numeric? ch)
      (string=? ch "a") (string=? ch "b") (string=? ch "c")
      (string=? ch "d") (string=? ch "e") (string=? ch "f")
      (string=? ch "A") (string=? ch "B") (string=? ch "C")
      (string=? ch "D") (string=? ch "E") (string=? ch "F")))

;; Keywords lookup table
(define keywords
  '(("let" LET) ("var" VAR) ("fn" FN) ("if" IF) ("else" ELSE)
    ("while" WHILE) ("for" FOR) ("in" IN) ("return" RETURN)
    ("match" MATCH) ("true" TRUE) ("false" FALSE) ("null" NULL)
    ("and" AND) ("or" OR) ("not" NOT) ("import" IMPORT)
    ("from" FROM) ("as" AS) ("break" BREAK) ("continue" CONTINUE)
    ("on" ON) ("emit" EMIT) ("loop" LOOP) ("spawn" SPAWN)
    ("with" WITH) ("extend" EXTEND) ("type" TYPE) ("yield" YIELD)
    ("wait" WAIT) ("by" BY)))

;; Make a token
(define (make-token type lexeme line col)
  (list type lexeme line col))

;; Get token components
(define (token-type tok) (nth tok 0))
(define (token-lexeme tok) (nth tok 1))
(define (token-line tok) (nth tok 2))
(define (token-col tok) (nth tok 3))

;; Lexer state: (source pos line col indent-stack bracket-depth)
;; bracket-depth tracks (), [], {} depth for implicit line continuation
(define (make-lexer source)
  (list source 0 1 1 (list 0) 0))

(define (lexer-source lex) (nth lex 0))
(define (lexer-pos lex) (nth lex 1))
(define (lexer-line lex) (nth lex 2))
(define (lexer-col lex) (nth lex 3))
(define (lexer-indent-stack lex) (nth lex 4))
(define (lexer-bracket-depth lex) (nth lex 5))

(define (lexer-set-pos lex pos)
  (list (lexer-source lex) pos (lexer-line lex) (lexer-col lex) (lexer-indent-stack lex) (lexer-bracket-depth lex)))

(define (lexer-set-line lex line)
  (list (lexer-source lex) (lexer-pos lex) line (lexer-col lex) (lexer-indent-stack lex) (lexer-bracket-depth lex)))

(define (lexer-set-col lex col)
  (list (lexer-source lex) (lexer-pos lex) (lexer-line lex) col (lexer-indent-stack lex) (lexer-bracket-depth lex)))

(define (lexer-set-indent-stack lex stack)
  (list (lexer-source lex) (lexer-pos lex) (lexer-line lex) (lexer-col lex) stack (lexer-bracket-depth lex)))

(define (lexer-set-bracket-depth lex depth)
  (list (lexer-source lex) (lexer-pos lex) (lexer-line lex) (lexer-col lex) (lexer-indent-stack lex) depth))

(define (lexer-inc-bracket-depth lex)
  (lexer-set-bracket-depth lex (+ (lexer-bracket-depth lex) 1)))

(define (lexer-dec-bracket-depth lex)
  (lexer-set-bracket-depth lex (max 0 (- (lexer-bracket-depth lex) 1))))

;; Is at end of source?
(define (lexer-at-end? lex)
  (>= (lexer-pos lex) (string-length (lexer-source lex))))

;; Peek current character
(define (peek lex)
  (if (lexer-at-end? lex)
      ""
      (string-ref (lexer-source lex) (lexer-pos lex))))

;; Peek next character
(define (peek-next lex)
  (let ((next-pos (+ (lexer-pos lex) 1)))
    (if (>= next-pos (string-length (lexer-source lex)))
        ""
        (string-ref (lexer-source lex) next-pos))))

;; Advance and return (char . new-lexer)
(define (lexer-advance lex)
  (let* ((ch (peek lex))
         (new-pos (+ (lexer-pos lex) 1))
         (new-lex (lexer-set-pos lex new-pos)))
    (if (string=? ch "\n")
        (cons ch (lexer-set-col (lexer-set-line new-lex (+ (lexer-line new-lex) 1)) 1))
        (cons ch (lexer-set-col new-lex (+ (lexer-col new-lex) 1))))))

;; Match a character and advance if it matches
(define (match-char lex expected)
  (if (string=? (peek lex) expected)
      (cons #t (cdr (lexer-advance lex)))
      (cons #f lex)))

;; Skip whitespace (not newlines)
(define (skip-spaces lex)
  (let ((ch (peek lex)))
    (if (or (string=? ch " ") (string=? ch "\t") (string=? ch "\r"))
        (skip-spaces (cdr (lexer-advance lex)))
        lex)))

;; Skip line comment
(define (skip-line-comment lex)
  (let ((ch (peek lex)))
    (cond
      ((lexer-at-end? lex) lex)
      ((string=? ch "\n") lex)
      (else (skip-line-comment (cdr (lexer-advance lex)))))))

;; Read a number
(define (read-number lex start-col)
  (define (read-digits lex acc)
    (let ((ch (peek lex)))
      (if (char-numeric? ch)
          (let ((result (lexer-advance lex)))
            (read-digits (cdr result) (string-append acc (car result))))
          (cons acc lex))))

  (let* ((result1 (read-digits lex ""))
         (int-part (car result1))
         (lex1 (cdr result1)))
    ;; Check for decimal part
    (if (and (string=? (peek lex1) ".")
             (char-numeric? (peek-next lex1)))
        (let* ((lex2 (cdr (lexer-advance lex1)))  ; consume .
               (result2 (read-digits lex2 ""))
               (frac-part (car result2))
               (lex3 (cdr result2)))
          (cons (make-token 'NUMBER
                           (string-append int-part "." frac-part)
                           (lexer-line lex)
                           start-col)
                lex3))
        (cons (make-token 'NUMBER int-part (lexer-line lex) start-col) lex1))))

;; Read an identifier or keyword
(define (read-identifier lex start-col)
  (define (id-char? ch)
    (or (char-alphabetic? ch)
        (char-numeric? ch)
        (string=? ch "_")))

  (define (read-id-chars lex acc)
    (let ((ch (peek lex)))
      (if (id-char? ch)
          (let ((result (lexer-advance lex)))
            (read-id-chars (cdr result) (string-append acc (car result))))
          (cons acc lex))))

  (let* ((result (read-id-chars lex ""))
         (name (car result))
         (lex1 (cdr result))
         (kw (assoc name keywords)))
    (if kw
        (cons (make-token (nth kw 1) name (lexer-line lex) start-col) lex1)
        (cons (make-token 'IDENTIFIER name (lexer-line lex) start-col) lex1))))

;; Read a string
(define (read-string lex start-col)
  (let ((lex1 (cdr (lexer-advance lex))))  ; consume opening "
    (define (read-str-chars lex acc)
      (let ((ch (peek lex)))
        (cond
          ((lexer-at-end? lex) (error "Unterminated string"))
          ((string=? ch "\"")
           (cons acc (cdr (lexer-advance lex))))  ; consume closing "
          ((string=? ch "\\")
           (let* ((lex2 (cdr (lexer-advance lex)))
                  (escaped (peek lex2))
                  (lex3 (cdr (lexer-advance lex2)))
                  (ch-val (cond
                           ((string=? escaped "n") "\n")
                           ((string=? escaped "t") "\t")
                           ((string=? escaped "r") "\r")
                           ((string=? escaped "\\") "\\")
                           ((string=? escaped "\"") "\"")
                           (else escaped))))
             (read-str-chars lex3 (string-append acc ch-val))))
          (else
           (let ((result (lexer-advance lex)))
             (read-str-chars (cdr result) (string-append acc (car result))))))))

    (let ((result (read-str-chars lex1 "")))
      (cons (make-token 'STRING (car result) (lexer-line lex) start-col)
            (cdr result)))))

;; Read #lang directive: #lang name or #lang name/variant
;; Returns (lang-value . new-lexer) or (#f . lexer) if no directive
(define (try-read-lang-directive lex)
  (if (not (string=? (peek lex) "#"))
      (cons #f lex)
      (let ((lex1 (cdr (lexer-advance lex))))  ; skip #
        (if (not (string=? (peek lex1) "l"))
            (cons #f lex)  ; not #lang, return original
            ;; Check for "lang"
            (let* ((lex2 (cdr (lexer-advance lex1)))
                   (lex3 (cdr (lexer-advance lex2)))
                   (lex4 (cdr (lexer-advance lex3)))
                   (lex5 (cdr (lexer-advance lex4))))
              ;; Skip whitespace after #lang
              (let ((lex6 (skip-spaces lex5)))
                ;; Read the language name (alphanum, _, /, -)
                (define (read-lang-chars lex acc)
                  (let ((ch (peek lex)))
                    (if (or (char-alphabetic? ch)
                            (char-numeric? ch)
                            (string=? ch "_")
                            (string=? ch "/")
                            (string=? ch "-"))
                        (let ((result (lexer-advance lex)))
                          (read-lang-chars (cdr result) (string-append acc (car result))))
                        (cons acc lex))))
                (let* ((result (read-lang-chars lex6 ""))
                       (lang-name (car result))
                       (lex7 (cdr result)))
                  ;; Skip to end of line
                  (let ((lex8 (skip-line-comment lex7)))
                    ;; Skip the newline itself if present
                    (if (string=? (peek lex8) "\n")
                        (cons lang-name (cdr (lexer-advance lex8)))
                        (cons lang-name lex8))))))))))

;; Read a color literal: #RGB, #RRGGBB, or #RRGGBBAA
(define (read-color lex start-col)
  (let ((lex1 (cdr (lexer-advance lex))))  ; consume #
    (define (read-hex-chars lex acc count)
      (if (and (< count 8) (char-hex-digit? (peek lex)))
          (let ((result (lexer-advance lex)))
            (read-hex-chars (cdr result) (string-append acc (car result)) (+ count 1)))
          (cons acc lex)))

    (let* ((result (read-hex-chars lex1 "" 0))
           (hex-str (car result))
           (lex2 (cdr result))
           (len (string-length hex-str)))
      ;; Valid lengths: 3 (#RGB), 6 (#RRGGBB), or 8 (#RRGGBBAA)
      (if (or (= len 3) (= len 6) (= len 8))
          (cons (make-token 'COLOR (string-append "#" hex-str) (lexer-line lex) start-col) lex2)
          (error (string-append "Invalid color literal: #" hex-str))))))

;; Tokenize the source
(define (tokenize source)
  ;; Try to read #lang directive at the start
  (define (start-scan lex)
    (let ((result (try-read-lang-directive lex)))
      (if (car result)
          ;; Found #lang directive, emit token and continue from next line
          (let* ((lang-tok (make-token 'LANG_DIRECTIVE (car result) 1 1))
                 (new-lex (cdr result)))
            (scan new-lex (list lang-tok) '() #t))
          ;; No directive, start normal scanning
          (scan lex '() '() #t))))

  (define (scan lex tokens pending-dedents at-line-start)
    (cond
      ;; Emit pending dedents first
      ((pair? pending-dedents)
       (let ((dedent-tok (make-token 'DEDENT "" (lexer-line lex) (lexer-col lex))))
         (scan lex (cons dedent-tok tokens) (cdr pending-dedents) at-line-start)))

      ;; At end - emit final dedents for remaining indentation
      ((lexer-at-end? lex)
       (let ((stack (lexer-indent-stack lex)))
         (if (and (pair? stack) (> (car stack) 0))
             (let ((new-stack (cdr stack))
                   (dedent-tok (make-token 'DEDENT "" (lexer-line lex) (lexer-col lex))))
               (scan (lexer-set-indent-stack lex new-stack)
                     (cons dedent-tok tokens) '() at-line-start))
             (reverse (cons (make-token 'EOF "" (lexer-line lex) (lexer-col lex)) tokens)))))

      ;; Handle indentation at line start
      (at-line-start
       (let* ((line (lexer-line lex))
              (indent-count (count-indent lex))
              (indent (car indent-count))
              (lex1 (cdr indent-count))
              (stack (lexer-indent-stack lex1))
              (current-indent (car stack)))
         (cond
           ;; Blank line or comment - skip
           ((or (string=? (peek lex1) "\n")
                (string=? (peek lex1) "#"))
            (if (string=? (peek lex1) "#")
                (scan (skip-line-comment lex1) tokens '() #t)
                (scan (cdr (lexer-advance lex1)) tokens '() #t)))
           ;; Same indentation - just continue
           ((= indent current-indent)
            (scan lex1 tokens '() #f))
           ;; Increased indentation - emit INDENT
           ((> indent current-indent)
            (let ((new-stack (cons indent stack))
                  (indent-tok (make-token 'INDENT "" line 1)))
              (scan (lexer-set-indent-stack lex1 new-stack)
                    (cons indent-tok tokens) '() #f)))
           ;; Decreased indentation - emit DEDENTs
           (else
            (let ((dedents (calc-dedents indent stack '())))
              (scan (lexer-set-indent-stack lex1 (car dedents))
                    tokens (cdr dedents) #f))))))

      ;; Skip spaces
      ((or (string=? (peek lex) " ") (string=? (peek lex) "\t"))
       (scan (skip-spaces lex) tokens '() #f))

      ;; Skip carriage return
      ((string=? (peek lex) "\r")
       (scan (cdr (lexer-advance lex)) tokens '() #f))

      ;; Handle newline (only emit if not inside brackets)
      ((string=? (peek lex) "\n")
       (if (> (lexer-bracket-depth lex) 0)
           ;; Inside brackets - just advance, don't emit NEWLINE
           (scan (cdr (lexer-advance lex)) tokens '() #f)
           ;; Outside brackets - emit NEWLINE
           (let ((newline-tok (make-token 'NEWLINE "\\n" (lexer-line lex) (lexer-col lex))))
             (scan (cdr (lexer-advance lex)) (cons newline-tok tokens) '() #t))))

      ;; Hash: color literal or comment
      ((string=? (peek lex) "#")
       (if (char-hex-digit? (peek-next lex))
           ;; Color literal
           (let ((result (read-color lex (lexer-col lex))))
             (scan (cdr result) (cons (car result) tokens) '() #f))
           ;; Comment
           (scan (skip-line-comment lex) tokens '() #f)))

      ;; Operators and punctuation
      ((string=? (peek lex) "+")
       (let ((tok (make-token 'PLUS "+" (lexer-line lex) (lexer-col lex))))
         (scan (cdr (lexer-advance lex)) (cons tok tokens) '() #f)))

      ((string=? (peek lex) "-")
       (let ((tok (make-token 'MINUS "-" (lexer-line lex) (lexer-col lex))))
         (scan (cdr (lexer-advance lex)) (cons tok tokens) '() #f)))

      ((string=? (peek lex) "*")
       (let ((tok (make-token 'STAR "*" (lexer-line lex) (lexer-col lex))))
         (scan (cdr (lexer-advance lex)) (cons tok tokens) '() #f)))

      ((string=? (peek lex) "/")
       (let ((tok (make-token 'SLASH "/" (lexer-line lex) (lexer-col lex))))
         (scan (cdr (lexer-advance lex)) (cons tok tokens) '() #f)))

      ((string=? (peek lex) "%")
       (let ((tok (make-token 'PERCENT "%" (lexer-line lex) (lexer-col lex))))
         (scan (cdr (lexer-advance lex)) (cons tok tokens) '() #f)))

      ((string=? (peek lex) "^")
       (let ((tok (make-token 'CARET "^" (lexer-line lex) (lexer-col lex))))
         (scan (cdr (lexer-advance lex)) (cons tok tokens) '() #f)))

      ((string=? (peek lex) "(")
       (let* ((tok (make-token 'LPAREN "(" (lexer-line lex) (lexer-col lex)))
              (lex1 (lexer-inc-bracket-depth (cdr (lexer-advance lex)))))
         (scan lex1 (cons tok tokens) '() #f)))

      ((string=? (peek lex) ")")
       (let* ((tok (make-token 'RPAREN ")" (lexer-line lex) (lexer-col lex)))
              (lex1 (lexer-dec-bracket-depth (cdr (lexer-advance lex)))))
         (scan lex1 (cons tok tokens) '() #f)))

      ((string=? (peek lex) "[")
       (let* ((tok (make-token 'LBRACKET "[" (lexer-line lex) (lexer-col lex)))
              (lex1 (lexer-inc-bracket-depth (cdr (lexer-advance lex)))))
         (scan lex1 (cons tok tokens) '() #f)))

      ((string=? (peek lex) "]")
       (let* ((tok (make-token 'RBRACKET "]" (lexer-line lex) (lexer-col lex)))
              (lex1 (lexer-dec-bracket-depth (cdr (lexer-advance lex)))))
         (scan lex1 (cons tok tokens) '() #f)))

      ((string=? (peek lex) "{")
       (let* ((tok (make-token 'LBRACE "{" (lexer-line lex) (lexer-col lex)))
              (lex1 (lexer-inc-bracket-depth (cdr (lexer-advance lex)))))
         (scan lex1 (cons tok tokens) '() #f)))

      ((string=? (peek lex) "}")
       (let* ((tok (make-token 'RBRACE "}" (lexer-line lex) (lexer-col lex)))
              (lex1 (lexer-dec-bracket-depth (cdr (lexer-advance lex)))))
         (scan lex1 (cons tok tokens) '() #f)))

      ((string=? (peek lex) ",")
       (let ((tok (make-token 'COMMA "," (lexer-line lex) (lexer-col lex))))
         (scan (cdr (lexer-advance lex)) (cons tok tokens) '() #f)))

      ((string=? (peek lex) ":")
       (let ((tok (make-token 'COLON ":" (lexer-line lex) (lexer-col lex))))
         (scan (cdr (lexer-advance lex)) (cons tok tokens) '() #f)))

      ((string=? (peek lex) "|")
       (let ((tok (make-token 'PIPE "|" (lexer-line lex) (lexer-col lex))))
         (scan (cdr (lexer-advance lex)) (cons tok tokens) '() #f)))

      ;; Dot, ranges, and spread
      ((string=? (peek lex) ".")
       (let ((lex1 (cdr (lexer-advance lex))))
         (if (string=? (peek lex1) ".")
             (let ((lex2 (cdr (lexer-advance lex1))))
               (cond
                 ;; ... (spread)
                 ((string=? (peek lex2) ".")
                  (let ((tok (make-token 'SPREAD "..." (lexer-line lex) (lexer-col lex))))
                    (scan (cdr (lexer-advance lex2)) (cons tok tokens) '() #f)))
                 ;; ..= (inclusive range)
                 ((string=? (peek lex2) "=")
                  (let ((tok (make-token 'RANGE_INCLUSIVE "..=" (lexer-line lex) (lexer-col lex))))
                    (scan (cdr (lexer-advance lex2)) (cons tok tokens) '() #f)))
                 ;; .. (exclusive range)
                 (else
                  (let ((tok (make-token 'RANGE ".." (lexer-line lex) (lexer-col lex))))
                    (scan lex2 (cons tok tokens) '() #f)))))
             (let ((tok (make-token 'DOT "." (lexer-line lex) (lexer-col lex))))
               (scan lex1 (cons tok tokens) '() #f)))))

      ;; Equals and arrow
      ((string=? (peek lex) "=")
       (let ((lex1 (cdr (lexer-advance lex))))
         (cond
           ((string=? (peek lex1) "=")
            (let ((tok (make-token 'DOUBLE_EQUALS "==" (lexer-line lex) (lexer-col lex))))
              (scan (cdr (lexer-advance lex1)) (cons tok tokens) '() #f)))
           ((string=? (peek lex1) ">")
            (let ((tok (make-token 'ARROW "=>" (lexer-line lex) (lexer-col lex))))
              (scan (cdr (lexer-advance lex1)) (cons tok tokens) '() #f)))
           (else
            (let ((tok (make-token 'EQUALS "=" (lexer-line lex) (lexer-col lex))))
              (scan lex1 (cons tok tokens) '() #f))))))

      ;; Not equals and bang
      ((string=? (peek lex) "!")
       (let ((lex1 (cdr (lexer-advance lex))))
         (if (string=? (peek lex1) "=")
             (let ((tok (make-token 'NOT_EQUALS "!=" (lexer-line lex) (lexer-col lex))))
               (scan (cdr (lexer-advance lex1)) (cons tok tokens) '() #f))
             (let ((tok (make-token 'BANG "!" (lexer-line lex) (lexer-col lex))))
               (scan lex1 (cons tok tokens) '() #f)))))

      ;; Less than
      ((string=? (peek lex) "<")
       (let ((lex1 (cdr (lexer-advance lex))))
         (if (string=? (peek lex1) "=")
             (let ((tok (make-token 'LESS_EQUALS "<=" (lexer-line lex) (lexer-col lex))))
               (scan (cdr (lexer-advance lex1)) (cons tok tokens) '() #f))
             (let ((tok (make-token 'LESS "<" (lexer-line lex) (lexer-col lex))))
               (scan lex1 (cons tok tokens) '() #f)))))

      ;; Greater than
      ((string=? (peek lex) ">")
       (let ((lex1 (cdr (lexer-advance lex))))
         (if (string=? (peek lex1) "=")
             (let ((tok (make-token 'GREATER_EQUALS ">=" (lexer-line lex) (lexer-col lex))))
               (scan (cdr (lexer-advance lex1)) (cons tok tokens) '() #f))
             (let ((tok (make-token 'GREATER ">" (lexer-line lex) (lexer-col lex))))
               (scan lex1 (cons tok tokens) '() #f)))))

      ;; Question mark (for null coalescing and optional chaining)
      ((string=? (peek lex) "?")
       (let ((lex1 (cdr (lexer-advance lex))))
         (cond
           ;; ?? (null coalescing)
           ((string=? (peek lex1) "?")
            (let ((tok (make-token 'DOUBLE_QUESTION "??" (lexer-line lex) (lexer-col lex))))
              (scan (cdr (lexer-advance lex1)) (cons tok tokens) '() #f)))
           ;; ?. (optional member access)
           ((string=? (peek lex1) ".")
            (let ((tok (make-token 'QUESTION_DOT "?." (lexer-line lex) (lexer-col lex))))
              (scan (cdr (lexer-advance lex1)) (cons tok tokens) '() #f)))
           ;; ?[ (optional index access)
           ((string=? (peek lex1) "[")
            (let ((tok (make-token 'QUESTION_BRACKET "?[" (lexer-line lex) (lexer-col lex)))
                  (lex2 (lexer-inc-bracket-depth (cdr (lexer-advance lex1)))))
              (scan lex2 (cons tok tokens) '() #f)))
           ;; Just ? (for future use)
           (else
            (let ((tok (make-token 'QUESTION "?" (lexer-line lex) (lexer-col lex))))
              (scan lex1 (cons tok tokens) '() #f))))))

      ;; At sign (for signals)
      ((string=? (peek lex) "@")
       (let ((tok (make-token 'AT "@" (lexer-line lex) (lexer-col lex))))
         (scan (cdr (lexer-advance lex)) (cons tok tokens) '() #f)))

      ;; String
      ((string=? (peek lex) "\"")
       (let ((result (read-string lex (lexer-col lex))))
         (scan (cdr result) (cons (car result) tokens) '() #f)))

      ;; Number
      ((char-numeric? (peek lex))
       (let ((result (read-number lex (lexer-col lex))))
         (scan (cdr result) (cons (car result) tokens) '() #f)))

      ;; Identifier or keyword
      ((or (char-alphabetic? (peek lex)) (string=? (peek lex) "_"))
       (let ((result (read-identifier lex (lexer-col lex))))
         (scan (cdr result) (cons (car result) tokens) '() #f)))

      ;; Unknown character
      (else
       (error (string-append "Unexpected character: " (peek lex))))))

  ;; Count leading spaces/tabs for indentation
  (define (count-indent lex)
    (define (count lex n)
      (let ((ch (peek lex)))
        (cond
          ((string=? ch " ") (count (cdr (lexer-advance lex)) (+ n 1)))
          ((string=? ch "\t") (count (cdr (lexer-advance lex)) (+ n 4)))  ; Tab = 4 spaces
          (else (cons n lex)))))
    (count lex 0))

  ;; Calculate dedents needed
  (define (calc-dedents target-indent stack dedents)
    (if (or (null? stack) (<= (car stack) target-indent))
        (cons stack dedents)
        (calc-dedents target-indent (cdr stack) (cons 'dedent dedents))))

  ;; Utility for assoc that returns just value
  (define (assoc key alist)
    (cond
      ((null? alist) #f)
      ((string=? (car (car alist)) key) (car alist))
      (else (assoc key (cdr alist)))))

  ;; Start scanning (with potential #lang directive check)
  (start-scan (make-lexer source)))

;; Export main function
tokenize
