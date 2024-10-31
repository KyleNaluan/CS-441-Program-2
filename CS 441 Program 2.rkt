#lang racket

;; SCANNER
(define (scan-file filename)
  (with-input-from-file filename
    (lambda ()
      (let loop ((char (read-char)) 
                 (tokens '()) 
                 (current-token "") 
                 (line 1) 
                 (column 1)
                 (line-start #t))
        (cond
          [(eof-object? char) 
           (reverse (if (string=? current-token "") 
                        tokens 
                        (cons (categorize-token current-token) tokens)))]

          [(char=? char #\newline)
           (loop (read-char) 
                 (cons `(newline) 
                       (if (string=? current-token "")
                           tokens
                           (cons (categorize-token current-token) tokens)))
                 ""
                 (add1 line)
                 1
                 #t)]

          ;; Detect "REM" as a statement at line start or after whitespace
          [(and (or line-start (char-whitespace? char))
                (string-ci=? (string-trim current-token) "rem"))
           ;; Add "REM" token and skip to end of the line, with newline after
           (skip-to-end-of-line (read-char) line column (cons `(comment "REM") tokens) loop #t)]

          [(char-whitespace? char)
           (loop (read-char) 
                 (if (string=? current-token "")
                     tokens
                     (cons (categorize-token current-token) tokens))
                 ""
                 line
                 (add1 column)
                 line-start)]

          ;; Handle two-character operators like ":=", "<>", etc.
          [(and (member char '(#\: #\< #\>))
                (let ((next-char (peek-char)))
                  (and next-char
                       (member (string char next-char) '(":=" "<>" "><" ">=" "<=")))))
           (let* ((two-char-op (string char (read-char))))  ; consume the second character
             (loop (read-char) 
                   (cons (categorize-token two-char-op) 
                         (if (string=? current-token "")
                             tokens
                             (cons (categorize-token current-token) tokens)))
                   ""
                   line
                   (+ column 2)
                   #f))]

          ;; Finalize current token if punctuation (like parentheses) follows directly after a token
          [(or (char-operator? char) (char-punctuation? char))
           (loop (read-char)
                 (if (string=? current-token "")
                     (cons (categorize-token (string char)) tokens)
                     (cons (categorize-token (string char))
                           (cons (categorize-token current-token) tokens)))
                 ""
                 line
                 (add1 column)
                 #f)]

          ;; Handle quoted strings as a single token
          [(char=? char #\")
           (let-values ([(string-token rest-chars new-column) 
                         (read-string char column)]) ; read-string processes the entire quoted string
             (loop rest-chars
                   (cons string-token tokens)
                   ""
                   line
                   new-column
                   #f))]

          ;; Process decimal points within numbers
          [(char=? char #\.)
           (if (and (not (string=? current-token "")) (string->number current-token))
               ;; We're in the middle of a number, so this is a decimal point
               (loop (read-char)
                     tokens
                     (string-append current-token ".")
                     line
                     (add1 column)
                     #f)
               ;; Otherwise, treat it as a separate punctuation token
               (loop (read-char)
                     (cons `(punctuation ".") 
                           (if (string=? current-token "")
                               tokens
                               (cons (categorize-token current-token) tokens)))
                     ""
                     line
                     (add1 column)
                     #f))]

          ;; Read characters and build up the current token
          [else 
           (loop (read-char) 
                 tokens 
                 (string-append current-token (string char))
                 line
                 (add1 column)
                 #f)])))))




;; Simplified handle-operator-or-punctuation for single-character operators only
(define (handle-operator-or-punctuation char next-char tokens line column continue)
  (continue (read-char)
            (cons (categorize-token (string char)) tokens)
            ""
            line
            (add1 column)
            #f))

(define (char-operator? char)
  (member char '(#\: #\< #\> #\= #\+ #\- #\* #\/)))

(define (char-punctuation? char)
  (member char '(#\( #\) #\, #\;)))

(define (valid-char? char)
  (or (char-alphabetic? char)
      (char-numeric? char)
      (char-operator? char)
      (char-punctuation? char)
      (char-whitespace? char)
      (char=? char #\")
      (char=? char #\.)))

(define (skip-to-end-of-line first-char line column tokens continue add-newline)
  (let loop ((char first-char))
    (cond
      [(or (eof-object? char) (char=? char #\newline))
       ;; Add a newline token after a comment line
       (if add-newline
           (continue (read-char) (cons `(newline) tokens) "" (add1 line) 1 #t)
           (continue (read-char) tokens "" (add1 line) 1 #t))]
      [else (loop (read-char))])))  ; Keep reading until end of line

(define (char-whitespace? char)
  (or (char=? char #\space)
      (char=? char #\tab)
      (char=? char #\return)))

; Helper function to read the entire quoted string
(define (read-string start-char column)
  (let loop ((char (read-char)) 
             (str (string start-char)) 
             (current-column (add1 column)))
    (cond
      [(eof-object? char) 
       (error (format "Unterminated string starting at column ~a" column))]
      [(char=? char #\") 
       (values `(string ,(string-append str (string char))) 
               (read-char) 
               (add1 current-column))]
      [else (loop (read-char) 
                  (string-append str (string char)) 
                  (add1 current-column))])))

(define (categorize-token token)
  (let ([lower-token (string-downcase token)])
    (cond
      [(string->number token) 
       (if (string-contains? token ".")
           `(real ,token)
           `(integer ,token))]
      [(member lower-token '("def" "enddef" "end" "if" "then" "endif" "print" "return" "while" "do" "endwhile" "or" "and" "not"))
       `(keyword ,lower-token)]
      [(string=? token ":=") `(assign ,token)]
      [(member token '("=" "<>" "><" ">" ">=" "<" "<="))
       `(compare ,token)]
      [(string=? token "+") `(plus ,token)]
      [(string=? token "-") `(minus ,token)]
      [(string=? token "*") `(times ,token)]
      [(string=? token "/") `(divides ,token)]
      [(string=? token "(") `(lparen ,token)]
      [(string=? token ")") `(rparen ,token)]
      [(string=? token ",") `(comma ,token)]
      [(string=? token ";") `(semicolon ,token)]
      [(string=? token ":") `(colon ,token)]
      [else `(id ,token)])))


;; PARSER

; Global variables to manage token consumption
(define current-token null)
(define tokens null)

; Initialize the parser with a list of tokens
(define (initialize-parser token-list)
  (set! tokens token-list)
  (advance-token))

; Advance to the next token
(define (advance-token)
  (if (null? tokens)
      (set! current-token null)
      (begin
        (set! current-token (car tokens))
        (set! tokens (cdr tokens)))))

; Match and consume a token of expected type
(define (match expected-type)
  (if (eq? (car current-token) expected-type)
      (let ((matched-token current-token))
        (advance-token)
        matched-token)
      (error (format "Expected ~a, but found ~a" expected-type (car current-token)))))

; Main parsing function
(define (parse token-list)
  (initialize-parser token-list)
  (parse-lines))

; Parse <Lines> at the top level, handling `end` and `enddef`
(define (parse-lines)
  (if (null? current-token)
      '()
      (let loop ()
        (cond
          ((null? current-token) '())
          ((eq? (car current-token) 'newline)
           (match 'newline)
           (loop))
          ;; Skip `end` and `enddef` at the top level
          ((and (eq? (car current-token) 'keyword)
                (member (string-downcase (cadr current-token)) '("end" "enddef")))
           (advance-token) ; Ensure `end` or `enddef` is advanced correctly
           (loop)) ; Continue to next block
          ;; Parse the next block of statements
          (else
           (let ((statements (parse-statements)))
             (if (null? current-token)
                 (list statements)
                 (cons statements (loop)))))))))

; Helper to format assignment for project documentation syntax
(define (format-assignment id expr)
  `(STMT (ID ,id) (assign-op ":=") ,expr))

; Parse <Statements> with end-of-block handling for multiple keywords
(define (parse-statements)
  (let loop ((stmts '()))
    (cond
      ((null? current-token) 
       (reverse stmts))
      ((eq? (car current-token) 'newline)
       (match 'newline)
       (if (and (not (null? stmts)) (eq? (car (car stmts)) 'colon))
           (error "Syntax error: Line ended with a colon and no following statement")
           (loop stmts)))
      ;; Stop parsing when encountering end-of-block keywords, including END
      ((and (eq? (car current-token) 'keyword)
            (member (string-downcase (cadr current-token)) '("endif" "endwhile" "else" "end" "enddef")))
       (reverse stmts)) ; Exit without consuming the end-of-block keyword
      (else
       (let ((stmt (parse-statement)))
         (cond
           ((null? current-token) 
            (if (eq? (car stmt) 'colon)
                (error "Syntax error: Line ended with a colon and no following statement")
                (reverse (cons stmt stmts))))
           ((eq? (car current-token) 'newline)
            (match 'newline)
            (if (eq? (car stmt) 'colon)
                (error "Syntax error: Line ended with a colon and no following statement")
                (loop (cons stmt stmts))))
           ((member (car current-token) '(colon semicolon))
            (let ((separator (match (car current-token)))) ; consume the separator
              (loop (cons separator (cons stmt stmts)))))
           (else (loop (cons stmt stmts)))))))))

; Parse <Statement>
(define (parse-statement)
  (cond
    ((null? current-token) 
     (error "Unexpected end of input"))
    ((eq? (car current-token) 'newline)
     (match 'newline)
     (parse-statement))  ; Skip empty lines
    ((eq? (car current-token) 'comment)
     (let ((comment (match 'comment)))
       (list 'comment (cadr comment))))
    ((eq? (car current-token) 'keyword)
     (case (string-downcase (cadr current-token))
       (("def") (parse-def-statement))
       (("enddef") (begin (match 'keyword) (list 'enddef)))
       (("end") (begin (match 'keyword) (list 'end)))
       (("if") (parse-if-statement))
       (("print") (parse-print-statement))
       (("return") (parse-return-statement))
       (("while") (parse-while-statement))
       (else (error (format "Unexpected keyword: ~a" (cadr current-token))))))
    ((eq? (car current-token) 'id) 
     (parse-id-statement))
    ((member (car current-token) '(colon semicolon))
     (let ((sep (match (car current-token))))
       sep))
    (else (error (format "Unexpected token type: ~a" (car current-token))))))

; Parse DEF statement
(define (parse-def-statement)
  (match 'keyword) ; Match DEF keyword
  (let ((func-name (match 'id))) ; Match function name
    (match 'lparen) ; Match opening parenthesis for parameters
    (let ((params (parse-id-list))) ; Parse parameter list
      (match 'rparen) ; Match closing parenthesis
      `(STMT (def-statement ,(cadr func-name) ,params))))) ; Return DEF statement

; Parse IF statement, include THEN and ENDIF in the output
(define (parse-if-statement)
  (match 'keyword) ; IF
  (let ((condition (parse-expression)))
    (match 'keyword) ; THEN
    (let ((then-statements (parse-statements)))
      (match 'keyword) ; ENDIF
      `(STMT (if-statement ,condition (then ,then-statements) (endif))))))

; Parse PRINT statement
(define (parse-print-statement)
  (match 'keyword) ; Match the PRINT keyword
  `(STMT (print-statement ,(parse-print-list)))) ; Parse the list of expressions for PRINT

; Parse RETURN statement
(define (parse-return-statement)
  (match 'keyword) ; RETURN
  `(STMT (return-statement ,(parse-expression))))

; Parse WHILE statement, include DO and ENDWHILE in the output
(define (parse-while-statement)
  (match 'keyword) ; Match WHILE
  (let ((condition (parse-expression))) ; Parse the WHILE condition
    (match 'keyword) ; Match DO keyword
    (let ((body (parse-statements))) ; Parse the body of the WHILE loop
      (if (and (not (null? current-token))
               (eq? (car current-token) 'keyword)
               (string-ci=? (cadr current-token) "endwhile"))
          (begin
            (match 'keyword) ; Consume the ENDWHILE keyword
            `(STMT (while-statement ,condition (do ,body) (endwhile)))) ; Construct WHILE statement
          (error "Expected ENDWHILE at the end of WHILE statement")))))

; Parse ID statement (assignment or function call)
(define (parse-id-statement)
  (let ((id (match 'id)))
    (cond
      ((eq? (car current-token) 'assign)
       (match 'assign)
       (let ((expr (parse-expression)))
         (format-assignment (cadr id) expr)))
      ((eq? (car current-token) 'punctuation)
       (match 'punctuation) ; (
       (let ((args (parse-expression-list)))
         (match 'punctuation) ; )
         `(STMT (function-call-statement (ID ,(cadr id)) ,args))))
      (else `(ID ,(cadr id))))))

; Parse <ID List>
(define (parse-id-list)
  (let ((id (match 'id)))
    (if (and (not (null? current-token)) (eq? (car current-token) 'punctuation) (string=? (cadr current-token) ","))
        (begin
          (match 'punctuation)
          (cons `(ID ,(cadr id)) (parse-id-list)))
        (list `(ID ,(cadr id))))))

; Parse <Expression List>
(define (parse-expression-list)
  (let ((expr (parse-expression)))
    (if (and (not (null? current-token)) (eq? (car current-token) 'punctuation) (string=? (cadr current-token) ","))
        (begin
          (match 'punctuation)
          (cons expr (parse-expression-list)))
        (list expr))))

; Parse <Print List>
(define (parse-print-list)
  (let ((expr (parse-expression))) ; Parse the first expression
    (if (and (not (null? current-token)) 
             (eq? (car current-token) 'semicolon))
        (begin
          (match 'semicolon) ; Consume the semicolon separator
          (cons expr (parse-print-list))) ; Parse additional expressions after the semicolon
        (list expr)))) ; Return the single expression if no semicolon is found

; Parse <Expression>
(define (parse-expression)
  `(expr ,(parse-and-exp)))

; Parse <And Exp>
(define (parse-and-exp)
  (let ((left (parse-not-exp)))
    (if (and (not (null? current-token)) (eq? (car current-token) 'keyword) (string-ci=? (cadr current-token) "and"))
        `(and-expr ,left "and" ,(parse-and-exp))
        `(and-expr ,left))))

; Parse <Not Exp>
(define (parse-not-exp)
  (if (and (not (null? current-token)) (eq? (car current-token) 'keyword) (string-ci=? (cadr current-token) "not"))
      `(not-expr "not" ,(parse-compare-exp))
      `(not-expr ,(parse-compare-exp))))

; Parse <Compare Exp>
(define (parse-compare-exp)
  (let ((left (parse-add-exp)))
    (if (and (not (null? current-token)) (eq? (car current-token) 'compare))
        (let ((op (match 'compare)))
          `(compare-expr ,left ,(cadr op) ,(parse-compare-exp)))
        `(compare-expr ,left))))

; Parse <Add Exp>
(define (parse-add-exp)
  (let ((left (parse-mult-exp)))
    (if (and (not (null? current-token)) (member (car current-token) '(plus minus)))
        (let ((op (match (car current-token))))
          `(add-expr ,left ,(cadr op) ,(parse-add-exp)))
        `(add-expr ,left))))

; Parse <Mult Exp>
(define (parse-mult-exp)
  (let ((left (parse-negate-exp)))
    (if (and (not (null? current-token)) (member (car current-token) '(times divides)))
        (let ((op (match (car current-token))))
          `(mult-expr ,left ,(cadr op) ,(parse-mult-exp)))
        `(mult-expr ,left))))

; Parse <Negate Exp> 
(define (parse-negate-exp)
  (if (and (not (null? current-token)) (eq? (car current-token) 'minus))
      `(negate-expr "-" ,(parse-value))
      `(negate-expr ,(parse-value))))

; Parse <Value List>
(define (parse-value-list)
  (let ((val (parse-value)))
    (if (and (not (null? current-token)) (eq? (car current-token) 'punctuation) (string=? (cadr current-token) ","))
        (begin
          (match 'punctuation)
          (cons val (parse-value-list)))
        (list val))))

; Parse <Constant List>
(define (parse-constant-list)
  (let ((const (parse-constant)))
    (if (and (not (null? current-token)) (eq? (car current-token) 'punctuation) (string=? (cadr current-token) ","))
        (begin
          (match 'punctuation)
          (cons const (parse-constant-list)))
        (list const))))

; Parse <Integer List>
(define (parse-integer-list)
  (let ((int-val (parse-constant)))
    (if (and (not (null? current-token)) (eq? (car current-token) 'punctuation) (string=? (cadr current-token) ","))
        (begin
          (match 'punctuation)
          (cons int-val (parse-integer-list)))
        (list int-val))))

; Parse <Value>
(define (parse-value)
  (cond
    [(eq? (car current-token) 'lparen)
     (match 'lparen)
     (let ((expr (parse-expression))) ; Parse the entire subexpression
       (match 'rparen)               ; Expect and match closing parenthesis here
       `(value (parenthesized-expr ,expr)))]

    [(eq? (car current-token) 'id)
     (let ((id (match 'id)))
       (if (and (not (null? current-token)) (eq? (car current-token) 'lparen))
           (begin
             (match 'lparen)
             (let ((args (parse-expression-list)))
               (match 'rparen)
               `(value (function-call (ID ,(cadr id)) ,args))))
           `(value (ID ,(cadr id)))))]
    [(member (car current-token) '(integer real string))
     `(value ,(parse-constant))]  ; call parse-constant for uniformity

    [else (error (format "Unexpected token in value: ~a" (car current-token)))]))

; Parse <Constant>
(define (parse-constant)
  (let ((const (match (car current-token))))
    `(constant ,(car const) ,(cadr const))))

;; Testing Calls
(displayln "Scanner Tokens:")
(scan-file "filename.txt")

(displayln "Parse Tree:")
(parse (scan-file "filename.txt"))