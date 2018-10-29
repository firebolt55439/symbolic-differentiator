(define nil '())
(define (cadr s) (car (cdr s)))
(define (caddr s) (cadr (cdr s)))
(define (error str) (console-error str))

; derive returns the derivative of EXPR with respect to VAR
(define (derive expr var)
  (cond ((number? expr) 0)
        ((variable? expr) (if (same-variable? expr var) 1 0))
        ((sum? expr) (derive-sum expr var))
        ((subtraction? expr) (derive-subtraction expr var))
        ((product? expr) (derive-product expr var))
        ((exp? expr) (derive-exp expr var))
        ((division? expr) (derive-div expr var))
        ((function? expr) (derive-function expr var))
        (else 'Error)))

; Variables are represented as symbols
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

; Numbers are compared with =
(define (=number? expr num)
  (and (number? expr) (= expr num)))

; Sums are represented as lists that start with +.
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        ((and (product? a1) (or (=number? (multiplier a1) -1) (=number? (multiplicand a1) -1)))
          (if (=number? (multiplier a1) -1)
            (make-subtraction a2 (multiplicand a1))
            (make-subtraction a2 (multiplier a1))
          )
        )
        ((and (product? a2) (or (=number? (multiplier a2) -1) (=number? (multiplicand a2) -1)))
          (make-sum a2 a1)
        )
        ((and (number? a2) (not (number? a1))) ; make constants go in front
          (make-sum a2 a1)
        )
        ((and (product? a2) (number? (multiplier a2)) (< (multiplier a2) 0)) ; a + (-bc) = a - bc
          (make-subtraction a1 (make-product (* -1 (multiplier a2)) (multiplicand a2)))
        )
        (else (list '+ a1 a2))))
(define (sum? x)
  (and (list? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s) (caddr s))

; Products are represented as lists that start with *.
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0) ; x * 0 = x
        ((=number? m1 1) m2) ; 1 * x = x
        ((=number? m2 1) m1) ; x * 1 = x
        ((and (number? m1) (number? m2)) (* m1 m2)) ; a * b = ab
        ((equal? m1 m2) (make-exp m1 2)) ; a * a = a^2
        ((and (exp? m1) (exp? m2) (equal? (base m1) (base m2))) ; x^a * x^b = x^(a + b)
          (make-exp (base m1) (make-sum (exponent m1) (exponent m2)))
        )
        ((and (exp? m2) (symbol? m1) (eq? m1 (base m2)))
          (make-exp m1 (make-sum (exponent m2) 1))
        ) ; x * x ^ a = x ^ (a + 1)
        ((and (exp? m1) (symbol? m2) (eq? m2 (base m1)))
          (make-product m2 m1)
        ) ; x ^ a * x = x ^ (a + 1)
        ((and (product? m2) (or (symbol? m1) (exp? m1))
          (or
            (equal? m1 (multiplier m2))
            (and (exp? (multiplier m2)) (equal? m1 (base (multiplier m2))))
            (and (exp? m1) (equal? (base m1) (multiplier m2)))
            (and (exp? m1) (exp? (multiplier m2)) (equal? (base m1) (base (multiplier m2))))
          )) ; a * (a^n b) = a^(n + 1) b
          (make-product
            (make-product m1 (multiplier m2))
            (multiplicand m2)
          )
        )
        ((and (number? m2) (product? m1) (or (number? (multiplier m1)) (number? (multiplicand m1)))) ; a(kx) = (ak)x
          (if (number? (multiplier m1))
            (make-product (* m2 (multiplier m1)) (multiplicand m1))
            (make-product (* m2 (multiplicand m1)) (multiplier m1))
          )
        )
        ((and (number? m1) (product? m2) (or (number? (multiplier m2)) (number? (multiplicand m2)))) ; (kx)a = x(ka)
          (make-product m2 m1)
        )
        ((and (number? m2) (not (number? m1))) ; make sure constant factors are in front
          (make-product m2 m1)
        )
        ((and (product? m1) (product? m2) (number? (multiplier m1)) (number? (multiplier m2))) ; (k x) (c y) = (kc) (xy)
          (make-product
            (* (multiplier m1) (multiplier m2))
            (make-product (multiplicand m1) (multiplicand m2))
          )
        )
        ((and (product? m2) (number? (multiplier m2)) (not (product? m1))) ; bring constants forward in repeated multiplication
          (make-product
            (multiplier m2)
            (make-product
              m1
              (multiplicand m2)
            )
          )
        )
        ((and (function? m1) (not (function? m2))) ; bring coefficients in front of functions
          (make-product m2 m1)
        )
        ((and (division? m1) (division? m2)) ; (a/b) * (c/d) = (ac) / (bd)
          (make-div
            (make-product (numerator m1) (numerator m2))
            (make-product (denominator m1) (denominator m2)))
        )
        ((and (or (number? m1) (symbol? m1)) (division? m2)) ; a * (b/c) = (ab)/c
          (make-div (make-product m1 (numerator m2)) (denominator m2))
        )
        ((and (symbol? m2) (division? m1)) ; (b/c) * a = (ba)/c
          (make-product m2 m1)
        )
        (else (list '* m1 m2))))
(define (product? x)
  (and (list? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))

(define (derive-sum expr var)
  (make-sum (derive (addend expr) var) (derive (augend expr) var))
)

(define (derive-product expr var)
  (make-sum
    (make-product
      (derive (multiplier expr) var)
      (multiplicand expr)
    )
    (make-product
      (multiplier expr)
      (derive (multiplicand expr) var)
    )
  )
)

; Exponentiations are represented as lists that start with ^.
(define (function? expr) (and (list? expr) (contains functions (car expr)))) ; to avoid forward reference
(define (make-exp base exponent)
  (cond
    ((=number? base 0) 0)
    ((=number? exponent 0) 1)
    ((=number? exponent 1) base)
    ((and (number? base) (number? exponent) (< exponent 3) (> exponent 0) (< base 5)) (expt base exponent)) ; evaluate for small numbers / powers
    ((exp? base) (make-exp (cadr base) (make-product (caddr base) exponent))) ; power of a power
    ((and (function? base) (eq? (car base) 'sqrt) (number? exponent)) ; sqrt(x)^a = x^(a/2)
      (make-exp (cadr base) (make-div exponent 2))
    )
    ((and (product? base) (number? (multiplier base)) (exp? (multiplicand base)))
      (make-product
        (make-exp (multiplier base) exponent)
        (make-exp (cadr (multiplicand base)) (make-product (caddr (multiplicand base)) exponent))
      )
    )
    (else (list '^ base exponent)))
)

(define (base exp)
  (cadr exp)
)

(define (exponent exp)
  (caddr exp)
)

(define (exp? exp)
  (and (list? exp) (eq? (car exp) '^))
)

(define x^2 (make-exp 'x 2))
(define x^3 (make-exp 'x 3))

; Extension: chain rule and general power rule for exponents
(define (derive-exp exp var)
  (if (number? (exponent exp))
    (make-product
      (exponent exp)
      (make-product
        (make-exp (base exp) (make-subtraction (exponent exp) 1))
        (if (number? (base exp)) 1 (derive (base exp) var))
      )
    )
    (make-product
      (make-exp (base exp) (exponent exp))
      (make-sum
        (make-product
          (derive (base exp) var)
          (make-div
            (exponent exp)
            (base exp)
          )
        )
        (make-product
          (derive (exponent exp) var)
          (make-ln (base exp))
        )
      )
    )
  )
)

; Extension: subtraction
(define (make-subtraction a1 a2)
  (cond ((=number? a1 0) (make-product -1 a2))
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (- a1 a2))
        ((equal? a1 a2) 0)
        ((and (product? a2) (or (=number? (multiplier a2) -1) (=number? (multiplicand a2) -1)))
          (if (=number? (multiplier a2) -1)
            (make-sum a1 (multiplicand a2))
            (make-sum a1 (multiplier a2))
          )
        )
        ((and (product? a1) (or (=number? (multiplier a1) -1) (=number? (multiplicand a1) -1)))
          (make-product
            -1
            (if (=number? (multiplier a1) -1)
              (make-sum a2 (multiplicand a1))
              (make-sum a2 (multiplier a1))
            )
          )
        )
        ((and (product? a2) (number? (multiplier a2)) (< (multiplier a2) 0)) ; a - (-bc) = a + b
          (make-sum a1 (make-product (* -1 (multiplier a2)) (multiplicand a2)))
        )
        (else (list '- a1 a2))))
(define (subtraction? x)
  (and (list? x) (eq? (car x) '-)))
(define (derive-subtraction expr var)
  (make-subtraction (derive (addend expr) var) (derive (augend expr) var))
)

; Extension: division
(define (make-div m1 m2)
  (cond ((and (=number? m1 0) (=number? m2 0)) (error '0/0))
        ((=number? m1 0) 0)
        ((=number? m2 0) (error "Division by zero"))
        ((equal? m1 m2) 1) ; x / x = 1
        ((or
          (and (exp? m1) (symbol? m2) (eq? (base m1) m2))
          (and (exp? m2) (symbol? m1) (eq? (base m2) m1))) ; simplify x / x^n or x^n / x forms
          (if (exp? m1)
            (make-exp (base m1) (make-subtraction (exponent m1) 1))
            (make-div 1 (make-exp (base m2) (make-subtraction (exponent m2) 1)))
          )
        )
        ((and (product? m1) (number? (multiplier m1))) ; simplify (kx)/y = k*(x/y)
          (make-product (multiplier m1) (make-div (multiplicand m1) m2))
        )
        ((and (exp? m2) (number? (exponent m2)) (< (exponent m2) 0)) ; simplify negative exponents of form x/y^(-2) = xy^2
          (make-product m1 (make-exp (base m2) (- (exponent m2))))
        )
        ((and (division? m1) (not (division? m2))) ; (a / b) / c = a / (bc)
          (make-div (numerator m1) (make-product (denominator m1) m2))
        )
        ((and (division? m2) (not (division? m1))) ; c / (a / b) = (cb) / a
          (make-div (make-product (denominator m2) m1) (numerator m2))
        )
        ((and
          (product? m1) (product? m2)
          (or
            (equal? (multiplier m1) (multiplier m2))
            (equal? (multiplier m1) (multiplicand m2))
            (equal? (multiplicand m1) (multiplier m2))
            (equal? (multiplicand m1) (multiplicand m2))
          )
          ) ; implement general cancellation (ab) / (cd) if any of {a, b} equals any of {c, d}
          (cond
            ((equal? (multiplier m1) (multiplier m2))
              (make-div (multiplicand m1) (multiplicand m2))
            )
            ((equal? (multiplier m1) (multiplicand m2))
              (make-div (multiplicand m1) (multiplier m2))
            )
            ((equal? (multiplicand m1) (multiplier m2))
              (make-div (multiplier m1) (multiplicand m2))
            )
            ((equal? (multiplicand m1) (multiplicand m2))
              (make-div (multiplier m1) (multiplier m2))
            )
          )
        )
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (/ m1 m2))
        (else (list '/ m1 m2))))
(define (numerator expr) (cadr expr))
(define (denominator expr) (caddr expr))
(define (derive-div expr var)
  (make-div
    (make-sum
      (make-product
        (derive (numerator expr) var)
        (denominator expr)
      )
      (make-product
        -1
        (make-product
          (numerator expr)
          (derive (denominator expr) var)
        )
      )
    )
    (make-exp (denominator expr) 2)
  )
)
(define (division? div)
  (and (list? div) (eq? (car div) '/))
)

; Extension: infix to postfix + prefix
(define operators '(+ - * / ^))
(define op_precedence '(1 1 2 2 3))
(define functions '(sin cos tan sinh cosh tanh log ln sqrt abs))
(define seen-variables (list))
(define (contains lst x)
  (cond
    ((null? lst) #f)
    ((eq? (car lst) x) #t)
    (else (contains (cdr lst) x))
  )
)
(define inf-precedence 1e9)
(define (precedence x)
  (define (helper a b)
    (cond
      ((null? a) inf-precedence)
      ((eq? (car a) x) (car b))
      (else (helper (cdr a) (cdr b)))
    )
  )
  (helper operators op_precedence)
)
(define (parse-infix expr_orig)
  (define output_queue (list))
  (define op_stack (list))
  (define (push_stack x)
    ; (console-log "Push op:" x)
    (set! op_stack (append (list x) op_stack))
  )
  (define (pop_stack)
    (define ret (car op_stack))
    ; (console-log "Pop op:" ret)
    (set! op_stack (cdr op_stack))
    ret
  )
  (define (top_stack)
    (if (null? op_stack) nil (car op_stack))
  )
  (define (push_queue x)
    ; (console-log "Push queue:" x)
    (set! output_queue (append output_queue (list x)))
  )
  (define (pop_queue)
    (define ret (car output_queue))
    ; (console-log "Pop queue:" ret)
    (set! output_queue (cdr output_queue))
    ret
  )
  (define left-paren 'left-paren)
  (define right-paren 'right-paren)
  (define (flatten expr)
    (cond
      ((null? expr) nil)
      ((list? (car expr))
        (append '(left-paren) (flatten (car expr)) '(right-paren) (flatten (cdr expr)))
      )
      (else (cons (car expr) (flatten (cdr expr))))
    )
  )
  (define (parse-helper expr) ; Shunting-Yard Algorithm
    (cond
      ((null? expr) nil)
      ((number? (car expr)) (push_queue (car expr))) ; check for number literal
      ((eq? (car expr) left-paren) (push_stack (car expr))) ; handle left parenthesis
      ((eq? (car expr) right-paren)
        (begin
          (define (loop_fn)
            (define top_op (top_stack))
            (if (and (not (eq? top_op left-paren)) (not (null? top_op)))
              (begin
                (push_queue (pop_stack))
                (loop_fn)
              )
            )
          )
          (loop_fn)
          (if (not (eq? (top_stack) left-paren))
            (error "Input has mismatched parenthesis")
            (pop_stack)
          )
        )
      ) ; handle right parenthesis
      ((symbol? (car expr)) ; handle symbol
        (cond
          ((contains operators (car expr)) ; handle operators
            (begin
              (define cur_precedence (precedence (car expr)))
              (define (loop_fn)
                (define top_op (top_stack))
                (if
                  (and
                    (not (null? top_op))
                    (not (eq? top_op left-paren))
                    (or
                      (contains functions top_op)
                      (begin
                        (define top_op_precedence (precedence top_op))
                        (or
                          (> top_op_precedence cur_precedence)
                          (= top_op_precedence cur_precedence) ; TODO: This condition needs to check for left associativity of operator
                        )
                      )
                    )
                  )
                  (begin
                    (push_queue (pop_stack))
                    (loop_fn)
                  )
                )
              )
              (loop_fn)
              (push_stack (car expr))
            )
          )
          ((contains functions (car expr)) (push_stack (car expr))) ; handle functions
          (else (begin
            (set! seen-variables (append seen-variables (list (car expr))))
            (push_queue (car expr)))
          ) ; is a variable otherwise, so treat it like a number
        )
      )
    )
    (if (not (null? expr))
      (parse-helper (cdr expr))
    )
  )
  (define flattened_exp (flatten expr_orig))
  (parse-helper flattened_exp)
  ; (console-log "(pre) Output queue:" output_queue)
  (set! output_queue (append output_queue op_stack))
  ; (console-log "Output queue:" output_queue)
  (define prefix-stack (list))
  (define (push_prefix x)
    (set! prefix-stack (append (list x) prefix-stack))
  )
  (define (pop_prefix)
    (define ret (car prefix-stack))
    (set! prefix-stack (cdr prefix-stack))
    ret
  )
  (define (prefix-conversion expr)
    (cond
      ((null? expr) nil)
      ((contains functions (car expr))
        (push_prefix (list (car expr) (pop_prefix)))
      )
      ((contains operators (car expr))
        (define right (pop_prefix))
        (define left (pop_prefix))
        (push_prefix (list (car expr) left right))
      )
      (else (push_prefix (car expr)))
    )
    (if (not (null? expr))
      (prefix-conversion (cdr expr))
    )
  )
  (prefix-conversion output_queue)
  (car prefix-stack)
)

; Extension: one-argument functions with chain rule
(define (make-ln m1) ; special function: ln
  (cond
    ((=number? m1 1) 0) ; ln(1) = 0
    ((and (exp? m1) (eq? (base m1) 'e)) (exponent m1)) ; ln(e^x) = x
    ((and (exp? m1)) (make-product (exponent m1) (make-ln (base m1)))) ; ln(a^b) = b*ln(a)
    ((and (symbol? m1) (eq? m1 'e)) 1) ; ln(e) = 1
    (else (list 'ln m1))
  )
)
(define (make-sqrt m1) ; special function: sqrt
  (cond
    ((=number? m1 0) 0) ; sqrt(0) = 0
    ((=number? m1 1) 1) ; sqrt(1) = 1
    ((and (exp? m1) (number? (exponent m1)))
      (if (even? (exponent m1))
        (if (even? (/ (exponent m1) 2))
          (make-exp (base m1) (/ (exponent m1) 2))
          (list 'abs (make-exp (base m1) (/ (exponent m1) 2)))
        )
        (make-exp (base m1) (make-div (exponent m1) 2))
      )
    ) ; sqrt(x^(2n)) = x^n for n = integer OR sqrt(x^k) = x^(k/2) for k = integer
    (else (list 'sqrt m1))
  )
)
(define (derive-function expr var)
  (define func (car expr))
  (define derivative-func (cond
    ((eq? func 'sin)
      (lambda (x) (list 'cos x))
    )
    ((eq? func 'cos)
      (lambda (x) (make-product -1 (list 'sin x)))
    )
    ((eq? func 'tan)
      (lambda (x) (make-sum 1 (make-exp (list 'tan x) 2)))
    )
    ((eq? func 'sinh)
      (lambda (x) (list 'cosh x))
    )
    ((eq? func 'cosh)
      (lambda (x) (list 'sinh x))
    )
    ((eq? func 'tanh)
      (lambda (x) (make-subtraction 1 (make-exp (list 'tanh x) 2)))
    )
    ((or (eq? func 'log) (eq? func 'ln))
      (lambda (x) (make-div 1 x))
    )
    ((eq? func 'sqrt)
      (lambda (x) (make-div 1 (make-product 2 (make-sqrt x))))
    )
    ((eq? func 'abs)
      (derivative-dne "Cannot take derivative of abs")
    )
    (else (error "Unknown function derivative"))
  ))
  (make-product
    (derivative-func (cadr expr))
    (derive (cadr expr) var)
  )
)

; Extension: derivative with infix input and output
(define (leq? a b) (not (< b a)))
(define safe-operators '(+ *))
(define (prefix-to-infix expr)
  ; (console-log "Called with" expr)
  (cond
    ((null? expr) nil)
    ((symbol? expr) (list (list expr) inf-precedence))
    ((number? expr) (list (list expr) inf-precedence))
    ((contains functions (car expr))
      (list (list (car expr) (car (prefix-to-infix (cadr expr)))) inf-precedence)
    )
    (else
      (begin
        ; (console-log "L/R1" (car expr) (cadr expr) (caddr expr))
        (define lr (list (prefix-to-infix (cadr expr)) (prefix-to-infix (caddr expr))))
        (define cur_precedence (precedence (car expr)))
        (define left (car lr))
        (define right (cadr lr))
        ; (console-log left)
        ; (console-log right)
        ; (console-log "L/R2" (car expr) (cadr expr) (caddr expr))
        (define ret (list)) ; (list left (car expr) right)
        (if (or
            (< (cadr left) cur_precedence)
            (and
              (= (cadr left) cur_precedence)
              (not (eq? (cadr (cdr left)) (car expr))) ; put parenthesis if operators don't match up
              (not (contains safe-operators (caddr left))) ; and not "safe" operators (e.g. * and + is safe)
            )
          )
          (define ret (append ret (list (car left))))
          (define ret (append ret (car left)))
        )

        (define ret (append ret (list (car expr))))
        (if (or
            (< (cadr right) cur_precedence)
            (and
              (= (cadr right) cur_precedence) ; if same precedence, then
              (not (eq? (cadr (cdr right)) (car expr))) ; put parenthesis if operators don't match up
              (not (contains safe-operators (caddr right))) ; and not "safe" operators (e.g. * and + is safe)
            )
            (and
              (exp? expr) ; special handling for exponentials
              (or
                (exp? (caddr expr)) ; nested exponentials are right-associative
                ; (= 1 (precedence (caddr right))) ; if sum or difference inside exponential
              )
            )
          )
          (define ret (append ret (list (car right))))
          (define ret (append ret (car right)))
        )
        (list ret cur_precedence (car expr))
      )
    )
  )
)
(define (derive-infix expr var)
  (set! seen-variables (list))
  (define parsed-infix (parse-infix expr))
  (console-log "Parsed Infix:" parsed-infix)
  (define derivative (derive parsed-infix var))
  (console-log "Derivative:" derivative)
  (define ret (prefix-to-infix derivative))
  (if (not (list? ret))
    (error 'Could not take derivative)
    (list (car ret) seen-variables)
  )
)
