#lang racket

(provide rename evaluate translate generate)

;; Description of language syntax:
;
; <e> ::= <v>
;       | <x>
;       | (<e> <e>)
;       | (let (<x> <e>) <e>)
;       | (if <e> <e> <e>)
;       | (box <e>)
;       | (set-box! <e> <e> <e>)
;       | (unbox <e>)
;       | (cons <e> <e>)
;       | (car <e>)
;       | (cdr <e>)
;       | (+ <e> <e>)
;       | (- <e> <e>)
;       | (* <e> <e>)
;       | (/ <e> <e>)
;       | (< <e> <e>)
;       | (<= <e> <e>)
;       | (= <e> <e>)
;       | (> <e> <e>)
;       | (>= <e> <e>)
;       | (lambda? <e>)
;       | (void? <e>)
;       | (null? <e>)
;       | (cons? <e>)
;       | (boolean? <e>)
;       | (integer? <e>)
;       | (string? <e>)
;       | (equal? <e> <e>)
;       | (begin <e> ...)
;
; <v> ::= (lambda <f> (<x>) <e>)
;       | void
;       | null
;       | (cons <v> <v>)
;       | true
;       | false
;       | <integer>
;       | <string>

;; Environment lookup
(define (lookup x env)
  (cdr (or (assoc x env) (cons x #f))))

;; Identifier postfix
(define (postfix x i)
  (string->symbol (string-append (symbol->string x) "_" (number->string i))))

(define (recursive? var val)
  (match val
    [`(lambda ,f (,x) ,e) (equal? var f)]
    [_ #f]))

;; Substructure test
(define (substruct? v1 v2)
  (match v1
    [`(cons ,v11 ,v12) (or (equal? v11 v2) (substruct? v11 v2)
                           (equal? v12 v2) (substruct? v12 v2))]
    [_ #f]))

;; Free-Variable test
(define (free? var expr)
  (match expr
    [`(lambda ,f (,x) ,e)
     (and (not (equal? var f)) (not (equal? var x)) (free? var e))]
    [`void #f]
    [`null #f]
    [`true #f]
    [`false #f]
    [(? integer?) #f]
    [(? string?) #f]
    [(? symbol?) (equal? var expr)]
    [`(let (,x ,e1) ,e2)
     (or (free? var e1) (and (not (equal? var x)) (free? var e2)))]
    [`(if ,e1 ,e2 ,e3)
     (or (free? var e1) (free? var e2) (free? var e3))]
    [`(box ,e) (free? var e)]
    [`(set-box! ,e1 ,e2 ,e3)
     (or (free? var e1) (free? var e2) (free? var e3))]
    [`(unbox ,e) (free? var e)]
    [`(cons ,e1 ,e2) (or (free? var e1) (free? var e2))]
    [`(car ,e) (free? var e)]
    [`(cdr ,e) (free? var e)]
    [`(,(or `+ `- `* `/ `< `<= `= `> `>=) ,e1 ,e2)
     (or (free? var e1) (free? var e2))]
    [`(lambda? ,e) (free? var e)]
    [`(void? ,e) (free? var e)]
    [`(null? ,e) (free? var e)]
    [`(cons? ,e) (free? var e)]
    [`(boolean? ,e) (free? var e)]
    [`(integer? ,e) (free? var e)]
    [`(string? ,e) (free? var e)]
    [`(equal? ,e1 ,e2) (or (free? var e1) (free? var e2))]
    [`(begin . ,es) (ormap (curry free? var) es)]
    [`(,e1 ,e2) (or (free? var e1) (free? var e2))]
    [_ #f]))

;; Value test (weak-head normal form, with only pure subexpressions)
(define (value? expr [head #t] #:weak [weak #t])
  (let ([recur (λ (e) (value? e #f #:weak weak))])
    (match expr
      [`(lambda ,f (,x) ,e) #t]
      [`void #t]
      [`null #t]
      [`(cons ,e1 ,e2) (and (recur e1) (recur e2))]
      [`true #t]
      [`false #t]
      [(? integer?) #t]
      [(? string?) #t]
      [(? symbol?) (and (not head) weak)]
      [`(let (,x ,e1) ,e2)
       (and weak (not head) (recur e1) (recur e2))]
      [`(if ,e1 ,e2 ,e3)
       (and weak (not head) (recur e1) (recur e2) (recur e3))]
      [`(,(or `box `unbox) ,e) #f]
      [`(set-box! ,e1 ,e2) #f]
      [`(,(or `car  `cdr) ,e) (and weak (not head) (recur e))]
      [`(,(or `+ `- `* `/ `< `<= `= `> `>=) ,e1 ,e2)
       (and weak (not head) (recur e1) (recur e2))]
      [`(,(or `lambda?  `void? `null? `cons? `boolean? `integer? `string?) ,e)
       (and weak (not head) (recur e))]
      [`(equal? ,e1 ,e2)
       (and weak (not head) (recur e1) (recur e2))]
      [`(begin . ,es) (and weak (andmap (λ (e) (recur e)) es))]
      [`(,e1 ,e2) #f]
      [_ #f])))

;; Runtime-type checks
(define (has-type test form v)
  (cond
    [(not (value? v)) `(,form ,v)]
    [(if (list? test) (member v test) (test v)) `true]
    [else `false]))

;; Arithmetic operations
(define (arithmetic form v1 v2)
  (let* ([boolify (λ (b) (if b `true `false))]
         [f (match form
              ['+ +]
              ['- -]
              ['* *]
              ['/ (compose floor /)]
              ['< (compose boolify <)]
              ['<= (compose boolify <=)]
              ['= (compose boolify =)]
              ['>= (compose boolify >=)]
              ['> (compose boolify >)])])
    (cond
      [(and (integer? v1) (integer? v2)) (f v1 v2)]
      [(and (value? v1) (value? v2))
       (raise-user-error 'evaluate "~a of non-integers: ~a" form `(,form ,v1 ,v2))]
      [else `(,form ,v1 ,v2)])))

;; Variable rename (to avoid shadowing)
(define (rename expr [env null])
  (match expr
    ; Value expressions
    [`void `void]
    [`null `null]
    [`true `true]
    [`false `false]
    [(? integer?) expr]
    [(? string?) expr]

    ; Variable expression
    [(? symbol?)
     (let ([i (lookup expr env)])
       (if i (postfix expr i) expr))]

    ; Lambda expression
    [`(lambda ,f (,x) ,e)
     (let* ([i (+ (or (lookup f env) -1) 1)]
            [j (+ (or (lookup x env) -1) 1)]
            [fi (postfix f i)]
            [xj (postfix x j)]
            [eij (rename e (list* (cons x j) (cons f i) env))])
       `(lambda ,fi (,xj) ,eij))]

    ; Let expressions
    [`(let (,x ,e1) ,e2)
     (let* ([e1 (rename e1 env)]
            [i (+ (or (lookup x env) -1) 1)]
            [xi (postfix x i)]
            [e2i (rename e2 (cons (cons x i) env))])
       `(let (,xi ,e1) ,e2i))]

    ; Reference expressions
    [`(box ,e) `(box ,(rename e env))]
    [`(set-box! ,e1 ,e2) `(set-box! ,(rename e1 env) ,(rename e2 env))]
    [`(unbox ,e) `(unbox ,(rename e env))]

    ; Pair expressions
    [`(cons ,e1 ,e2) `(cons ,(rename e1 env) ,(rename e2 env))]
    [`(car ,e) `(car ,(rename e env))]
    [`(cdr ,e) `(cdr ,(rename e env))]

    ; Arithmetic expressions
    [`(+ ,e1 ,e2) `(+ ,(rename e1 env) ,(rename e2 env))]
    [`(- ,e1 ,e2) `(- ,(rename e1 env) ,(rename e2 env))]
    [`(* ,e1 ,e2) `(* ,(rename e1 env) ,(rename e2 env))]
    [`(/ ,e1 ,e2) `(/ ,(rename e1 env) ,(rename e2 env))]
    [`(< ,e1 ,e2) `(< ,(rename e1 env) ,(rename e2 env))]
    [`(<= ,e1 ,e2) `(<= ,(rename e1 env) ,(rename e2 env))]
    [`(= ,e1 ,e2) `(= ,(rename e1 env) ,(rename e2 env))]
    [`(> ,e1 ,e2) `(> ,(rename e1 env) ,(rename e2 env))]
    [`(>= ,e1 ,e2) `(>= ,(rename e1 env) ,(rename e2 env))]

    ; Conditional expression
    [`(if ,e1 ,e2 ,e3)
     `(if ,(rename e1 env) ,(rename e2 env) ,(rename e3 env))]

    ; Predicate expressions
    [`(void? ,e) `(void? ,(rename e env))]
    [`(null? ,e) `(null? ,(rename e env))]
    [`(boolean? ,e) `(boolean? ,(rename e env))]
    [`(integer? ,e) `(integer? ,(rename e env))]
    [`(string? ,e) `(string? ,(rename e env))]
    [`(cons? ,e) `(cons? ,(rename e env))]
    [`(lambda? ,e) `(lambda? ,(rename e env))]

    ; Equality expression
    [`(equal? ,e1 ,e2) `(equal? ,(rename e1 env) ,(rename e2 env))]

    ; Sequence expression
    [`(begin . ,es)
     `(begin . ,(map (λ (e) (rename e env)) es))]

    ; Application expression
    [`(,e1 ,e2) `(,(rename e1 env) ,(rename e2 env))]))

;; Set of blocked subexpressions
(define structurality (make-hasheq))

;; Partial evaluation
(define (evaluate expr [env null] [spec #f] [rec null] [stack null])
  (let ([recur (lambda (expr #:speculative? [spec spec] #:recursive [f-x #f] . bs)
                 (evaluate expr (append bs env)
                           spec (if f-x (cons f-x rec) rec) (cons expr stack)))])
  (match expr
    ; Value expressions
    [`void `void]
    [`null `null]
    [`true `true]
    [`false `false]
    [(? integer?) expr]
    [(? string?) expr]

    ; Variable expression
    [(? symbol?)
     (let ([v (lookup expr env)])
       (if (and (not (symbol? v)) (or (not (value? v)) (recursive? expr v))) expr v))]

    ; Lambda expression
    [`(lambda ,f (,x) ,e)
     (let ([v (recur e (cons x (void)) (cons f (void)))])
       `(lambda ,f (,x) ,v))]

    ; Let expressions
    [`(let (,x ,e1) ,e2)
     (let* ([v1 (recur e1)]
            [v2 (recur e2 (cons x v1))])
       (cond
         [(equal? x v2) v1]
         [(and (free? x v2) (not (equal? x v1))) `(let (,x ,v1) ,v2)]
         [else v2]))]

    ; Reference expressions
    [`(box ,e)
     (let ([v (recur e)])
       `(box ,v))]
    [`(set-box! ,e1 ,e2)
     (let ([v1 (recur e1)]
           [v2 (recur e2)])
       `(set-box! ,v1 ,v2))]
    [`(unbox ,e)
     (let ([v (recur e)])
       `(unbox ,v))]

    ; Pair expressions
    [`(cons ,e1 ,e2)
     (let ([v1 (recur e1)]
           [v2 (recur e2)])
       `(cons ,v1 ,v2))]
    [`(car ,e)
     (let ([v (recur e)])
       (if (value? v)
           (match v
             [`(cons ,v1 ,v2) v1]
             [_
              (if spec
                  `(car ,v)
                  (raise-user-error 'evaluate "car of non-pair: ~a" expr))])
           `(car ,v)))]
    [`(cdr ,e)
     (let ([v (recur e)])
       (if (value? v)
           (match v
             [`(cons ,v1 ,v2) v2]
             [_
              (if spec
                  `(cdr ,v)
                  (raise-user-error 'evaluate "cdr of non-pair: ~a" expr))])
           `(cdr ,v)))]

    ; Arithmetic expressions
    [`(+ ,e1 ,e2) (arithmetic '+ (recur e1) (recur e2))]
    [`(- ,e1 ,e2) (arithmetic '- (recur e1) (recur e2))]
    [`(* ,e1 ,e2) (arithmetic '* (recur e1) (recur e2))]
    [`(/ ,e1 ,e2) (arithmetic '/ (recur e1) (recur e2))]
    [`(< ,e1 ,e2) (arithmetic '< (recur e1) (recur e2))]
    [`(<= ,e1 ,e2) (arithmetic '<= (recur e1) (recur e2))]
    [`(= ,e1 ,e2) (arithmetic '= (recur e1) (recur e2))]
    [`(>= ,e1 ,e2) (arithmetic '>= (recur e1) (recur e2))]
    [`(> ,e1 ,e2) (arithmetic '> (recur e1) (recur e2))]

    ; Conditional expression
    [`(if ,e1 ,e2 ,e3)
     (let ([v1 (recur e1)])
       (match v1
         [`false (recur e3)]
         [(? value?) (recur e2)]
         [_ `(if ,v1
                 ,(recur e2 #:speculative? #t)
                 ,(recur e3 #:speculative? #t))]))]

    ; Predicate expressions
    [`(void? ,e) (has-type `(void) `void? (recur e))]
    [`(null? ,e) (has-type `(null) `null? (recur e))]
    [`(boolean? ,e) (has-type `(true false) `boolean? (recur e))]
    [`(integer? ,e) (has-type integer? `integer? (recur e))]
    [`(string? ,e) (has-type string? `string? (recur e))]
    [`(cons? ,e)
     (let ([cons? (match-lambda [`(cons ,v1 ,v2) #t] [_ #f])])
       (has-type cons? `cons? (recur e)))]
    [`(lambda? ,e)
     (let ([lambda? (match-lambda [`(lambda ,f (,x) ,e) #t] [_ #f])])
       (has-type lambda? `lambda? (recur e)))]

    ; Equality expression
    [`(equal? ,e1 ,e2)
     (let ([v1 (recur e1)]
           [v2 (recur e2)])
       (if (and (value? v1) (value? v2))
           (if (equal? v1 v2) `true `false)
           `(equal? ,v1 ,v2)))]

    ; Sequence expression
    [`(begin . ,es)
     (let* ([vs (filter-not value? (map recur (take es (- (length es) 1))))]
            [vf (recur (last es))])
       (if (null? vs)
           vf
           `(begin . ,(append vs (list vf)))))]

    ; Application expression
    [`(,e1 ,e2)
     (let ([v1 (recur e1)]
           [v2 (recur e2)])
       (match v1
         [`(lambda ,f (,x) ,e)
          (recur `(let (,f ,v1) (let (,x ,v2) ,e)) #:recursive (cons f v2))]
         [`(let (,x ,e3), e4)
          (recur `(let (,x ,e3) (,e4 ,v2)))]
         [(? symbol?)
          (match (lookup v1 env)
            [`(lambda ,f (,x) ,e) ; recursive function
             (let ([v0 (or (lookup f rec) #f)])
               (if (and (value? v2) (not (free? x v2))
                        (or (not v0) (not spec) (substruct? v0 v2)))
                   (recur `(let (,x ,v2) ,e) #:recursive (cons f v2))
                   `(,v1 ,v2)))]
            [_ `(,v1 ,v2)])]
         [_ `(,v1 ,v2)]))])))

;; Syntactic translation
(define (translate expr)
  (match expr
    ; Value expressions
    [`void "Void"]
    [`null "Null"]
    [`true "Boolean true"]
    [`false "Boolean false"]
    [(? integer?) (format "Integer ~a" expr)]
    [(? string?) (format "String ~s" expr)]

    ; Variable expression
    [(? symbol?) (symbol->string expr)]

    ; Lambda expression
    [`(lambda ,f (,x) ,e)
     (format "let rec ~a = Lambda (fun ~a -> ~a) in ~a" f x (translate e) f)]

    ; Binding expression
    [`(let (,x ,e1) ,e2)
     (format "let ~a = ~a in ~a" x (translate e1) (translate e2))]

    ; Reference expression
    [`(box ,e) (format "ref (~a)" (translate e))]
    [`(set-box! ,e1 ,e2)
     (format "~a := ~a" (translate e1) (translate e2))]
    [`(unbox ,e) (format "!(~a)" (translate e))]

    ; Pair expressions
    [`(cons ,e1 ,e2) (format "Cons (~a, ~a)" (translate e1) (translate e2))]
    [`(car ,e) (format "car (~a)" (translate e))]
    [`(cdr ,e) (format "cdr (~a)" (translate e))]

    ; Arithmetic expressions
    [`(+ ,e1 ,e2) (format "add (~a) (~a)" (translate e1) (translate e2))]
    [`(- ,e1 ,e2) (format "sub (~a) (~a)" (translate e1) (translate e2))]
    [`(* ,e1 ,e2) (format "mul (~a) (~a)" (translate e1) (translate e2))]
    [`(/ ,e1 ,e2) (format "div (~a) (~a)" (translate e1) (translate e2))]
    [`(< ,e1 ,e2) (format "lt (~a) (~a)" (translate e1) (translate e2))]
    [`(<= ,e1 ,e2) (format "le (~a) (~a)" (translate e1) (translate e2))]
    [`(= ,e1 ,e2) (format "eq (~a) (~a)" (translate e1) (translate e2))]
    [`(>= ,e1 ,e2) (format "ge (~a) (~a)" (translate e1) (translate e2))]
    [`(> ,e1 ,e2) (format "gt (~a) (~a)" (translate e1) (translate e2))]

    ; Conditional expression
    [`(if ,e1 ,e2 ,e3)
     (format "if truth (~a) then (~a) else (~a)"
             (translate e1) (translate e2) (translate e3))]

    ; Predicate expressions
    [`(void? ,e) (format "is_void (~a)" (translate e))]
    [`(null? ,e) (format "is_null (~a)" (translate e))]
    [`(boolean? ,e) (format "is_boolean (~a)" (translate e))]
    [`(integer? ,e) (format "is_integer (~a)" (translate e))]
    [`(string? ,e) (format "is_string (~a)" (translate e))]
    [`(cons? ,e) (format "is_cons (~a)" (translate e))]
    [`(lambda? ,e) (format "is_lambda (~a)" (translate e))]

    ; Equality test
    [`(equal? ,e1 ,e2)
     (format "(~a) = (~a)" (translate e1) (translate e2))]

    ; Sequence expression
    [`(begin . ,es)
     (format "(~a)" (string-join (map translate es) "; "))]

    ; Application expression
    [`(,e1 ,e2)
     (format "app (~a) (~a)" (translate e1) (translate e2))]))

;; Residual compilation
(define (generate expr)
  (translate (evaluate (rename expr))))
