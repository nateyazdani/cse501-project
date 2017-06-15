#lang racket

(require "../generator.rkt")


;; Commands
(define (get-residual)
  (evaluate (rename `((,interpret ,program) (cons (cons "k" k) null)))))

(define (get-output)
  (translate (get-residual)))

;; Macros
(define (lyst . xs)
  (foldl (lambda (x xs) `(cons ,x ,xs)) `null xs))

(define (kond . branches)
  (if (null? branches)
      `void
      (let* ([branch (first branches)]
             [bool (first branch)]
             [expr (second branch)])
        (if (equal? bool 'else)
            expr
            `(if ,bool
                 ,expr
                 ,(apply kond (rest branches)))))))

(define (lets bindings body)
  (if (null? bindings)
      body
      `(let ,(first bindings)
         ,(lets (rest bindings) body))))

(define (sum v ei ej e)
  `(cons "sum" (cons (cons (cons ,v ,ei) ,ej) ,e)))

(define (prod v ei ej e)
  `(cons "prod" (cons (cons (cons ,v ,ei) ,ej) ,e)))

(define (fix v e0 e)
  `(cons "fix" (cons (cons ,v ,e0) ,e)))

(define (add expr-l expr-r)
  `(cons "+" (cons ,expr-l ,expr-r)))

(define (sub expr-l expr-r)
  `(cons "-" (cons ,expr-l ,expr-r)))

(define (mul expr-l expr-r)
  `(cons "*" (cons ,expr-l ,expr-r)))

(define (div expr-l expr-r)
  `(cons "/" (cons ,expr-l ,expr-r)))


;; An example program
(define program
  (add (prod "x" 1 4 (mul "k" "x"))
       (fix "x" 16 (add (div "x" 2) "k"))))


;; Program interpreter
(define lookup
  `(lambda lookup
     (variable)
     (lambda recur
       (environ)
       (if (null? environ)
           void ; i.e., error
           (if (equal? variable (car (car environ)))
               (cdr (car environ))
               (recur (cdr environ)))))))

(define interpret
  `(let (lookup ,lookup)
     (lambda interpret
       (program)
       (lambda _
         (environ)
         ,(kond
           `[(integer? program)
             program]
           `[(string? program)
             ((lookup program) environ)]
           `[(cons? program)
             (let (op (car program))
               ,(kond
                 `[(equal? op "+")
                   (let (e1 (car (cdr program)))
                     (let (e2 (cdr (cdr program)))
                       (+ ((interpret e1) environ)
                          ((interpret e2) environ))))]
                 `[(equal? op "-")
                   (let (e1 (car (cdr program)))
                     (let (e2 (cdr (cdr program)))
                       (- ((interpret e1) environ)
                          ((interpret e2) environ))))]
                 `[(equal? op "*")
                   (let (e1 (car (cdr program)))
                     (let (e2 (cdr (cdr program)))
                       (* ((interpret e1) environ)
                          ((interpret e2) environ))))]
                 `[(equal? op "/")
                   (let (e1 (car (cdr program)))
                     (let (e2 (cdr (cdr program)))
                       (/ ((interpret e1) environ)
                          ((interpret e2) environ))))]
                 `[(equal? op "sum")
                   ,(lets `([v (car (car (car (cdr program))))]
                            [ei (cdr (car (car (cdr program))))]
                            [ej (cdr (car (cdr program)))]
                            [i ((interpret ei) environ)]
                            [j ((interpret ej) environ)]
                            [e (cdr (cdr program))]
                            [f (lambda _
                                 (k)
                                 ((interpret e) (cons (cons v k) environ)))])
                          `((lambda sum
                              (k) (if (<= k j) (+ (f k) (sum (+ k 1))) 1))
                            i))]
                 `[(equal? op "prod")
                   ,(lets `([v (car (car (car (cdr program))))]
                            [ei (cdr (car (car (cdr program))))]
                            [ej (cdr (car (cdr program)))]
                            [i ((interpret ei) environ)]
                            [j ((interpret ej) environ)]
                            [e (cdr (cdr program))]
                            [f (lambda _
                                 (k)
                                 ((interpret e) (cons (cons v k) environ)))])
                          `((lambda prod
                              (k) (if (<= k j) (* (f k) (prod (+ k 1))) 1))
                            i))]
                 `[(equal? op "fix")
                   ,(lets `([v (car (car (cdr program)))]
                            [e0 (cdr (car (cdr program)))]
                            [x0 ((interpret e0) environ)]
                            [e (cdr (cdr program))]
                            [f (lambda _
                                 (x)
                                 ((interpret e) (cons (cons v x) environ)))])
                          `((lambda fix
                              (x) (if (= (f x) x) x (fix (f x))))
                            x0))]))])))))
