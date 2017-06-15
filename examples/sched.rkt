#lang racket

(require "../generator.rkt")

;; Syntax for a domain-specific language of simple tree traversal schedules.
;
; schedule ::= (cons "seq" (cons <traversal> <traversal))
;            | (cons "par" (cons <traversal> <traversal>))
;
; traversal ::= (cons "pre" <statement>*)
;             | (cons "post" <statement>*)
;
; statement ::= (cons (cons <string> <string>) <expression>)
;
; expression ::= (cons "+" (cons <expression> <expression))
;              | (cons "-" (cons <expression> <expression))
;              | (cons "*" (cons <expression> <expression))
;              | (cons "/" (cons <expression> <expression))
;              | (cons "var" (cons <string> <string>))
;              | (cons "int" <integer>)


;; Commands
(define (get-residual)
  (evaluate (rename `(,interpret ,schedule))))

(define (get-output)
  (translate (get-residual)))


;; Macros
(define (lyst . xs)
  (foldl (lambda (x xs) `(cons ,x ,xs)) `null xs))

(define (kond . branches)
  (let* ([branch (first branches)]
         [bool (first branch)]
         [expr (second branch)])
    (if (equal? bool 'else)
        expr
        `(if ,bool
             ,expr
             ,(apply kond (rest branches))))))


;; An example tree traversal schedule in the above syntax.
(define statement1
  `(cons (cons "left" "x")
         (cons "+" (cons (cons "int" 16)
                         (cons "var" (cons "self" "x"))))))

(define statement2
  `(cons (cons "right" "x")
         (cons "+" (cons (cons "int" 16)
                         (cons "var" (cons "self" "x"))))))

(define statement3
  `(cons (cons "self" "y")
         (cons "+" (cons (cons "var" (cons "left" "y"))
                         (cons "var" (cons "right" "y"))))))

(define traversal1
  `(cons "pre" ,(lyst statement1 statement2)))

(define traversal2
  `(cons "post" ,(lyst statement3)))

(define schedule
  `(cons "seq" (cons ,traversal1 ,traversal2)))


;; Schedule interpreter
(define compute
  `(lambda compute
     (expression)
     (lambda _
       (node)
       (let (op (car expression))
         ,(kond
           `[(equal? op "+")
             (let (left (car (cdr expression)))
               (let (right (cdr (cdr expression)))
                 (+ ((compute left) node) ((compute right) node))))]
           `[(equal? op "-")
             (let (left (car (cdr expression)))
               (let (right (cdr (cdr expression)))
                 (- ((compute left) node) ((compute right) node))))]
           `[(equal? op "*")
             (let (left (car (cdr expression)))
               (let (right (cdr (cdr expression)))
                 (* ((compute left) node) ((compute right) node))))]
           `[(equal? op "/")
             (let (left (car (cdr expression)))
               (let (right (cdr (cdr expression)))
                 (/ ((compute left) node) ((compute right) node))))]
           `[(equal? op "-") (cdr expression)]
           `[(equal? op "var") ((lookup node) (cdr expression))]
           `[(equal? op "int") (cdr expression)]
           `[else (cdr expression)])))))

(define visitor
  `(lambda visitor
     (statements)
     (lambda _
       (node)
       (if (null? statements)
           void
           (let (statement (car statements))
             (let (attribute (car statement))
               (let (expression (cdr statement))
                 (let (value ((compute expression) tree))
                   (begin
                     (((assign node) attribute) value)
                     ((visitor (cdr statements)) node))))))))))

(define interpret
  `(let (compute ,compute)
     (let (visitor ,visitor)
       (lambda interpret
         (sched)
         (let (op (car sched))
           ,(kond
             `[(equal? op "pre")
               ((preorder (visitor (cdr sched))) tree)]
             `[(equal? op "post")
               ((postorder (visitor (cdr sched))) tree)]
             `[(equal? op "par")
               (let (left (car (cdr sched)))
                 (let (right (cdr (cdr sched)))
                   (begin ; parallel
                     (spawn (lambda _ (unit) (interpret left)))
                     (spawn (lambda _ (unit) (interpret right)))
                     (join void))))]
             `[else
               (let (left (car (cdr sched)))
                 (let (right (cdr (cdr sched)))
                   (begin ; sequential
                     (interpret left)
                     (interpret right))))]))))))
