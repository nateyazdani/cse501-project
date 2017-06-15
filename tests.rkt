#lang racket

(require rackunit
         rackunit/text-ui
         (prefix-in gen: "generator.rkt"))

(define rename-tests
  (test-suite
   "Test variable renaming"

   (check-equal? (gen:rename `x) `x "Renaming preserves free variable")

   (check-equal? (gen:rename `void) `void "Renaming preserves void")

   (check-equal? (gen:rename `null) `null "Renaming preserves null")

   (check-equal? (gen:rename `true) `true "Renaming preserves true")

   (check-equal? (gen:rename `false) `false "Renaming preserves false")

   (check-equal? (gen:rename 16) 16 "Renaming preserves integer")

   (check-equal? (gen:rename "test") "test" "Renaming preserves string")

   (check-equal? (gen:rename `(lambda f (x) x))
                 `(lambda f_0 (x_0) x_0)
                 "Renaming lambda")

   (check-equal? (gen:rename `(lambda f (x) (cons (f x) (lambda f (x) (f x)))))
                 `(lambda f_0 (x_0) (cons (f_0 x_0) (lambda f_1 (x_1) (f_1 x_1))))
                 "Renaming nested lambda")

   (check-equal? (gen:rename `(let (x y) (cons x y)))
                 `(let (x_0 y) (cons x_0 y))
                 "Renaming let")

   (check-equal? (gen:rename `(let (x y) (cons (let (x x) x) x)))
                 `(let (x_0 y) (cons (let (x_1 x_0) x_1) x_0))
                 "Renaming nested let")

   (check-equal? (gen:rename `(box (let (x 8) x)))
                 `(box (let (x_0 8) x_0))
                 "Renaming preserves box")

   (check-equal? (gen:rename `(set-box! (box (let (x 7) x))
                                        (let (x 8) (cons x x))
                                        (let (x null) x)))
                 `(set-box! (box (let (x_0 7) x_0))
                            (let (x_0 8) (cons x_0 x_0))
                            (let (x_0 null) x_0))
                 "Renaming preserves set-box!")

   (check-equal? (gen:rename `(unbox (let (x 8) (box x))))
                 `(unbox (let (x_0 8) (box x_0)))
                 "Renaming preserves unbox")

   (check-equal? (gen:rename `(cons (let (x 8) (box x)) (let (x null) x)))
                 `(cons (let (x_0 8) (box x_0)) (let (x_0 null) x_0))
                 "Renaming preserves cons")

   (check-equal? (gen:rename `(car (let (x void) x)))
                 `(car (let (x_0 void) x_0))
                 "Renaming preserves car")

   (check-equal? (gen:rename `(cdr (let (x void) x)))
                 `(cdr (let (x_0 void) x_0))
                 "Renaming preserves cdr")

   (check-equal? (gen:rename `(let (x void) (+ (let (x y) x) (let (x 8) x))))
                 `(let (x_0 void) (+ (let (x_1 y) x_1) (let (x_1 8) x_1)))
                 "Renaming preserves addition")

   (check-equal? (gen:rename `(let (x void) (- (let (x y) x) (let (x 8) x))))
                 `(let (x_0 void) (- (let (x_1 y) x_1) (let (x_1 8) x_1)))
                 "Renaming preserves subtraction")

   (check-equal? (gen:rename `(let (x void) (* (let (x y) x) (let (x 8) x))))
                 `(let (x_0 void) (* (let (x_1 y) x_1) (let (x_1 8) x_1)))
                 "Renaming preserves multiplication")

   (check-equal? (gen:rename `(let (x void) (/ (let (x y) x) (let (x 8) x))))
                 `(let (x_0 void) (/ (let (x_1 y) x_1) (let (x_1 8) x_1)))
                 "Renaming preserves division")

   (check-equal? (gen:rename `(let (x void) (if (let (x z) x)
                                            (let (x y) x)
                                            (let (x 8) x))))
                 `(let (x_0 void) (if (let (x_1 z) x_1)
                                      (let (x_1 y) x_1)
                                      (let (x_1 8) x_1)))
                 "Renaming preserves if-then-else")

   (check-equal? (gen:rename `(void? (let (x void) x)))
                 `(void? (let (x_0 void) x_0))
                 "Renaming preserves void?")

   (check-equal? (gen:rename `(null? (let (x void) x)))
                 `(null? (let (x_0 void) x_0))
                 "Renaming preserves null?")

   (check-equal? (gen:rename `(boolean? (let (x void) x)))
                 `(boolean? (let (x_0 void) x_0))
                 "Renaming preserves boolean?")

   (check-equal? (gen:rename `(integer? (let (x void) x)))
                 `(integer? (let (x_0 void) x_0))
                 "Renaming preserves integer?")

   (check-equal? (gen:rename `(string? (let (x void) x)))
                 `(string? (let (x_0 void) x_0))
                 "Renaming preserves string?")

   (check-equal? (gen:rename `(cons? (let (x void) x)))
                 `(cons? (let (x_0 void) x_0))
                 "Renaming preserves cons?")

   (check-equal? (gen:rename `(lambda? (let (x void) x)))
                 `(lambda? (let (x_0 void) x_0))
                 "Renaming preserves lambda?")

   (check-equal? (gen:rename `(let (x void) ((let (x y) x) (let (x x) x))))
                 `(let (x_0 void) ((let (x_1 y) x_1) (let (x_1 x_0) x_1)))
                 "Renaming preserves application")))

(define evaluate-tests
  (let ([evaluate (compose gen:evaluate gen:rename)])
  (test-suite
   "Test partial evaluation"

   (check-equal? (evaluate `x) `x "Evaluation preserves free variable")

   (check-equal? (evaluate `void) `void "Evaluation preserves void")

   (check-equal? (evaluate `null) `null "Evaluation preserves null")

   (check-equal? (evaluate `true) `true "Evaluation preserves true")

   (check-equal? (evaluate `false) `false "Evaluation preserves false")

   (check-equal? (evaluate 16) 16 "Evaluation preserves integer")

   (check-equal? (evaluate "test") "test" "Evaluation preserves string")

   (check-equal? (evaluate `(lambda f (x) (+ (+ 8 2) x)))
                 `(lambda f_0 (x_0) (+ 10 x_0))
                 "Evaluation of addition under a lambda")

   (check-equal? (evaluate `(let (x (+ 8 0)) (cons x y)))
                 `(cons 8 y)
                 "Evaluation of let-bound value")

   (check-equal? (evaluate `(let (x (box 8)) (cons x y)))
                 `(let (x_0 (box 8)) (cons x_0 y))
                 "Evaluation of let-bound irreducible term")

   (check-equal? (evaluate `(box (let (x 8) x)))
                 `(box 8)
                 "Evaluation preserves box")

   (check-equal? (evaluate `(set-box! (box (let (x 7) x))
                                        (let (x 8) (cons x x))
                                        (let (x null) x)))
                 `(set-box! (box 7) (cons 8 8) null)
                 "Evaluation preserves set-box!")

   (check-equal? (evaluate `(unbox (let (x 8) (box x))))
                 `(unbox (box 8))
                 "Evaluation preserves unbox")

   (check-equal? (evaluate `(cons (let (x 8) (box x)) (let (x null) x)))
                 `(cons (box 8) null)
                 "Evaluation preserves cons")

   (check-equal? (evaluate `(car (let (x (cons 1 2)) x)))
                 `1
                 "Evaluation computes car of value")

   (check-equal? (evaluate `(car (cons (f x) 0)))
                 `(car (cons (f x) 0))
                 "Evaluation preserves car of irreducible term")

   (check-equal? (evaluate `(cdr (let (x (cons 1 2)) x)))
                 `2
                 "Evaluation computes cdr of value")

   (check-equal? (evaluate `(cdr (cons (f x) 0)))
                 `(cdr (cons (f x) 0))
                 "Evaluation preserves cdr of irreducible term")

   (check-equal? (evaluate `(let (x void) (+ (let (x 7) x) (let (x 8) x))))
                 `15
                 "Evaluation computes addition of values")

   (check-equal? (evaluate `(let (x void) (+ (let (x 7) x) (let (x y) x))))
                 `(+ 7 y)
                 "Evaluation preserves addition with an irreducible term")

   (check-equal? (evaluate `(let (x void) (- (let (x 7) x) (let (x 8) x))))
                 `-1
                 "Evaluation computes addition of values")

   (check-equal? (evaluate `(let (x void) (- (let (x 7) x) (let (x y) x))))
                 `(- 7 y)
                 "Evaluation preserves addition with an irreducible term")

   (check-equal? (evaluate `(let (x void) (* (let (x 7) x) (let (x 8) x))))
                 `56
                 "Evaluation computes addition of values")

   (check-equal? (evaluate `(let (x void) (* (let (x 7) x) (let (x y) x))))
                 `(* 7 y)
                 "Evaluation preserves addition with an irreducible term")

   (check-equal? (evaluate `(let (x void) (/ (let (x 7) x) (let (x 8) x))))
                 `0
                 "Evaluation computes addition of values")

   (check-equal? (evaluate `(let (x void) (/ (let (x 7) x) (let (x y) x))))
                 `(/ 7 y)
                 "Evaluation preserves addition with an irreducible term")

   (check-equal? (evaluate `(let (x void) (if (let (x z) x)
                                              (let (x y) x)
                                              (let (x 8) x))))
                 `(if z y 8)
                 "Evaluation preserves indeterminate if-then-else")

   (check-equal? (evaluate `(void? (let (x void) x)))
                 `true
                 "Evaluation computes void? of void")

   (check-equal? (evaluate `(void? (lambda f (x) (let (x void) x))))
                 `false
                 "Evaluation computes void? of non-void value")

   (check-equal? (evaluate `(void? (let (x y) x)))
                 `(void? y)
                 "Evaluation preserves void? of irreducible term")

   (check-equal? (evaluate `(null? (let (x null) x)))
                 `true
                 "Evaluation computes null? of null")

   (check-equal? (evaluate `(null? (lambda f (x) (let (x null) x))))
                 `false
                 "Evaluation computes null? of non-null value")

   (check-equal? (evaluate `(null? (let (x y) x)))
                 `(null? y)
                 "Evaluation preserves null? of irreducible term")

   (check-equal? (evaluate `(boolean? (let (x true) x)))
                 `true
                 "Evaluation computes boolean? of true")

   (check-equal? (evaluate `(boolean? (let (x false) x)))
                 `true
                 "Evaluation computes boolean? of false")

   (check-equal? (evaluate `(boolean? (lambda f (x) (let (x boolean) x))))
                 `false
                 "Evaluation computes boolean? of non-boolean value")

   (check-equal? (evaluate `(boolean? (let (x y) x)))
                 `(boolean? y)
                 "Evaluation preserves boolean? of irreducible term")

   (check-equal? (evaluate `(integer? (let (x 64) x)))
                 `true
                 "Evaluation computes integer? of an integer")

   (check-equal? (evaluate `(integer? (lambda f (x) (let (x integer) x))))
                 `false
                 "Evaluation computes integer? of non-integer value")

   (check-equal? (evaluate `(integer? (let (x y) x)))
                 `(integer? y)
                 "Evaluation preserves integer? of irreducible term")

   (check-equal? (evaluate `(string? (let (x "hello world") x)))
                 `true
                 "Evaluation computes string? of a string")

   (check-equal? (evaluate `(string? (lambda f (x) (let (x string) x))))
                 `false
                 "Evaluation computes string? of non-string value")

   (check-equal? (evaluate `(string? (let (x y) x)))
                 `(string? y)
                 "Evaluation preserves string? of irreducible term")

   (check-equal? (evaluate `(lambda? (let (x (lambda f (y) y)) x)))
                 `true
                 "Evaluation computes lambda? of lambda")

   (check-equal? (evaluate `(lambda? null))
                 `false
                 "Evaluation computes lambda? of non-lambda value")

   (check-equal? (evaluate `(lambda? (let (x y) x)))
                 `(lambda? y)
                 "Evaluation preserves lambda? of irreducible term")

   (check-equal? (evaluate `(lambda f (x) (cons (f x) ((lambda f (x) x) 10))))
                 `(lambda f_0 (x_0) (cons (f_0 x_0) 10))
                 "Evaluation of recursive and non-recursive applications under lambda")

   (check-equal? (evaluate `(let (x (box 0)) ((lambda f (x) x) (lambda f (y) x))))
                 `(let (x_0 (box 0)) (lambda f_0 (y_0) x_0))
                 "Evaluation of application under let-binding")

   (check-equal? (evaluate `((lambda f (x) x) (let (x (box 0)) (lambda f (y) x))))
                 `(let (x_0 (box 0)) (lambda f_0 (y_0) x_0))
                 "Evaluation of application across let-binding"))))

(define translate-tests
  (test-suite
   "Test syntactic translation"

   (check-equal? (gen:translate `void)
                 "Void"
                 "Translate void")

   (check-equal? (gen:translate `null)
                 "Null"
                 "Translate null")

   (check-equal? (gen:translate `true)
                 "Boolean true"
                 "Translate true")

   (check-equal? (gen:translate `false)
                 "Boolean false"
                 "Translate false")

   (check-equal? (gen:translate `64)
                 "Integer 64"
                 "Translate integer")

   (check-equal? (gen:translate `"hello world")
                 "String \"hello world\""
                 "Translate string")

   (check-equal? (gen:translate `x)
                 "x"
                 "Translate free variable")

   (check-equal? (gen:translate `(lambda f (x) (+ x x)))
                 "let rec f = Lambda (fun x -> __add (x) (x)) in f"
                 "Translate non-recursive lambda")

   (check-equal? (gen:translate `(lambda f (x) (if x 1 (f 3))))
                 "let rec f = Lambda (fun x -> if __truth (x) then (Integer 1) else (__app (f) (Integer 3))) in f"
                 "Translate recursive lambda")

   (check-equal? (gen:translate `(let (x 7) x))
                 "let x = Integer 7 in x"
                 "Translate let-binding")

   (check-equal? (gen:translate `(box 9))
                 "ref (Integer 9)"
                 "Translate box")

   (check-equal? (gen:translate `(set-box! b 9 0))
                 "b := Integer 9; Integer 0"
                 "Translate mutation")

   (check-equal? (gen:translate `(unbox b))
                 "!(b)"
                 "Translate unbox")

   (check-equal? (gen:translate `(cons 1 2))
                 "Cons (Integer 1, Integer 2)"
                 "Translate cons")

   (check-equal? (gen:translate `(car p))
                 "car (p)"
                 "Translate car")

   (check-equal? (gen:translate `(cdr p))
                 "cdr (p)"
                 "Translate cdr")

   (check-equal? (gen:translate `(+ x y))
                 "__add (x) (y)"
                 "Translate addition")

   (check-equal? (gen:translate `(- x y))
                 "__sub (x) (y)"
                 "Translate subtraction")

   (check-equal? (gen:translate `(* x y))
                 "__mul (x) (y)"
                 "Translate multiplication")

   (check-equal? (gen:translate `(/ x y))
                 "__div (x) (y)"
                 "Translate division")

   (check-equal? (gen:translate `(if x y z))
                 "if __truth (x) then (y) else (z)"
                 "Translate division")

   (check-equal? (gen:translate `(void? v))
                 "__is_void (v)"
                 "Translate void?")

   (check-equal? (gen:translate `(null? v))
                 "__is_null (v)"
                 "Translate null?")

   (check-equal? (gen:translate `(boolean? v))
                 "__is_boolean (v)"
                 "Translate boolean?")

   (check-equal? (gen:translate `(integer? v))
                 "__is_integer (v)"
                 "Translate integer?")

   (check-equal? (gen:translate `(string? v))
                 "__is_string (v)"
                 "Translate string?")

   (check-equal? (gen:translate `(string? v))
                 "__is_string (v)"
                 "Translate string?")

   (check-equal? (gen:translate `(cons? v))
                 "__is_cons (v)"
                 "Translate cons?")

   (check-equal? (gen:translate `(lambda? v))
                 "__is_lambda (v)"
                 "Translate lambda?")

   (check-equal? (gen:translate `(equal? x y))
                 "(x) = (y)"
                 "Translate equal?")

   (check-equal? (gen:translate `(begin (f 0) (g 1) (h 2)))
                 "(__app (f) (Integer 0); __app (g) (Integer 1); __app (h) (Integer 2))"
                 "Translate begin")

   (check-equal? (gen:translate `(f x))
                 "__app (f) (x)"
                 "Translate application")))

(run-tests rename-tests)
(run-tests evaluate-tests)
(run-tests translate-tests)
