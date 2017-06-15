(* Runtime library for generated code. *)

type 'a dyn = Lambda of ('a dyn -> 'a dyn)
            | Integer of int
            | Boolean of bool
            | String of string
            | Cons of 'a dyn * 'a dyn
            | Null
            | Void
            | Extern of 'a;;

let lambda_of_dyn d =
  match d with
  | Lambda f -> f
  | _ -> failwith "expected lambda";;

let integer_of_dyn d =
  match d with
  | Integer i -> i
  | _ -> failwith "expected integer";;

let boolean_of_dyn d =
  match d with
  | Boolean b -> b
  | _ -> failwith "expected boolean";;

let string_of_dyn d =
  match d with
  | String s -> s
  | _ -> failwith "expected string";;

let pair_of_dyn d =
  match d with
  | Cons (x, y) -> (x, y)
  | _ -> failwith "expected pair";;

let null_of_dyn d =
  match d with
  | Null -> ()
  | _ -> failwith "expected null";;

let void_of_dyn d =
  match d with
  | Void -> ()
  | _ -> failwith "expected void";;

let extern_of_dyn d =
  match d with
  | Extern v -> v
  | _ -> failwith "expected external data";;

let app f x =
    match f with
    | Lambda f -> f x
    | _ -> failwith "application of non-lambda";;

let is_lambda d =
    match d with
    | Lambda _ -> Boolean true
    | _ -> Boolean false;;

let car d =
    match d with
    | Cons (x, _) -> x
    | _ -> failwith "car of non-pair";;

let cdr d =
    match d with
    | Cons (_, y) -> y
    | _ -> failwith "cdr of non-pair";;

let is_pair d =
    match d with
    | Cons _ -> Boolean true
    | _ -> Boolean false;;

let is_null d =
    match d with
    | Null -> Boolean true
    | _ -> Boolean false;;

let is_integer d =
    match d with
    | Integer _ -> Boolean true
    | _ -> Boolean false;;

let is_string d =
    match d with
    | String _ -> Boolean true
    | _ -> Boolean false;;

let is_void d =
    match d with
    | Void -> Boolean true
    | _ -> Boolean false;;

let truth d =
    match d with
    | Boolean false -> false
    | _ -> true;;

let is_boolean d =
    match d with
    | Boolean _ -> Boolean true
    | _ -> Boolean false;;

let add x y =
    match x, y with
    | Integer i, Integer j -> Integer (i + j)
    | _ -> failwith "+ of non-integers";;

let sub x y =
    match x, y with
    | Integer i, Integer j -> Integer (i - j)
    | _ -> failwith "- of non-integers";;

let mul x y =
    match x, y with
    | Integer i, Integer j -> Integer (i * j)
    | _ -> failwith "* of non-integers";;

let div x y =
    match x, y with
    | Integer i, Integer j -> Integer (i / j)
    | _ -> failwith "/ of non-integers";;

let lt x y =
    match x, y with
    | Integer i, Integer j -> Boolean (i < j)
    | _ -> failwith "< of non-integers";;

let le x y =
    match x, y with
    | Integer i, Integer j -> Boolean (i <= j)
    | _ -> failwith "<= of non-integers";;

let eq x y =
    match x, y with
    | Integer i, Integer j -> Boolean (i = j)
    | _ -> failwith "= of non-integers";;

let ge x y =
    match x, y with
    | Integer i, Integer j -> Boolean (i >= j)
    | _ -> failwith ">= of non-integers";;

let gt x y =
    match x, y with
    | Integer i, Integer j -> Boolean (i > j)
    | _ -> failwith "> of non-integers";;
