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

let int_of_dyn d =
  match d with
  | Integer i -> i
  | _ -> failwith "expected integer";;

let bool_of_dyn d =
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

let is_lambda d =
  match d with
  | Lambda _ -> Boolean true
  | _ -> Boolean false;;

let is_bool d =
  match d with
  | Boolean _ -> Boolean true
  | _ -> Boolean false;;

let is_int d =
  match d with
  | Integer _ -> Boolean true
  | _ -> Boolean false;;

let is_string d =
  match d with
  | String _ -> Boolean true
  | _ -> Boolean false;;

let is_cons d =
  match d with
  | Cons _ -> Boolean true
  | _ -> Boolean false;;

let is_null d =
  match d with
  | Null -> Boolean true
  | _ -> Boolean false;;

let is_void d =
  match d with
  | Void -> Boolean true
  | _ -> Boolean false;;

let app f x =
  lambda_of_dyn(f) x;;

let cons x y =
  Cons (x, y);;

let car d =
  match d with
  | Cons (x, _) -> x
  | _ -> failwith "car of non-pair";;

let cdr d =
  match d with
  | Cons (_, y) -> y
  | _ -> failwith "cdr of non-pair";;

let truth d =
  match d with
  | Boolean false -> false
  | _ -> true;;

let add x y =
  int_of_dyn(x) + int_of_dyn(y);;

let sub x y =
  int_of_dyn(x) - int_of_dyn(y);;

let mul x y =
  int_of_dyn(x) * int_of_dyn(y);;

let div x y =
  int_of_dyn(x) / int_of_dyn(y);;

let lt x y =
  int_of_dyn(x) < int_of_dyn(y);;

let le x y =
  int_of_dyn(x) <= int_of_dyn(y);;

let eq x y =
  int_of_dyn(x) = int_of_dyn(y);;

let ge x y =
  int_of_dyn(x) >= int_of_dyn(y);;

let gt x y =
  int_of_dyn(x) > int_of_dyn(y);;

