
type tree = {
  mutable x: int;
  mutable y: int;
  left: tree option;
  right: tree option;
};;

let map_default (f: 'a -> 'b) (x: 'a option) (z: 'b) =
  match x with
  | None -> z
  | Some x' -> f x';;

let do_lookup (node: tree) (child: string) (label: string)=
  let get node =
    match label with
    | "x" -> node.x
    | "y" -> node.y
    | _ -> failwith "invalid attribute label"
  in
  match child with
  | "self" -> get node
  | "left" -> map_default get node.left 0
  | "right" -> map_default get node.right 0
  | _ -> failwith "invalid attribute child";;

let do_assign (node: tree) (child: string) (label: string) (value: int) =
  (let set node =
     match label with
     | "x" -> node.x <- value
     | "y" -> node.y <- value
     | _ -> failwith "invalid attribute label"
   in
   match child with
   | "self" -> set node
   | "left" -> map_default set node.left ()
   | "right" -> map_default set node.right ()
   | _ -> failwith "invalid attribute child");;

let rec do_preorder (f: tree -> unit) (tree: tree) : unit =
  f tree;
  map_default (do_preorder f) tree.left ();
  map_default (do_preorder f) tree.right ();;

let rec do_postorder (f: tree -> unit) (tree: tree) : unit =
  map_default (do_postorder f) tree.left ();
  map_default (do_postorder f) tree.right ();
  f tree;;

let assign : 'a dyn =
  Lambda (fun node ->
      Lambda (fun attr ->
          Lambda (fun value ->
              do_assign
                (extern_of_dyn node)
                (string_of_dyn (car attr)) (string_of_dyn (cdr attr))
                (int_of_dyn value);
              Void)));;

let lookup : 'a dyn =
  Lambda (fun node ->
      Lambda (fun attr ->
          Integer (do_lookup
                     (extern_of_dyn node)
                     (string_of_dyn (car attr))
                     (string_of_dyn (cdr attr)))));;

let preorder : 'a dyn =
  Lambda (fun f ->
      Lambda (fun tree ->
          let f' = lambda_of_dyn f in
          do_preorder (fun t -> void_of_dyn (f' (Extern t))) (extern_of_dyn tree);
          Void));;

let postorder : 'a dyn =
  Lambda (fun f ->
      Lambda (fun tree ->
          let f' = lambda_of_dyn f in
          do_postorder (fun t -> void_of_dyn (f' (Extern t))) (extern_of_dyn tree);
          Void));;
