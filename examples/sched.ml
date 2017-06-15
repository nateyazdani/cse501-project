
let evaluate tree =
  (app (app (preorder)
            (let rec __1 = Lambda (fun node_1 ->
                 let value_1 = add
                     (Integer 16)
                     (app (app (lookup) (node_1))
                        (Cons (String "self", String "x")))
                 in
                 (app (app (app (assign) (node_1))
                    (Cons (String "right", String "x"))) (value_1);
                 let value_1 = add (Integer 16)
                                 (app (app (lookup) (node_1))
                                    (Cons (String "self", String "x")))
                 in
                 (app (app (app (assign) (node_1))
                    (Cons (String "left", String "x"))) (value_1); Void)))
             in __1))
     (tree);
   app (app (postorder)
            (let rec __1 = Lambda (fun node_1 ->
                 let value_1 = add (app (app (lookup) (node_1))
                                        (Cons (String "left", String "y")))
                                 (app (app (lookup) (node_1))
                                    (Cons (String "right", String "y")))
                 in
                 (app (app (app (assign) (node_1))
                    (Cons (String "self", String "y"))) (value_1); Void))
             in __1))
     (tree))
