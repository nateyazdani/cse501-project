
let f k =
  add (mul (mul (k) (Integer 1))
           (mul (mul (k) (Integer 2))
              (mul (mul (k) (Integer 3))
                 (mul (mul (k) (Integer 4))
                    (Integer 1)))))
    (let fix_0 =
       let rec fix_0 = Lambda (fun x_0 ->
           if truth (eq (add (div (x_0) (Integer 2)) (k)) (x_0))
           then (x_0)
           else (app (fix_0) (add (div (x_0) (Integer 2)) (k))))
       in fix_0
     in
     if truth (eq (add (Integer 8) (k)) (Integer 16))
     then (Integer 16)
     else (app (fix_0) (add (Integer 8) (k))))
