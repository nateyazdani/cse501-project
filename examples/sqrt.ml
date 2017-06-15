
let int_sqrt i =
  truncate (sqrt (float i));;

let square_root (d: 'a dyn) =
  Integer (int_sqrt (integer_of_dyn d));;
