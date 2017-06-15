#!/bin/bash

utop <<EOF
#use "runtime.ml";;
#use "arith.ml";;
let x = 16;;
let y = integer_of_dyn (f (Integer x));;
EOF
