#!/bin/bash

utop <<EOF
#use "runtime.ml";;
#use "tree.ml";;
#use "sched.ml";;
let tree = { x=64; y=32; left=Some { x=86; y=128; left=None; right=None }; right=Some { x=4; y=8; left=None; right=None} };;
evaluate (Extern tree);;
tree;;
EOF
