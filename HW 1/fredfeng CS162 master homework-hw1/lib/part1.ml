open Base
open Util

let rec fib (n : int) : int = 
match n with
  |0 -> 0
  |1 -> 1 
  |x -> (fib (x-1)) + (fib (x-2));;
