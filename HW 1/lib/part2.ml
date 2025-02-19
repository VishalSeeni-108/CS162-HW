open Base
open Util







let rec compress (equal : 'a -> 'a -> bool) (xs : 'a list) : 'a list = 
match xs with
    [] -> []
    | (h::t) -> (match t with 
                [] -> h::[]
                | (head::tail) -> if(equal h head) then (compress equal t) else h::(compress equal t));;           
  
let rec max_helper (curr_max : int) (list : int list) : int = 
  match list with
  [] -> curr_max
  | (h::t) -> if(h > curr_max) then (max_helper h t)
              else (max_helper curr_max t);;
  
let max (xs : int list) : int option =
    match xs with 
    [] -> None
    | (h::t) -> (Some (max_helper min_int xs));;
  
let rec join (xs : 'a option list) : 'a list option =
  match xs with
  |[] -> (Some [])
  |(None::t) -> None
  |(Some h::t) -> 
    (match (join t) with
      |None -> None
      |Some x -> Some (h::x));;

let insert (key : 'k) (value : 'v) (dict : ('k * 'v) list) : ('k * 'v) list =
  (key, value) :: dict
  
let rec lookup (equal : 'k -> 'k -> bool) (key : 'k) (dict : ('k * 'v) list) : 'v option =
match dict with
|[] -> None (*End of dictionary reached*)
|(h::t) -> let (curr_key, curr_value) = h in 
          if (equal curr_key key) then (Some curr_value)
          else (lookup equal key t);;