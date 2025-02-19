open Base
open Util

type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree [@@deriving show]

(*
type 'a tree =
|Leaf
|Node of 'a * 'a tree * 'a tree;;
type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree
*)

let rec equal_tree (equal : 'a -> 'a -> bool) (t1 : 'a tree) (t2 : 'a tree) : bool =
  match t1, t2 with
  | Leaf, Leaf -> true
  | Node (v1, l1, r1), Node (v2, l2, r2) -> if (equal v1 v2) then ((equal_tree equal l1 l2) && (equal_tree equal r1 r2)) else false
  | _, _ -> false;;

let rec timestamp_helper (next_stamp : int) (tree_in : 'a tree) : (((int*'a) tree)*int) = 
  match tree_in with
  | Leaf -> (Leaf,next_stamp)
  | Node (value, left, right) -> let (left_branch, left_stamp) = (timestamp_helper (next_stamp+1) left) in
                                  let (right_branch, right_stamp) = (timestamp_helper left_stamp right) in
                                  ( (Node((next_stamp, value), left_branch, right_branch)), right_stamp);;

let timestamp (t : 'a tree) : (int * 'a) tree =
  let (return_tree, junk_stamp) = timestamp_helper 0 t in return_tree;; 


