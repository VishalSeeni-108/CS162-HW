open Base
open Util

type expr =
  | Const of int
  | X
  | Add of expr * expr
  | Mul of expr * expr
  | Compose of expr * expr

(* Pretty-printer *)
let rec pp_expr ppf =
  let open Fmt in
  function
  | Const n -> int ppf n
  | X -> string ppf "x"
  | Add (e1, e2) -> pf ppf "@[<hov 2>(%a + %a)@]" pp_expr e1 pp_expr e2
  | Mul (e1, e2) -> pf ppf "@[<hov 2>(%a * %a)@]" pp_expr e1 pp_expr e2
  | Compose (e1, e2) -> pf ppf "@[<hov 2>(%a; %a)@]" pp_expr e1 pp_expr e2

(* Convert an expression into a pretty string *)
let show_expr (e : expr) : string = Fmt.to_to_string pp_expr e
let rec eval_expr (x : int) (e : expr) : int = 
  match e with
  |Const c -> c
  |X -> x
  |Add (a1, a2) -> (eval_expr x a1) + (eval_expr x a2)
  |Mul (m1, m2) -> (eval_expr x m1) * (eval_expr x m2)
  |Compose (e1, e2) -> let result_1 = (eval_expr x e1) in (eval_expr result_1 e2);;


let rec simplify_helper (to_sub: expr) (sub_into : expr) : expr = 
  match sub_into with
  |Const c -> Const c
  |X -> to_sub
  |Add(a1, a2) -> Add((simplify_helper to_sub a1), (simplify_helper to_sub a2))
  |Mul(m1, m2) -> Mul((simplify_helper to_sub m1), (simplify_helper to_sub m2))
  |Compose(e1, e2) -> (simplify_helper e1 e2);;

let rec simplify (e : expr) : expr = 
  let subbed = (simplify_helper X e) in
    match subbed with
    |Const c -> Const c
    |X -> X
    |Add (a1, a2) -> (match simplify(a1), simplify(a2) with
                      |Const c1, Const c2 -> Const (c1+c2)
                      |_, Const c2 -> if c2 == 0 then simplify(a1) else Add(simplify(a1), simplify(a2))
                      |Const c1, _ -> if c1 == 0 then simplify(a2) else Add(simplify(a1), simplify(a2))
                      |_, _ -> Add(simplify(a1), simplify(a2)))
    |Mul (m1, m2) -> (match simplify(m1), simplify(m2) with
                      |Const c1, Const c2 -> Const (c1*c2)
                      |_, Const c2 -> if c2 == 0 then (Const 0) else (if c2 == 1 then simplify(m1) else Mul(simplify(m1), simplify(m2)))
                      |Const c1, _ -> if c1 == 0 then (Const 0) else (if c1 == 1 then simplify(m2) else Mul(simplify(m1), simplify(m2)))
                      |_, _ -> Mul(simplify(m1), simplify(m2)))
    |Compose (e1, e2) -> (simplify (simplify_helper e1 e2));;





type poly = int list [@@deriving show]

let rec eval_poly (x : int) (p : poly) : int = bonus ()
let rec normalize (e : expr) : poly = bonus ()
let semantic_equiv (e1 : expr) (e2 : expr) : bool = bonus ()

