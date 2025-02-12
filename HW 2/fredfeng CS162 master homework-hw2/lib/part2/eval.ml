open Ast

let todo () = failwith "TODO"
let bonus () = failwith "BONUS"

exception Stuck of string
(** Exception indicating that evaluation is stuck *)

(** Raises an exception indicating that evaluation got stuck. *)
let im_stuck msg = raise (Stuck msg)

(* let free_vars_binder_helper (e: Lambda) : Vars.t = 
  let Vars.t expr_vars = (free_vars (snd e)) in
    if(mem (fst e) expr_vars) then (diff expr_vars (singleton e)) else expr_vars;; *)

(** Computes the set of free variables in the given expression *)
let rec free_vars (e : expr) : Vars.t =
  (* This line imports the functions in Vars, so you can write [diff .. ..]
     instead of [Vars.diff .. ..] *)
  let open Vars in
  (* Your code goes here *)
  match e with
  | Num _ -> empty
  | Binop (_, e1, e2) -> (union (free_vars e1) (free_vars e2))
  | Var x -> (singleton x)
  | Lambda binder -> (diff (free_vars (snd binder)) (singleton (fst binder)))
  | App (e1, e2) -> (union (free_vars e1) (free_vars e2))
  | Let (e1, binder) -> union (free_vars e1) (diff (free_vars (snd binder)) (singleton (fst binder)))

(** Perform substitution c[x -> e], i.e., substituting x with e in c *)
let rec subst (x : string) (e : expr) (c : expr) : expr =
  match c with
  | Num n -> Num n
  | Binop (op, c1, c2) -> Binop (op, (subst x e c1), (subst x e c2))
  | Var y -> if (String.equal y x) then (e) else (Var y)
  | Lambda binder -> let y = (fst binder) in (if ((Bool.not (String.equal x y)) && (Bool.not (Vars.mem y (free_vars e)))) then Lambda (y, (subst x e (snd binder))) else Lambda binder)
  | App (c1, c2) -> App((subst x e c1), (subst x e c2))
  | Let (c1, binder) -> Let (subst x e c1, (let y = (fst binder) in (if ((Bool.not (String.equal x y)) && (Bool.not (Vars.mem y (free_vars e)))) then (y, (subst x e (snd binder))) else binder)))

(** Evaluate expression e *)
let rec eval (e : expr) : expr =
  try
    match e with
    | Num n -> Num n
    | Binop (op, e1, e2) -> let expr1 = (eval e1) in (let expr2 = (eval e2) in (match (expr1, expr2) with 
                                                                                | (Num x, Num y) -> (match op with
                                                                                                |Add -> (Num (x + y))
                                                                                                |Sub -> (Num (x - y))
                                                                                                |Mul -> (Num (x * y)))
                                                                                | (_, _) -> im_stuck (Fmt.str "Invalid binop: %a" Pretty.expr e)))
    | Var x -> im_stuck (Fmt.str "Unassigned: %a" Pretty.expr e)
    | Lambda binder -> (Lambda binder)
    | App (e1, e2) -> let expr1 = (eval e1) in (let expr2 = (eval e2) in (match expr1 with
                                                                          | Lambda x -> (eval (subst (fst x) expr2 (snd x)))
                                                                          | _ -> im_stuck (Fmt.str "Application to non lambda: %a" Pretty.expr e)))
    | Let (e1, (x, e2)) -> let v1 = (eval e1) in (eval (subst x v1 e2))
    | _ -> im_stuck (Fmt.str "Ill-formed expression: %a" Pretty.expr e)
  with Stuck msg ->
    im_stuck (Fmt.str "%s\nin expression %a" msg Pretty.expr e)

type sigma = (string * expr) list
(** Substitution  *)

(** Perform simultaneous substitution c[sigma], i.e., substituting variables in c according to sigma *)
let rec subst_multi (sigma : sigma) (c : expr) : expr = bonus ()

(** Alpha-equivalence *)
let alpha_equiv (e1 : expr) (e2 : expr) : bool = bonus ()
