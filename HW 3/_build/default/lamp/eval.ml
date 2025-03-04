open Ast

let todo () = failwith "TODO"

exception Stuck of string
(** Exception indicating that evaluation is stuck *)

(** Raises an exception indicating that evaluation got stuck. *)
let im_stuck msg = raise (Stuck msg)

(** Computes the set of free variables in the given expression *)
let rec free_vars (e : expr) : Vars.t =
  (* This line imports the functions in Vars, so you can write [diff .. ..]
     instead of [Vars.diff .. ..] *)
  let open Vars in
  match e with   
  | Num _ -> empty
  | Binop (_, e1, e2) -> (union (free_vars e1) (free_vars e2))
  | Var x -> (singleton x)
  | Lambda binder -> (diff (free_vars (snd binder)) (singleton (fst binder)))
  | App (e1, e2) -> (union (free_vars e1) (free_vars e2))
  | Let (e1, binder) -> union (free_vars e1) (diff (free_vars (snd binder)) (singleton (fst binder)))
  | True -> empty
  | False -> empty
  | IfThenElse (e1, e2, e3) -> (union (union (free_vars e1) (free_vars e2)) (free_vars e3))
  | Comp (op, e1, e2) -> (union (free_vars e1) (free_vars e2))
  | ListNil -> empty
  | ListCons (e1, e2) -> (union (free_vars e1) (free_vars e2))
  | ListMatch (e1, e2, (str1, (str2, e3))) -> (union (union (free_vars e1) (free_vars e2)) (diff (diff (free_vars e3) (singleton str2)) (singleton str1)))
  | Fix (name, def) -> (diff (free_vars def) (singleton name))

(** Perform substitution c[x -> e], i.e., substituting x with e in c *)
let rec subst (x : string) (e : expr) (c : expr) : expr =
  match c with 
  | Num n -> Num n
  | Binop (op, c1, c2) -> Binop (op, (subst x e c1), (subst x e c2))
  | Var y -> if (String.equal y x) then (e) else (Var y)
  | Lambda binder -> let y = (fst binder) in (if ((Bool.not (String.equal x y)) && (Bool.not (Vars.mem y (free_vars e)))) then Lambda (y, (subst x e (snd binder))) else Lambda binder)
  | App (c1, c2) -> App((subst x e c1), (subst x e c2))
  | Let (c1, binder) -> Let (subst x e c1, (let y = (fst binder) in (if ((Bool.not (String.equal x y)) && (Bool.not (Vars.mem y (free_vars e)))) then (y, (subst x e (snd binder))) else binder)))
  | True -> True
  | False -> False
  | IfThenElse (e1, e2, e3) -> IfThenElse((subst x e e1), (subst x e e2), (subst x e e3))
  | Comp (op, e1, e2) -> Comp (op, (subst x e e1), (subst x e e2))
  | ListNil -> ListNil
  | ListCons (e1, e2) -> ListCons ((subst x e e1), (subst x e e2))
  | ListMatch (e1, e2, (str1, (str2, e3))) -> if ((Bool.not (String.equal x str1)) && (Bool.not (String.equal x str2))) then ListMatch((subst x e e1), (subst x e e2), (str1, (str2, (subst x e e3)))) else ListMatch((subst x e e1), (subst x e e2), (str1, (str2, e3)))
  | Fix (name, def) -> if (Bool.not (String.equal x name)) then Fix(name, (subst x e def)) else Fix(name, def)

(** Evaluate expression e *)
let rec eval (e : expr) : expr =
  try match e with 
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
  | True -> True
  | False -> False
  | IfThenElse (e1, e2, e3) -> let conditional = (eval e1) in (match conditional with 
                                                               | True -> (eval e2)
                                                               | False -> (eval e3)
                                                               | _ -> im_stuck (Fmt.str "Non-bool in if statement: %a" Pretty.expr e))  
  | Comp (op, e1, e2) -> let arg1 = (eval e1) in (let arg2 = (eval e2) in (match (arg1, arg2) with 
                                                                          |(Num x, Num y) -> (match op with
                                                                                              | Eq -> if x = y then True else False
                                                                                              | Lt -> if x < y then True else False
                                                                                              | Gt -> if x > y then True else False)
                                                                          |(_, _) -> im_stuck (Fmt.str "Non-number operands to comparison: %a" Pretty.expr e)))
  | ListNil -> ListNil
  | ListCons (e1, e2) -> let arg1 = (eval e1) in (let arg2 = (eval e2) in (ListCons(arg1, arg2)))
  | ListMatch (e1, e2, (x, (y, e3))) -> let arg1 = (eval e1) in (match arg1 with
                                                                      |ListNil -> (eval e2)
                                                                      |ListCons(v1, v2) -> (eval (subst x v1 (subst y v2 e3)))
                                                                      |_ -> im_stuck (Fmt.str "List error: %a" Pretty.expr e))
  | Fix (f, e) -> (eval (subst f (Fix(f,e)) e))
  | _ -> im_stuck (Fmt.str "Ill-formed expression: %a" Pretty.expr e)
  with Stuck msg ->
    im_stuck (Fmt.str "%s\nin expression %a" msg Pretty.expr e)
