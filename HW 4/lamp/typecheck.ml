open Ast
open Base

type env = (string * ty) list
(** Typing environment, aka Gamma *)

(** Helper function to look up a variable in the env *)
let find : env -> string -> ty option = List.Assoc.find ~equal:String.equal

(** Helper function to insert a (variable, ty) pair into the env *)
let add : env -> string -> ty -> env = List.Assoc.add ~equal:String.equal

exception Type_error of string

let ty_err msg = raise (Type_error msg)
let rec equal_ty (t1 : ty) (t2 : ty) : bool = 
  match (t1, t2) with
  |(TInt, TInt) -> true
  |(TBool, TBool) -> true
  |(TList type1, TList type2) -> (equal_ty type1 type2)
  |(TFun(type1, type2), TFun(type3, type4)) -> ((equal_ty type1 type3) && (equal_ty type2 type4))
  |(_, _) -> false

let rec abstract_eval (env : env) (e : expr) : ty =
  try
    match e with
    (* T-Int rule *)
    | Num _ -> TInt
    (* T-True and T-false *)
    | True | False -> TBool
    (* Your code here *)
    | Var x -> (match (find env x) with
                | Some xType -> xType
                | None ->  (ty_err "\ntype error 1 in var"))
    | Binop (op, e1, e2) -> if ((equal_ty (abstract_eval env e1) TInt) && (equal_ty (abstract_eval env e2) TInt)) then (match op with 
                                                                                                                | Add |Sub |Mul -> TInt
                                                                                                                | _ -> (ty_err "\ntype error 1 in binop")) else (ty_err "\ntype error 2 in binop")
    | Comp (op, e1, e2) -> if ((equal_ty (abstract_eval env e1) TInt) && (equal_ty (abstract_eval env e2) TInt)) then (match op with 
                                                                                                                      | Eq |Lt |Gt -> TBool
                                                                                                                      | _ -> (ty_err "\ntype error 1 in comp")) else (ty_err "\ntype error 2 in comp")
    | IfThenElse (e1, e2, e3) -> if ((equal_ty (abstract_eval env e1) TBool) && (equal_ty (abstract_eval env e2) (abstract_eval env e3))) then (abstract_eval env e2) else (ty_err "\ntype error in if")
    | Lambda (xType, (x, e)) -> (match xType with 
                                | Some t1 -> let t2 = (abstract_eval (add env x t1) e) in TFun(t1, t2)
                                | None -> (ty_err "\ntype error in lambda"))
    | App (e1, e2) -> (match (abstract_eval env e1) with
                      | TFun(t1, t2) -> (if (equal_ty t1 (abstract_eval env e2)) then t2 else (ty_err "\ntype error 1 in app")) 
                      | _ -> (ty_err "\ntype error 2 in app"))
    | Let (e1, (x, e2)) -> let t1 = (abstract_eval env e1) in (abstract_eval (add env x t1) e2)
    | ListNil t -> (match t with 
                  | Some nilType -> (TList nilType)
                  | None -> (ty_err "type error in ListNil"))
    | ListCons (e1, e2) -> let t = (abstract_eval env e1) in (let t_con = (abstract_eval env e2) in (if (equal_ty t_con (TList t)) then (TList t) else (ty_err "\ntype error in ListCons")))
    | ListMatch (e1, e2, (x, (y, e3))) -> (match (abstract_eval env e1) with
                                          | (TList t1) -> let t2 = (abstract_eval env e2) in (if (equal_ty t2 (abstract_eval (add (add env x t1) y (TList t1)) e3)) then t2 else (ty_err "type error 2 in ListMatch"))
                                          | _ -> (ty_err "type error 1 in ListMatch"))
    | Fix ((funType, (f, e))) -> (match funType with
                                | Some t -> (abstract_eval (add env f t) e)
                                | None -> (ty_err "type error in Fix"))
    | Annot (e, t) -> if(equal_ty (abstract_eval env e) t) then t else (ty_err "type error in Annot")
    | _ -> (ty_err "type error, ill formed expression")
  with Type_error msg -> ty_err (msg ^ "\nin expression " ^ show_expr e)
