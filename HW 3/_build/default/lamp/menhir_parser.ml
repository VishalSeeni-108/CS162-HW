
module MenhirBasics = struct
  
  exception Error
  
  let _eRR =
    fun _s ->
      raise Error
  
  type token = 
    | WITH
    | TRUE
    | TIMES
    | THINARROW
    | THEN
    | SUB
    | SND
    | RPAREN
    | REC
    | PLUS
    | NUMBER of (
# 19 "lamp/menhir_parser.mly"
       (int)
# 25 "lamp/menhir_parser.ml"
  )
    | NIL
    | MATCH
    | LT
    | LPAREN
    | LET
    | LAMBDA
    | IS
    | IN
    | IF
    | ID of (
# 20 "lamp/menhir_parser.mly"
       (string)
# 39 "lamp/menhir_parser.ml"
  )
    | GT
    | FUN
    | FST
    | FIX
    | FILE of (
# 21 "lamp/menhir_parser.mly"
       (string)
# 48 "lamp/menhir_parser.ml"
  )
    | FALSE
    | EQ
    | EOF
    | END
    | ELSE
    | E2
    | E1
    | DOT
    | CSAVE
    | CPRINT
    | CONS
    | COMMA
    | CLOAD
    | CLET
    | CCLEAR
    | BAR
  
end

include MenhirBasics

# 1 "lamp/menhir_parser.mly"
  

let mk_lambdas (xs : string list) (e : Ast.expr) : Ast.expr =
  let f x e' = Ast.Lambda((x,e')) in
  List.fold_right f xs e

# 78 "lamp/menhir_parser.ml"

type ('s, 'r) _menhir_state = 
  | MenhirState000 : ('s, _menhir_box_expr_eof) _menhir_state
    (** State 000.
        Stack shape : .
        Start symbol: expr_eof. *)

  | MenhirState004 : (('s, 'r) _menhir_cell1_MATCH, 'r) _menhir_state
    (** State 004.
        Stack shape : MATCH.
        Start symbol: <undetermined>. *)

  | MenhirState005 : (('s, 'r) _menhir_cell1_LPAREN, 'r) _menhir_state
    (** State 005.
        Stack shape : LPAREN.
        Start symbol: <undetermined>. *)

  | MenhirState006 : (('s, 'r) _menhir_cell1_LET, 'r) _menhir_state
    (** State 006.
        Stack shape : LET.
        Start symbol: <undetermined>. *)

  | MenhirState009 : ((('s, 'r) _menhir_cell1_LET, 'r) _menhir_cell1_bind, 'r) _menhir_state
    (** State 009.
        Stack shape : LET bind.
        Start symbol: <undetermined>. *)

  | MenhirState010 : (('s, 'r) _menhir_cell1_LAMBDA, 'r) _menhir_state
    (** State 010.
        Stack shape : LAMBDA.
        Start symbol: <undetermined>. *)

  | MenhirState013 : ((('s, 'r) _menhir_cell1_LAMBDA, 'r) _menhir_cell1_bindlist, 'r) _menhir_state
    (** State 013.
        Stack shape : LAMBDA bindlist.
        Start symbol: <undetermined>. *)

  | MenhirState014 : (('s, 'r) _menhir_cell1_IF, 'r) _menhir_state
    (** State 014.
        Stack shape : IF.
        Start symbol: <undetermined>. *)

  | MenhirState016 : (('s, 'r) _menhir_cell1_FUN, 'r) _menhir_state
    (** State 016.
        Stack shape : FUN.
        Start symbol: <undetermined>. *)

  | MenhirState017 : ((('s, 'r) _menhir_cell1_FUN, 'r) _menhir_cell1_REC, 'r) _menhir_state
    (** State 017.
        Stack shape : FUN REC.
        Start symbol: <undetermined>. *)

  | MenhirState019 : (((('s, 'r) _menhir_cell1_FUN, 'r) _menhir_cell1_REC, 'r) _menhir_cell1_bind, 'r) _menhir_state
    (** State 019.
        Stack shape : FUN REC bind.
        Start symbol: <undetermined>. *)

  | MenhirState021 : ((((('s, 'r) _menhir_cell1_FUN, 'r) _menhir_cell1_REC, 'r) _menhir_cell1_bind, 'r) _menhir_cell1_bindlist, 'r) _menhir_state
    (** State 021.
        Stack shape : FUN REC bind bindlist.
        Start symbol: <undetermined>. *)

  | MenhirState022 : (('s, 'r) _menhir_cell1_FIX, 'r) _menhir_state
    (** State 022.
        Stack shape : FIX.
        Start symbol: <undetermined>. *)

  | MenhirState024 : ((('s, 'r) _menhir_cell1_FIX, 'r) _menhir_cell1_bind, 'r) _menhir_state
    (** State 024.
        Stack shape : FIX bind.
        Start symbol: <undetermined>. *)

  | MenhirState026 : (('s, 'r) _menhir_cell1_E2, 'r) _menhir_state
    (** State 026.
        Stack shape : E2.
        Start symbol: <undetermined>. *)

  | MenhirState027 : (('s, 'r) _menhir_cell1_E1, 'r) _menhir_state
    (** State 027.
        Stack shape : E1.
        Start symbol: <undetermined>. *)

  | MenhirState035 : (('s, 'r) _menhir_cell1_app, 'r) _menhir_state
    (** State 035.
        Stack shape : app.
        Start symbol: <undetermined>. *)

  | MenhirState039 : (('s, 'r) _menhir_cell1_expr, 'r) _menhir_state
    (** State 039.
        Stack shape : expr.
        Start symbol: <undetermined>. *)

  | MenhirState041 : (('s, 'r) _menhir_cell1_expr, 'r) _menhir_state
    (** State 041.
        Stack shape : expr.
        Start symbol: <undetermined>. *)

  | MenhirState043 : (('s, 'r) _menhir_cell1_expr, 'r) _menhir_state
    (** State 043.
        Stack shape : expr.
        Start symbol: <undetermined>. *)

  | MenhirState045 : (('s, 'r) _menhir_cell1_expr, 'r) _menhir_state
    (** State 045.
        Stack shape : expr.
        Start symbol: <undetermined>. *)

  | MenhirState047 : (('s, 'r) _menhir_cell1_expr, 'r) _menhir_state
    (** State 047.
        Stack shape : expr.
        Start symbol: <undetermined>. *)

  | MenhirState049 : (('s, 'r) _menhir_cell1_expr, 'r) _menhir_state
    (** State 049.
        Stack shape : expr.
        Start symbol: <undetermined>. *)

  | MenhirState051 : (('s, 'r) _menhir_cell1_expr, 'r) _menhir_state
    (** State 051.
        Stack shape : expr.
        Start symbol: <undetermined>. *)

  | MenhirState054 : (((((('s, 'r) _menhir_cell1_FUN, 'r) _menhir_cell1_REC, 'r) _menhir_cell1_bind, 'r) _menhir_cell1_bindlist, 'r) _menhir_cell1_expr, 'r) _menhir_state
    (** State 054.
        Stack shape : FUN REC bind bindlist expr.
        Start symbol: <undetermined>. *)

  | MenhirState057 : (('s, 'r) _menhir_cell1_bind, 'r) _menhir_state
    (** State 057.
        Stack shape : bind.
        Start symbol: <undetermined>. *)

  | MenhirState060 : ((('s, 'r) _menhir_cell1_FUN, 'r) _menhir_cell1_bind, 'r) _menhir_state
    (** State 060.
        Stack shape : FUN bind.
        Start symbol: <undetermined>. *)

  | MenhirState062 : (((('s, 'r) _menhir_cell1_FUN, 'r) _menhir_cell1_bind, 'r) _menhir_cell1_bindlist, 'r) _menhir_state
    (** State 062.
        Stack shape : FUN bind bindlist.
        Start symbol: <undetermined>. *)

  | MenhirState064 : ((((('s, 'r) _menhir_cell1_FUN, 'r) _menhir_cell1_bind, 'r) _menhir_cell1_bindlist, 'r) _menhir_cell1_expr, 'r) _menhir_state
    (** State 064.
        Stack shape : FUN bind bindlist expr.
        Start symbol: <undetermined>. *)

  | MenhirState067 : ((('s, 'r) _menhir_cell1_IF, 'r) _menhir_cell1_expr, 'r) _menhir_state
    (** State 067.
        Stack shape : IF expr.
        Start symbol: <undetermined>. *)

  | MenhirState069 : (((('s, 'r) _menhir_cell1_IF, 'r) _menhir_cell1_expr, 'r) _menhir_cell1_expr, 'r) _menhir_state
    (** State 069.
        Stack shape : IF expr expr.
        Start symbol: <undetermined>. *)

  | MenhirState073 : (((('s, 'r) _menhir_cell1_LET, 'r) _menhir_cell1_bind, 'r) _menhir_cell1_expr, 'r) _menhir_state
    (** State 073.
        Stack shape : LET bind expr.
        Start symbol: <undetermined>. *)

  | MenhirState077 : ((('s, 'r) _menhir_cell1_LPAREN, 'r) _menhir_cell1_expr, 'r) _menhir_state
    (** State 077.
        Stack shape : LPAREN expr.
        Start symbol: <undetermined>. *)

  | MenhirState085 : ((('s, 'r) _menhir_cell1_MATCH, 'r) _menhir_cell1_expr _menhir_cell0_option_BAR_, 'r) _menhir_state
    (** State 085.
        Stack shape : MATCH expr option(BAR).
        Start symbol: <undetermined>. *)

  | MenhirState089 : ((('s, 'r) _menhir_cell1_MATCH, 'r) _menhir_cell1_expr _menhir_cell0_option_BAR_ _menhir_cell0_ID, 'r) _menhir_state
    (** State 089.
        Stack shape : MATCH expr option(BAR) ID.
        Start symbol: <undetermined>. *)

  | MenhirState096 : ((('s, 'r) _menhir_cell1_MATCH, 'r) _menhir_cell1_expr _menhir_cell0_option_BAR_ _menhir_cell0_nil_branch _menhir_cell0_ID _menhir_cell0_ID, 'r) _menhir_state
    (** State 096.
        Stack shape : MATCH expr option(BAR) nil_branch ID ID.
        Start symbol: <undetermined>. *)

  | MenhirState104 : ((('s, 'r) _menhir_cell1_MATCH, 'r) _menhir_cell1_expr _menhir_cell0_option_BAR_ _menhir_cell0_branch_E1_ _menhir_cell0_ID, 'r) _menhir_state
    (** State 104.
        Stack shape : MATCH expr option(BAR) branch(E1) ID.
        Start symbol: <undetermined>. *)

  | MenhirState111 : ('s, _menhir_box_repl_command_eof) _menhir_state
    (** State 111.
        Stack shape : .
        Start symbol: repl_command_eof. *)

  | MenhirState117 : (('s, 'r) _menhir_cell1_CLET, 'r) _menhir_state
    (** State 117.
        Stack shape : CLET.
        Start symbol: <undetermined>. *)

  | MenhirState119 : ((('s, 'r) _menhir_cell1_CLET, 'r) _menhir_cell1_bind, 'r) _menhir_state
    (** State 119.
        Stack shape : CLET bind.
        Start symbol: <undetermined>. *)

  | MenhirState127 : ('s, _menhir_box_script_eof) _menhir_state
    (** State 127.
        Stack shape : .
        Start symbol: script_eof. *)

  | MenhirState131 : (('s, _menhir_box_script_eof) _menhir_cell1_file_command, _menhir_box_script_eof) _menhir_state
    (** State 131.
        Stack shape : file_command.
        Start symbol: script_eof. *)


and ('s, 'r) _menhir_cell1_app = 
  | MenhirCell1_app of 's * ('s, 'r) _menhir_state * (Ast.expr)

and ('s, 'r) _menhir_cell1_bind = 
  | MenhirCell1_bind of 's * ('s, 'r) _menhir_state * (string)

and ('s, 'r) _menhir_cell1_bindlist = 
  | MenhirCell1_bindlist of 's * ('s, 'r) _menhir_state * (string list)

and 's _menhir_cell0_branch_E1_ = 
  | MenhirCell0_branch_E1_ of 's * (Ast.expr Ast.binder)

and ('s, 'r) _menhir_cell1_expr = 
  | MenhirCell1_expr of 's * ('s, 'r) _menhir_state * (Ast.expr)

and ('s, 'r) _menhir_cell1_file_command = 
  | MenhirCell1_file_command of 's * ('s, 'r) _menhir_state * (Cmd.t)

and 's _menhir_cell0_nil_branch = 
  | MenhirCell0_nil_branch of 's * (Ast.expr)

and 's _menhir_cell0_option_BAR_ = 
  | MenhirCell0_option_BAR_ of 's * (unit option)

and ('s, 'r) _menhir_cell1_term = 
  | MenhirCell1_term of 's * ('s, 'r) _menhir_state * (Ast.expr)

and ('s, 'r) _menhir_cell1_CLET = 
  | MenhirCell1_CLET of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_E1 = 
  | MenhirCell1_E1 of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_E2 = 
  | MenhirCell1_E2 of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_FIX = 
  | MenhirCell1_FIX of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_FUN = 
  | MenhirCell1_FUN of 's * ('s, 'r) _menhir_state

and 's _menhir_cell0_ID = 
  | MenhirCell0_ID of 's * (
# 20 "lamp/menhir_parser.mly"
       (string)
# 338 "lamp/menhir_parser.ml"
)

and ('s, 'r) _menhir_cell1_IF = 
  | MenhirCell1_IF of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_LAMBDA = 
  | MenhirCell1_LAMBDA of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_LET = 
  | MenhirCell1_LET of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_LPAREN = 
  | MenhirCell1_LPAREN of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_MATCH = 
  | MenhirCell1_MATCH of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_REC = 
  | MenhirCell1_REC of 's * ('s, 'r) _menhir_state

and _menhir_box_script_eof = 
  | MenhirBox_script_eof of (Cmd.t list) [@@unboxed]

and _menhir_box_repl_command_eof = 
  | MenhirBox_repl_command_eof of (Cmd.t) [@@unboxed]

and _menhir_box_expr_eof = 
  | MenhirBox_expr_eof of (Ast.expr) [@@unboxed]

let _menhir_action_03 =
  fun _1 ->
    (
# 124 "lamp/menhir_parser.mly"
    ( _1 )
# 373 "lamp/menhir_parser.ml"
     : (Ast.expr))

let _menhir_action_04 =
  fun e1 e2 ->
    (
# 126 "lamp/menhir_parser.mly"
    ( App(e1, e2) )
# 381 "lamp/menhir_parser.ml"
     : (Ast.expr))

let _menhir_action_05 =
  fun x ->
    (
# 73 "lamp/menhir_parser.mly"
    ( x )
# 389 "lamp/menhir_parser.ml"
     : (string))

let _menhir_action_06 =
  fun l ->
    (
# 77 "lamp/menhir_parser.mly"
    ( l )
# 397 "lamp/menhir_parser.ml"
     : (string list))

let _menhir_action_07 =
  fun _1 _3 ->
    (
# 108 "lamp/menhir_parser.mly"
    ( Binop(Add, _1, _3) )
# 405 "lamp/menhir_parser.ml"
     : (Ast.expr))

let _menhir_action_08 =
  fun _1 _3 ->
    (
# 110 "lamp/menhir_parser.mly"
    ( Binop(Sub, _1, _3) )
# 413 "lamp/menhir_parser.ml"
     : (Ast.expr))

let _menhir_action_09 =
  fun _1 _3 ->
    (
# 112 "lamp/menhir_parser.mly"
    ( Binop(Mul, _1, _3) )
# 421 "lamp/menhir_parser.ml"
     : (Ast.expr))

let _menhir_action_10 =
  fun _1 _3 ->
    (
# 114 "lamp/menhir_parser.mly"
    ( Comp(Lt, _1, _3) )
# 429 "lamp/menhir_parser.ml"
     : (Ast.expr))

let _menhir_action_11 =
  fun _1 _3 ->
    (
# 116 "lamp/menhir_parser.mly"
    ( Comp(Gt, _1, _3) )
# 437 "lamp/menhir_parser.ml"
     : (Ast.expr))

let _menhir_action_12 =
  fun _1 _3 ->
    (
# 118 "lamp/menhir_parser.mly"
    ( Comp(Eq, _1, _3) )
# 445 "lamp/menhir_parser.ml"
     : (Ast.expr))

let _menhir_action_13 =
  fun _1 _3 ->
    (
# 120 "lamp/menhir_parser.mly"
    ( ListCons(_1, _3) )
# 453 "lamp/menhir_parser.ml"
     : (Ast.expr))

let _menhir_action_14 =
  fun e x ->
    (
# 144 "lamp/menhir_parser.mly"
    ( (x, e) )
# 461 "lamp/menhir_parser.ml"
     : (Ast.expr Ast.binder))

let _menhir_action_15 =
  fun e x ->
    (
# 144 "lamp/menhir_parser.mly"
    ( (x, e) )
# 469 "lamp/menhir_parser.ml"
     : (Ast.expr Ast.binder))

let _menhir_action_16 =
  fun e x y ->
    (
# 134 "lamp/menhir_parser.mly"
    ( (x, y, e) )
# 477 "lamp/menhir_parser.ml"
     : (string * string * Ast.expr))

let _menhir_action_17 =
  fun e ->
    (
# 81 "lamp/menhir_parser.mly"
    ( e )
# 485 "lamp/menhir_parser.ml"
     : (Ast.expr))

let _menhir_action_18 =
  fun bs e ->
    (
# 83 "lamp/menhir_parser.mly"
    ( mk_lambdas bs e )
# 493 "lamp/menhir_parser.ml"
     : (Ast.expr))

let _menhir_action_19 =
  fun bs e1 e2 x ->
    (
# 85 "lamp/menhir_parser.mly"
    ( Let(Fix ((x, mk_lambdas bs e1)), (x, e2)) )
# 501 "lamp/menhir_parser.ml"
     : (Ast.expr))

let _menhir_action_20 =
  fun e1 e2 x xs ->
    (
# 87 "lamp/menhir_parser.mly"
    ( Let(mk_lambdas xs e1, (x, e2)) )
# 509 "lamp/menhir_parser.ml"
     : (Ast.expr))

let _menhir_action_21 =
  fun e x ->
    (
# 89 "lamp/menhir_parser.mly"
    ( Fix (x,e) )
# 517 "lamp/menhir_parser.ml"
     : (Ast.expr))

let _menhir_action_22 =
  fun b e1 e2 ->
    (
# 91 "lamp/menhir_parser.mly"
    ( let x = b in
      Ast.Let (e1, (x,e2)) )
# 526 "lamp/menhir_parser.ml"
     : (Ast.expr))

let _menhir_action_23 =
  fun e1 e2 e3 ->
    (
# 94 "lamp/menhir_parser.mly"
    ( IfThenElse(e1, e2, e3) )
# 534 "lamp/menhir_parser.ml"
     : (Ast.expr))

let _menhir_action_24 =
  fun e ->
    (
# 96 "lamp/menhir_parser.mly"
    ( e )
# 542 "lamp/menhir_parser.ml"
     : (Ast.expr))

let _menhir_action_25 =
  fun e ->
    (
# 98 "lamp/menhir_parser.mly"
    ( e )
# 550 "lamp/menhir_parser.ml"
     : (Ast.expr))

let _menhir_action_26 =
  fun e ->
    (
# 100 "lamp/menhir_parser.mly"
    ( e )
# 558 "lamp/menhir_parser.ml"
     : (Ast.expr))

let _menhir_action_27 =
  fun e ->
    (
# 102 "lamp/menhir_parser.mly"
    ( E1 e )
# 566 "lamp/menhir_parser.ml"
     : (Ast.expr))

let _menhir_action_28 =
  fun e ->
    (
# 104 "lamp/menhir_parser.mly"
    ( E2 e )
# 574 "lamp/menhir_parser.ml"
     : (Ast.expr))

let _menhir_action_29 =
  fun e ->
    (
# 69 "lamp/menhir_parser.mly"
    ( e )
# 582 "lamp/menhir_parser.ml"
     : (Ast.expr))

let _menhir_action_30 =
  fun _2 _4 ->
    (
# 43 "lamp/menhir_parser.mly"
    ( let x = _2 in Cmd.CLet(x,_4) )
# 590 "lamp/menhir_parser.ml"
     : (Cmd.t))

let _menhir_action_31 =
  fun () ->
    (
# 45 "lamp/menhir_parser.mly"
    ( Cmd.CPrint)
# 598 "lamp/menhir_parser.ml"
     : (Cmd.t))

let _menhir_action_32 =
  fun () ->
    (
# 47 "lamp/menhir_parser.mly"
    ( Cmd.CClear)
# 606 "lamp/menhir_parser.ml"
     : (Cmd.t))

let _menhir_action_33 =
  fun _2 ->
    (
# 49 "lamp/menhir_parser.mly"
    ( Cmd.CLoad _2 )
# 614 "lamp/menhir_parser.ml"
     : (Cmd.t))

let _menhir_action_34 =
  fun _2 ->
    (
# 51 "lamp/menhir_parser.mly"
    ( Cmd.CSave _2 )
# 622 "lamp/menhir_parser.ml"
     : (Cmd.t))

let _menhir_action_35 =
  fun () ->
    (
# 216 "<standard.mly>"
    ( [] )
# 630 "lamp/menhir_parser.ml"
     : (Cmd.t list))

let _menhir_action_36 =
  fun x xs ->
    (
# 219 "<standard.mly>"
    ( x :: xs )
# 638 "lamp/menhir_parser.ml"
     : (Cmd.t list))

let _menhir_action_37 =
  fun b3 e1 e2 ->
    (
# 138 "lamp/menhir_parser.mly"
    (
      let (x,y,e3) = b3  in
      Ast.ListMatch(e1, e2, (x, (y, e3))))
# 648 "lamp/menhir_parser.ml"
     : (Ast.expr))

let _menhir_action_38 =
  fun e ->
    (
# 130 "lamp/menhir_parser.mly"
    ( e )
# 656 "lamp/menhir_parser.ml"
     : (Ast.expr))

let _menhir_action_39 =
  fun () ->
    (
# 111 "<standard.mly>"
    ( None )
# 664 "lamp/menhir_parser.ml"
     : (unit option))

let _menhir_action_40 =
  fun x ->
    (
# 114 "<standard.mly>"
    ( Some x )
# 672 "lamp/menhir_parser.ml"
     : (unit option))

let _menhir_action_41 =
  fun c ->
    (
# 55 "lamp/menhir_parser.mly"
    ( c )
# 680 "lamp/menhir_parser.ml"
     : (Cmd.t))

let _menhir_action_42 =
  fun _1 ->
    (
# 57 "lamp/menhir_parser.mly"
    ( Cmd.CEval _1 )
# 688 "lamp/menhir_parser.ml"
     : (Cmd.t))

let _menhir_action_43 =
  fun c ->
    (
# 61 "lamp/menhir_parser.mly"
    ( c )
# 696 "lamp/menhir_parser.ml"
     : (Cmd.t))

let _menhir_action_44 =
  fun cs ->
    (
# 65 "lamp/menhir_parser.mly"
    ( cs )
# 704 "lamp/menhir_parser.ml"
     : (Cmd.t list))

let _menhir_action_45 =
  fun x ->
    (
# 250 "<standard.mly>"
    ( [ x ] )
# 712 "lamp/menhir_parser.ml"
     : (string list))

let _menhir_action_46 =
  fun x xs ->
    (
# 253 "<standard.mly>"
    ( x :: xs )
# 720 "lamp/menhir_parser.ml"
     : (string list))

let _menhir_action_47 =
  fun b2 b3 e1 ->
    (
# 148 "lamp/menhir_parser.mly"
    ( Ast.Either(e1, b2, b3) )
# 728 "lamp/menhir_parser.ml"
     : (Ast.expr))

let _menhir_action_48 =
  fun x ->
    (
# 152 "lamp/menhir_parser.mly"
    ( Ast.Var x )
# 736 "lamp/menhir_parser.ml"
     : (Ast.expr))

let _menhir_action_49 =
  fun n ->
    (
# 154 "lamp/menhir_parser.mly"
    ( Ast.Num n )
# 744 "lamp/menhir_parser.ml"
     : (Ast.expr))

let _menhir_action_50 =
  fun () ->
    (
# 156 "lamp/menhir_parser.mly"
    ( Ast.True )
# 752 "lamp/menhir_parser.ml"
     : (Ast.expr))

let _menhir_action_51 =
  fun () ->
    (
# 158 "lamp/menhir_parser.mly"
    ( Ast.False )
# 760 "lamp/menhir_parser.ml"
     : (Ast.expr))

let _menhir_action_52 =
  fun () ->
    (
# 160 "lamp/menhir_parser.mly"
    ( ListNil )
# 768 "lamp/menhir_parser.ml"
     : (Ast.expr))

let _menhir_action_53 =
  fun e ->
    (
# 162 "lamp/menhir_parser.mly"
    ( e )
# 776 "lamp/menhir_parser.ml"
     : (Ast.expr))

let _menhir_action_54 =
  fun e1 e2 ->
    (
# 164 "lamp/menhir_parser.mly"
    ( Ast.Both(e1, e2) )
# 784 "lamp/menhir_parser.ml"
     : (Ast.expr))

let _menhir_action_55 =
  fun e ->
    (
# 166 "lamp/menhir_parser.mly"
    ( Ast.I1 e )
# 792 "lamp/menhir_parser.ml"
     : (Ast.expr))

let _menhir_action_56 =
  fun e ->
    (
# 168 "lamp/menhir_parser.mly"
    ( Ast.I2 e )
# 800 "lamp/menhir_parser.ml"
     : (Ast.expr))

let _menhir_print_token : token -> string =
  fun _tok ->
    match _tok with
    | BAR ->
        "BAR"
    | CCLEAR ->
        "CCLEAR"
    | CLET ->
        "CLET"
    | CLOAD ->
        "CLOAD"
    | COMMA ->
        "COMMA"
    | CONS ->
        "CONS"
    | CPRINT ->
        "CPRINT"
    | CSAVE ->
        "CSAVE"
    | DOT ->
        "DOT"
    | E1 ->
        "E1"
    | E2 ->
        "E2"
    | ELSE ->
        "ELSE"
    | END ->
        "END"
    | EOF ->
        "EOF"
    | EQ ->
        "EQ"
    | FALSE ->
        "FALSE"
    | FILE _ ->
        "FILE"
    | FIX ->
        "FIX"
    | FST ->
        "FST"
    | FUN ->
        "FUN"
    | GT ->
        "GT"
    | ID _ ->
        "ID"
    | IF ->
        "IF"
    | IN ->
        "IN"
    | IS ->
        "IS"
    | LAMBDA ->
        "LAMBDA"
    | LET ->
        "LET"
    | LPAREN ->
        "LPAREN"
    | LT ->
        "LT"
    | MATCH ->
        "MATCH"
    | NIL ->
        "NIL"
    | NUMBER _ ->
        "NUMBER"
    | PLUS ->
        "PLUS"
    | REC ->
        "REC"
    | RPAREN ->
        "RPAREN"
    | SND ->
        "SND"
    | SUB ->
        "SUB"
    | THEN ->
        "THEN"
    | THINARROW ->
        "THINARROW"
    | TIMES ->
        "TIMES"
    | TRUE ->
        "TRUE"
    | WITH ->
        "WITH"

let _menhir_fail : unit -> 'a =
  fun () ->
    Printf.eprintf "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

include struct
  
  [@@@ocaml.warning "-4-37"]
  
  let _menhir_goto_repl_command : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_repl_command_eof =
    fun _menhir_stack _v _tok ->
      match (_tok : MenhirBasics.token) with
      | EOF ->
          let c = _v in
          let _v = _menhir_action_43 c in
          MenhirBox_repl_command_eof _v
      | _ ->
          _eRR ()
  
  let _menhir_run_129 : type  ttv_stack. ttv_stack -> _ -> _menhir_box_script_eof =
    fun _menhir_stack _v ->
      let cs = _v in
      let _v = _menhir_action_44 cs in
      MenhirBox_script_eof _v
  
  let rec _menhir_run_132 : type  ttv_stack. (ttv_stack, _menhir_box_script_eof) _menhir_cell1_file_command -> _ -> _menhir_box_script_eof =
    fun _menhir_stack _v ->
      let MenhirCell1_file_command (_menhir_stack, _menhir_s, x) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_36 x xs in
      _menhir_goto_list_file_command_ _menhir_stack _v _menhir_s
  
  and _menhir_goto_list_file_command_ : type  ttv_stack. ttv_stack -> _ -> (ttv_stack, _menhir_box_script_eof) _menhir_state -> _menhir_box_script_eof =
    fun _menhir_stack _v _menhir_s ->
      match _menhir_s with
      | MenhirState131 ->
          _menhir_run_132 _menhir_stack _v
      | MenhirState127 ->
          _menhir_run_129 _menhir_stack _v
      | _ ->
          _menhir_fail ()
  
  let _menhir_run_125 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_repl_command_eof =
    fun _menhir_stack _v _tok ->
      let c = _v in
      let _v = _menhir_action_41 c in
      _menhir_goto_repl_command _menhir_stack _v _tok
  
  let rec _menhir_run_001 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_50 () in
      _menhir_goto_term _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_term : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState035 ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState111 ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState119 ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState000 ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState104 ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState096 ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState089 ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState085 ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState004 ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState077 ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState005 ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState073 ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState009 ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState013 ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState069 ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState067 ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState014 ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState064 ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState062 ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState054 ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState021 ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState051 ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState049 ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState047 ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState045 ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState043 ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState041 ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState039 ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState024 ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState026 ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState027 ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_036 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_app as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SND ->
          let _menhir_stack = MenhirCell1_term (_menhir_stack, _menhir_s, _v) in
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer
      | FST ->
          let _menhir_stack = MenhirCell1_term (_menhir_stack, _menhir_s, _v) in
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer
      | BAR | CCLEAR | CLET | CLOAD | COMMA | CONS | CPRINT | CSAVE | ELSE | END | EOF | EQ | FALSE | GT | ID _ | IN | LPAREN | LT | NIL | NUMBER _ | PLUS | RPAREN | SUB | THEN | TIMES | TRUE | WITH ->
          let MenhirCell1_app (_menhir_stack, _menhir_s, e1) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_04 e1 e2 in
          _menhir_goto_app _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_029 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_term -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_term (_menhir_stack, _menhir_s, e) = _menhir_stack in
      let _v = _menhir_action_56 e in
      _menhir_goto_term _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_030 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_term -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_term (_menhir_stack, _menhir_s, e) = _menhir_stack in
      let _v = _menhir_action_55 e in
      _menhir_goto_term _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_app : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          let _menhir_stack = MenhirCell1_app (_menhir_stack, _menhir_s, _v) in
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState035
      | NUMBER _v_0 ->
          let _menhir_stack = MenhirCell1_app (_menhir_stack, _menhir_s, _v) in
          _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState035
      | NIL ->
          let _menhir_stack = MenhirCell1_app (_menhir_stack, _menhir_s, _v) in
          _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState035
      | LPAREN ->
          let _menhir_stack = MenhirCell1_app (_menhir_stack, _menhir_s, _v) in
          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState035
      | ID _v_1 ->
          let _menhir_stack = MenhirCell1_app (_menhir_stack, _menhir_s, _v) in
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState035
      | FALSE ->
          let _menhir_stack = MenhirCell1_app (_menhir_stack, _menhir_s, _v) in
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState035
      | BAR | CCLEAR | CLET | CLOAD | COMMA | CONS | CPRINT | CSAVE | ELSE | END | EOF | EQ | GT | IN | LT | PLUS | RPAREN | SUB | THEN | TIMES | WITH ->
          let e = _v in
          let _v = _menhir_action_17 e in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_002 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let n = _v in
      let _v = _menhir_action_49 n in
      _menhir_goto_term _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_003 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_52 () in
      _menhir_goto_term _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_005 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState005 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NUMBER _v ->
          _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | NIL ->
          _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LAMBDA ->
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IF ->
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ID _v ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FUN ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FIX ->
          _menhir_run_022 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FALSE ->
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | E2 ->
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | E1 ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_004 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_MATCH (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState004 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NUMBER _v ->
          _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | NIL ->
          _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LAMBDA ->
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IF ->
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ID _v ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FUN ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FIX ->
          _menhir_run_022 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FALSE ->
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | E2 ->
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | E1 ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_006 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LET (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState006 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | ID _v ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_007 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let x = _v in
      let _v = _menhir_action_05 x in
      _menhir_goto_bind _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_bind : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState117 ->
          _menhir_run_118 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState016 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState010 ->
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState060 ->
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState057 ->
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState019 ->
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState022 ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState017 ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState006 ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_118 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_CLET as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_bind (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | EQ ->
          let _menhir_s = MenhirState119 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TRUE ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NUMBER _v ->
              _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | NIL ->
              _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LAMBDA ->
              _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IF ->
              _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ID _v ->
              _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FIX ->
              _menhir_run_022 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FALSE ->
              _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | E2 ->
              _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | E1 ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_010 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LAMBDA (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState010 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | ID _v ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_014 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_IF (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState014 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NUMBER _v ->
          _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | NIL ->
          _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LAMBDA ->
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IF ->
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ID _v ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FUN ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FIX ->
          _menhir_run_022 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FALSE ->
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | E2 ->
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | E1 ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_015 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let x = _v in
      let _v = _menhir_action_48 x in
      _menhir_goto_term _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_016 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_FUN (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState016 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | REC ->
          let _menhir_stack = MenhirCell1_REC (_menhir_stack, _menhir_s) in
          let _menhir_s = MenhirState017 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | ID _v ->
              _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | ID _v ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_022 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_FIX (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState022 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | ID _v ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_025 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_51 () in
      _menhir_goto_term _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_026 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_E2 (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState026 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NUMBER _v ->
          _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | NIL ->
          _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LAMBDA ->
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IF ->
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ID _v ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FUN ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FIX ->
          _menhir_run_022 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FALSE ->
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | E2 ->
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | E1 ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_027 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_E1 (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState027 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NUMBER _v ->
          _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | NIL ->
          _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LAMBDA ->
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IF ->
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ID _v ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FUN ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FIX ->
          _menhir_run_022 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FALSE ->
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | E2 ->
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | E1 ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_059 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_FUN as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_bind (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | WITH ->
          let _menhir_s = MenhirState060 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | ID _v ->
              _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_056 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | COMMA ->
          let _menhir_stack = MenhirCell1_bind (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState057 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | ID _v ->
              _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | DOT | EQ ->
          let x = _v in
          let _v = _menhir_action_45 x in
          _menhir_goto_separated_nonempty_list_COMMA_bind_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_separated_nonempty_list_COMMA_bind_ : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState057 ->
          _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState060 ->
          _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState019 ->
          _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState010 ->
          _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_058 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_bind -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_bind (_menhir_stack, _menhir_s, x) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_46 x xs in
      _menhir_goto_separated_nonempty_list_COMMA_bind_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_011 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let l = _v in
      let _v = _menhir_action_06 l in
      _menhir_goto_bindlist _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_bindlist : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState060 ->
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState019 ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState010 ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_061 : type  ttv_stack ttv_result. (((ttv_stack, ttv_result) _menhir_cell1_FUN, ttv_result) _menhir_cell1_bind as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_bindlist (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | EQ ->
          let _menhir_s = MenhirState062 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TRUE ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NUMBER _v ->
              _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | NIL ->
              _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LAMBDA ->
              _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IF ->
              _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ID _v ->
              _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FIX ->
              _menhir_run_022 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FALSE ->
              _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | E2 ->
              _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | E1 ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_020 : type  ttv_stack ttv_result. ((((ttv_stack, ttv_result) _menhir_cell1_FUN, ttv_result) _menhir_cell1_REC, ttv_result) _menhir_cell1_bind as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_bindlist (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | EQ ->
          let _menhir_s = MenhirState021 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TRUE ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NUMBER _v ->
              _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | NIL ->
              _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LAMBDA ->
              _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IF ->
              _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ID _v ->
              _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FIX ->
              _menhir_run_022 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FALSE ->
              _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | E2 ->
              _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | E1 ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_012 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_LAMBDA as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_bindlist (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | DOT ->
          let _menhir_s = MenhirState013 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TRUE ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NUMBER _v ->
              _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | NIL ->
              _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LAMBDA ->
              _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IF ->
              _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ID _v ->
              _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FIX ->
              _menhir_run_022 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FALSE ->
              _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | E2 ->
              _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | E1 ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_023 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_FIX as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_bind (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | IS ->
          let _menhir_s = MenhirState024 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TRUE ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NUMBER _v ->
              _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | NIL ->
              _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LAMBDA ->
              _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IF ->
              _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ID _v ->
              _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FIX ->
              _menhir_run_022 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FALSE ->
              _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | E2 ->
              _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | E1 ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_018 : type  ttv_stack ttv_result. (((ttv_stack, ttv_result) _menhir_cell1_FUN, ttv_result) _menhir_cell1_REC as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_bind (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | WITH ->
          let _menhir_s = MenhirState019 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | ID _v ->
              _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_008 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_LET as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_bind (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | EQ ->
          let _menhir_s = MenhirState009 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TRUE ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NUMBER _v ->
              _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | NIL ->
              _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LAMBDA ->
              _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IF ->
              _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ID _v ->
              _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FIX ->
              _menhir_run_022 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FALSE ->
              _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | E2 ->
              _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | E1 ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_goto_expr : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState111 ->
          _menhir_run_126 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState119 ->
          _menhir_run_120 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState000 ->
          _menhir_run_109 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState104 ->
          _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState096 ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState089 ->
          _menhir_run_090 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState085 ->
          _menhir_run_086 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState004 ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState077 ->
          _menhir_run_078 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState005 ->
          _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState073 ->
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState009 ->
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState013 ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState069 ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState067 ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState014 ->
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState064 ->
          _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState062 ->
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState054 ->
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState021 ->
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState051 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState049 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState047 ->
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState045 ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState043 ->
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState041 ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState039 ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState024 ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState026 ->
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState027 ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_126 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_repl_command_eof) _menhir_state -> _ -> _menhir_box_repl_command_eof =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer
      | SUB ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer
      | CONS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EOF ->
          let _1 = _v in
          let _v = _menhir_action_42 _1 in
          _menhir_goto_repl_command _menhir_stack _v _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_039 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_expr -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState039 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NUMBER _v ->
          _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | NIL ->
          _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LAMBDA ->
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IF ->
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ID _v ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FUN ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FIX ->
          _menhir_run_022 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FALSE ->
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | E2 ->
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | E1 ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_041 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_expr -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState041 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NUMBER _v ->
          _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | NIL ->
          _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LAMBDA ->
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IF ->
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ID _v ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FUN ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FIX ->
          _menhir_run_022 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FALSE ->
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | E2 ->
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | E1 ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_043 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_expr -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState043 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NUMBER _v ->
          _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | NIL ->
          _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LAMBDA ->
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IF ->
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ID _v ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FUN ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FIX ->
          _menhir_run_022 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FALSE ->
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | E2 ->
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | E1 ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_045 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_expr -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState045 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NUMBER _v ->
          _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | NIL ->
          _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LAMBDA ->
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IF ->
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ID _v ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FUN ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FIX ->
          _menhir_run_022 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FALSE ->
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | E2 ->
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | E1 ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_047 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_expr -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState047 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NUMBER _v ->
          _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | NIL ->
          _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LAMBDA ->
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IF ->
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ID _v ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FUN ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FIX ->
          _menhir_run_022 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FALSE ->
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | E2 ->
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | E1 ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_049 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_expr -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState049 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NUMBER _v ->
          _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | NIL ->
          _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LAMBDA ->
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IF ->
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ID _v ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FUN ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FIX ->
          _menhir_run_022 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FALSE ->
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | E2 ->
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | E1 ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_051 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_expr -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState051 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NUMBER _v ->
          _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | NIL ->
          _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LAMBDA ->
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IF ->
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ID _v ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FUN ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FIX ->
          _menhir_run_022 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FALSE ->
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | E2 ->
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | E1 ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_120 : type  ttv_stack ttv_result. (((ttv_stack, ttv_result) _menhir_cell1_CLET, ttv_result) _menhir_cell1_bind as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer
      | SUB ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer
      | CONS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer
      | CCLEAR | CLET | CLOAD | CPRINT | CSAVE | EOF ->
          let MenhirCell1_bind (_menhir_stack, _, _2) = _menhir_stack in
          let MenhirCell1_CLET (_menhir_stack, _menhir_s) = _menhir_stack in
          let _4 = _v in
          let _v = _menhir_action_30 _2 _4 in
          _menhir_goto_file_command _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_file_command : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState131 ->
          _menhir_run_131 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState127 ->
          _menhir_run_131 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState111 ->
          _menhir_run_125 _menhir_stack _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_131 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_script_eof) _menhir_state -> _ -> _menhir_box_script_eof =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_file_command (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | CSAVE ->
          _menhir_run_112 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState131
      | CPRINT ->
          _menhir_run_114 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState131
      | CLOAD ->
          _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState131
      | CLET ->
          _menhir_run_117 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState131
      | CCLEAR ->
          _menhir_run_121 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState131
      | EOF ->
          let _v_0 = _menhir_action_35 () in
          _menhir_run_132 _menhir_stack _v_0
      | _ ->
          _eRR ()
  
  and _menhir_run_112 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | FILE _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _2 = _v in
          let _v = _menhir_action_34 _2 in
          _menhir_goto_file_command _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_114 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_31 () in
      _menhir_goto_file_command _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_115 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | FILE _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _2 = _v in
          let _v = _menhir_action_33 _2 in
          _menhir_goto_file_command _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_117 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_CLET (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState117 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | ID _v ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_121 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_32 () in
      _menhir_goto_file_command _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_109 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_expr_eof) _menhir_state -> _ -> _menhir_box_expr_eof =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer
      | SUB ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EOF ->
          let e = _v in
          let _v = _menhir_action_29 e in
          MenhirBox_expr_eof _v
      | CONS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_105 : type  ttv_stack ttv_result. (((ttv_stack, ttv_result) _menhir_cell1_MATCH, ttv_result) _menhir_cell1_expr _menhir_cell0_option_BAR_ _menhir_cell0_branch_E1_ _menhir_cell0_ID as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer
      | SUB ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer
      | CONS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer
      | END ->
          let MenhirCell0_ID (_menhir_stack, x) = _menhir_stack in
          let e = _v in
          let _v = _menhir_action_15 e x in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell0_branch_E1_ (_menhir_stack, b2) = _menhir_stack in
          let MenhirCell0_option_BAR_ (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_expr (_menhir_stack, _, e1) = _menhir_stack in
          let MenhirCell1_MATCH (_menhir_stack, _menhir_s) = _menhir_stack in
          let b3 = _v in
          let _v = _menhir_action_47 b2 b3 e1 in
          let e = _v in
          let _v = _menhir_action_25 e in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_097 : type  ttv_stack ttv_result. (((ttv_stack, ttv_result) _menhir_cell1_MATCH, ttv_result) _menhir_cell1_expr _menhir_cell0_option_BAR_ _menhir_cell0_nil_branch _menhir_cell0_ID _menhir_cell0_ID as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer
      | SUB ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer
      | CONS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer
      | END ->
          let MenhirCell0_ID (_menhir_stack, y) = _menhir_stack in
          let MenhirCell0_ID (_menhir_stack, x) = _menhir_stack in
          let e = _v in
          let _v = _menhir_action_16 e x y in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell0_nil_branch (_menhir_stack, e2) = _menhir_stack in
          let MenhirCell0_option_BAR_ (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_expr (_menhir_stack, _, e1) = _menhir_stack in
          let MenhirCell1_MATCH (_menhir_stack, _menhir_s) = _menhir_stack in
          let b3 = _v in
          let _v = _menhir_action_37 b3 e1 e2 in
          let e = _v in
          let _v = _menhir_action_24 e in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_090 : type  ttv_stack ttv_result. (((ttv_stack, ttv_result) _menhir_cell1_MATCH, ttv_result) _menhir_cell1_expr _menhir_cell0_option_BAR_ _menhir_cell0_ID as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer
      | SUB ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer
      | CONS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer
      | BAR ->
          let MenhirCell0_ID (_menhir_stack, x) = _menhir_stack in
          let e = _v in
          let _v = _menhir_action_14 e x in
          let _menhir_stack = MenhirCell0_branch_E1_ (_menhir_stack, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | E2 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | ID _v ->
                  let _menhir_stack = MenhirCell0_ID (_menhir_stack, _v) in
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  (match (_tok : MenhirBasics.token) with
                  | THINARROW ->
                      let _menhir_s = MenhirState104 in
                      let _tok = _menhir_lexer _menhir_lexbuf in
                      (match (_tok : MenhirBasics.token) with
                      | TRUE ->
                          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                      | NUMBER _v ->
                          _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
                      | NIL ->
                          _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                      | MATCH ->
                          _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                      | LPAREN ->
                          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                      | LET ->
                          _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                      | LAMBDA ->
                          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                      | IF ->
                          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                      | ID _v ->
                          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
                      | FUN ->
                          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                      | FIX ->
                          _menhir_run_022 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                      | FALSE ->
                          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                      | E2 ->
                          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                      | E1 ->
                          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                      | _ ->
                          _eRR ())
                  | _ ->
                      _eRR ())
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_086 : type  ttv_stack ttv_result. (((ttv_stack, ttv_result) _menhir_cell1_MATCH, ttv_result) _menhir_cell1_expr _menhir_cell0_option_BAR_ as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer
      | SUB ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer
      | CONS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer
      | BAR ->
          let e = _v in
          let _v = _menhir_action_38 e in
          let _menhir_stack = MenhirCell0_nil_branch (_menhir_stack, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | ID _v ->
              let _menhir_stack = MenhirCell0_ID (_menhir_stack, _v) in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | CONS ->
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  (match (_tok : MenhirBasics.token) with
                  | ID _v ->
                      let _menhir_stack = MenhirCell0_ID (_menhir_stack, _v) in
                      let _tok = _menhir_lexer _menhir_lexbuf in
                      (match (_tok : MenhirBasics.token) with
                      | THINARROW ->
                          let _menhir_s = MenhirState096 in
                          let _tok = _menhir_lexer _menhir_lexbuf in
                          (match (_tok : MenhirBasics.token) with
                          | TRUE ->
                              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                          | NUMBER _v ->
                              _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
                          | NIL ->
                              _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                          | MATCH ->
                              _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                          | LPAREN ->
                              _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                          | LET ->
                              _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                          | LAMBDA ->
                              _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                          | IF ->
                              _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                          | ID _v ->
                              _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
                          | FUN ->
                              _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                          | FIX ->
                              _menhir_run_022 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                          | FALSE ->
                              _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                          | E2 ->
                              _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                          | E1 ->
                              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                          | _ ->
                              _eRR ())
                      | _ ->
                          _eRR ())
                  | _ ->
                      _eRR ())
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_080 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_MATCH as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | WITH ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | BAR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let x = () in
              let _v = _menhir_action_40 x in
              _menhir_goto_option_BAR_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | E1 | NIL ->
              let _v = _menhir_action_39 () in
              _menhir_goto_option_BAR_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | _ ->
              _eRR ())
      | TIMES ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer
      | SUB ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LT ->
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GT ->
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer
      | CONS ->
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_goto_option_BAR_ : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_MATCH, ttv_result) _menhir_cell1_expr -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _menhir_stack = MenhirCell0_option_BAR_ (_menhir_stack, _v) in
      match (_tok : MenhirBasics.token) with
      | NIL ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | THINARROW ->
              let _menhir_s = MenhirState085 in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | TRUE ->
                  _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | NUMBER _v ->
                  _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | NIL ->
                  _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | MATCH ->
                  _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | LPAREN ->
                  _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | LET ->
                  _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | LAMBDA ->
                  _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | IF ->
                  _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | ID _v ->
                  _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | FUN ->
                  _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | FIX ->
                  _menhir_run_022 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | FALSE ->
                  _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | E2 ->
                  _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | E1 ->
                  _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | E1 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | ID _v ->
              let _menhir_stack = MenhirCell0_ID (_menhir_stack, _v) in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | THINARROW ->
                  let _menhir_s = MenhirState089 in
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  (match (_tok : MenhirBasics.token) with
                  | TRUE ->
                      _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | NUMBER _v ->
                      _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
                  | NIL ->
                      _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | MATCH ->
                      _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | LPAREN ->
                      _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | LET ->
                      _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | LAMBDA ->
                      _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | IF ->
                      _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | ID _v ->
                      _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
                  | FUN ->
                      _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | FIX ->
                      _menhir_run_022 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | FALSE ->
                      _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | E2 ->
                      _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | E1 ->
                      _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | _ ->
                      _eRR ())
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_078 : type  ttv_stack ttv_result. (((ttv_stack, ttv_result) _menhir_cell1_LPAREN, ttv_result) _menhir_cell1_expr as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer
      | SUB ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer
      | RPAREN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_expr (_menhir_stack, _, e1) = _menhir_stack in
          let MenhirCell1_LPAREN (_menhir_stack, _menhir_s) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_54 e1 e2 in
          _menhir_goto_term _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer
      | CONS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_075 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_LPAREN as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer
      | SUB ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer
      | RPAREN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_LPAREN (_menhir_stack, _menhir_s) = _menhir_stack in
          let e = _v in
          let _v = _menhir_action_53 e in
          _menhir_goto_term _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer
      | CONS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer
      | COMMA ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState077 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TRUE ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NUMBER _v ->
              _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | NIL ->
              _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LAMBDA ->
              _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IF ->
              _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ID _v ->
              _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FIX ->
              _menhir_run_022 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FALSE ->
              _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | E2 ->
              _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | E1 ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_074 : type  ttv_stack ttv_result. ((((ttv_stack, ttv_result) _menhir_cell1_LET, ttv_result) _menhir_cell1_bind, ttv_result) _menhir_cell1_expr as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer
      | SUB ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer
      | CONS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer
      | BAR | CCLEAR | CLET | CLOAD | COMMA | CPRINT | CSAVE | ELSE | END | EOF | IN | RPAREN | THEN | WITH ->
          let MenhirCell1_expr (_menhir_stack, _, e1) = _menhir_stack in
          let MenhirCell1_bind (_menhir_stack, _, b) = _menhir_stack in
          let MenhirCell1_LET (_menhir_stack, _menhir_s) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_22 b e1 e2 in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_072 : type  ttv_stack ttv_result. (((ttv_stack, ttv_result) _menhir_cell1_LET, ttv_result) _menhir_cell1_bind as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer
      | SUB ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LT ->
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer
      | IN ->
          let _menhir_s = MenhirState073 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TRUE ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NUMBER _v ->
              _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | NIL ->
              _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LAMBDA ->
              _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IF ->
              _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ID _v ->
              _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FIX ->
              _menhir_run_022 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FALSE ->
              _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | E2 ->
              _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | E1 ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | GT ->
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer
      | CONS ->
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_071 : type  ttv_stack ttv_result. (((ttv_stack, ttv_result) _menhir_cell1_LAMBDA, ttv_result) _menhir_cell1_bindlist as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer
      | SUB ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer
      | CONS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer
      | BAR | CCLEAR | CLET | CLOAD | COMMA | CPRINT | CSAVE | ELSE | END | EOF | IN | RPAREN | THEN | WITH ->
          let MenhirCell1_bindlist (_menhir_stack, _, bs) = _menhir_stack in
          let MenhirCell1_LAMBDA (_menhir_stack, _menhir_s) = _menhir_stack in
          let e = _v in
          let _v = _menhir_action_18 bs e in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_070 : type  ttv_stack ttv_result. ((((ttv_stack, ttv_result) _menhir_cell1_IF, ttv_result) _menhir_cell1_expr, ttv_result) _menhir_cell1_expr as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer
      | SUB ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer
      | CONS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer
      | BAR | CCLEAR | CLET | CLOAD | COMMA | CPRINT | CSAVE | ELSE | END | EOF | IN | RPAREN | THEN | WITH ->
          let MenhirCell1_expr (_menhir_stack, _, e2) = _menhir_stack in
          let MenhirCell1_expr (_menhir_stack, _, e1) = _menhir_stack in
          let MenhirCell1_IF (_menhir_stack, _menhir_s) = _menhir_stack in
          let e3 = _v in
          let _v = _menhir_action_23 e1 e2 e3 in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_068 : type  ttv_stack ttv_result. (((ttv_stack, ttv_result) _menhir_cell1_IF, ttv_result) _menhir_cell1_expr as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer
      | SUB ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LT ->
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GT ->
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ELSE ->
          let _menhir_s = MenhirState069 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TRUE ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NUMBER _v ->
              _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | NIL ->
              _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LAMBDA ->
              _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IF ->
              _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ID _v ->
              _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FIX ->
              _menhir_run_022 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FALSE ->
              _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | E2 ->
              _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | E1 ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | CONS ->
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_066 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_IF as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer
      | THEN ->
          let _menhir_s = MenhirState067 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TRUE ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NUMBER _v ->
              _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | NIL ->
              _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LAMBDA ->
              _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IF ->
              _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ID _v ->
              _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FIX ->
              _menhir_run_022 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FALSE ->
              _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | E2 ->
              _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | E1 ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | SUB ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LT ->
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GT ->
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer
      | CONS ->
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_065 : type  ttv_stack ttv_result. (((((ttv_stack, ttv_result) _menhir_cell1_FUN, ttv_result) _menhir_cell1_bind, ttv_result) _menhir_cell1_bindlist, ttv_result) _menhir_cell1_expr as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer
      | SUB ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer
      | CONS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer
      | BAR | CCLEAR | CLET | CLOAD | COMMA | CPRINT | CSAVE | ELSE | END | EOF | IN | RPAREN | THEN | WITH ->
          let MenhirCell1_expr (_menhir_stack, _, e1) = _menhir_stack in
          let MenhirCell1_bindlist (_menhir_stack, _, xs) = _menhir_stack in
          let MenhirCell1_bind (_menhir_stack, _, x) = _menhir_stack in
          let MenhirCell1_FUN (_menhir_stack, _menhir_s) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_20 e1 e2 x xs in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_063 : type  ttv_stack ttv_result. ((((ttv_stack, ttv_result) _menhir_cell1_FUN, ttv_result) _menhir_cell1_bind, ttv_result) _menhir_cell1_bindlist as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer
      | SUB ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LT ->
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer
      | IN ->
          let _menhir_s = MenhirState064 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TRUE ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NUMBER _v ->
              _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | NIL ->
              _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LAMBDA ->
              _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IF ->
              _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ID _v ->
              _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FIX ->
              _menhir_run_022 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FALSE ->
              _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | E2 ->
              _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | E1 ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | GT ->
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer
      | CONS ->
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_055 : type  ttv_stack ttv_result. ((((((ttv_stack, ttv_result) _menhir_cell1_FUN, ttv_result) _menhir_cell1_REC, ttv_result) _menhir_cell1_bind, ttv_result) _menhir_cell1_bindlist, ttv_result) _menhir_cell1_expr as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer
      | SUB ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer
      | CONS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer
      | BAR | CCLEAR | CLET | CLOAD | COMMA | CPRINT | CSAVE | ELSE | END | EOF | IN | RPAREN | THEN | WITH ->
          let MenhirCell1_expr (_menhir_stack, _, e1) = _menhir_stack in
          let MenhirCell1_bindlist (_menhir_stack, _, bs) = _menhir_stack in
          let MenhirCell1_bind (_menhir_stack, _, x) = _menhir_stack in
          let MenhirCell1_REC (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_FUN (_menhir_stack, _menhir_s) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_19 bs e1 e2 x in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_053 : type  ttv_stack ttv_result. (((((ttv_stack, ttv_result) _menhir_cell1_FUN, ttv_result) _menhir_cell1_REC, ttv_result) _menhir_cell1_bind, ttv_result) _menhir_cell1_bindlist as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer
      | SUB ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LT ->
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer
      | IN ->
          let _menhir_s = MenhirState054 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TRUE ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NUMBER _v ->
              _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | NIL ->
              _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LAMBDA ->
              _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IF ->
              _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ID _v ->
              _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FIX ->
              _menhir_run_022 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FALSE ->
              _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | E2 ->
              _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | E1 ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | GT ->
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer
      | CONS ->
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_052 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_expr as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer
      | SUB ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer
      | CONS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer
      | BAR | CCLEAR | CLET | CLOAD | COMMA | CPRINT | CSAVE | ELSE | END | EOF | IN | RPAREN | THEN | WITH ->
          let MenhirCell1_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_13 _1 _3 in
          _menhir_goto_binop_expr_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_binop_expr_ : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let e = _v in
      let _v = _menhir_action_26 e in
      _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_050 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_expr as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer
      | SUB ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer
      | BAR | CCLEAR | CLET | CLOAD | COMMA | CONS | CPRINT | CSAVE | ELSE | END | EOF | EQ | GT | IN | LT | RPAREN | THEN | WITH ->
          let MenhirCell1_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_12 _1 _3 in
          _menhir_goto_binop_expr_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_048 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_expr as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer
      | SUB ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer
      | BAR | CCLEAR | CLET | CLOAD | COMMA | CONS | CPRINT | CSAVE | ELSE | END | EOF | EQ | GT | IN | LT | RPAREN | THEN | WITH ->
          let MenhirCell1_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_11 _1 _3 in
          _menhir_goto_binop_expr_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_046 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_expr as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer
      | SUB ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer
      | BAR | CCLEAR | CLET | CLOAD | COMMA | CONS | CPRINT | CSAVE | ELSE | END | EOF | EQ | GT | IN | LT | RPAREN | THEN | WITH ->
          let MenhirCell1_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_10 _1 _3 in
          _menhir_goto_binop_expr_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_044 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_expr as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer
      | BAR | CCLEAR | CLET | CLOAD | COMMA | CONS | CPRINT | CSAVE | ELSE | END | EOF | EQ | GT | IN | LT | PLUS | RPAREN | SUB | THEN | WITH ->
          let MenhirCell1_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_07 _1 _3 in
          _menhir_goto_binop_expr_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_042 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_expr as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer
      | BAR | CCLEAR | CLET | CLOAD | COMMA | CONS | CPRINT | CSAVE | ELSE | END | EOF | EQ | GT | IN | LT | PLUS | RPAREN | SUB | THEN | WITH ->
          let MenhirCell1_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_08 _1 _3 in
          _menhir_goto_binop_expr_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_040 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_expr -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_09 _1 _3 in
      _menhir_goto_binop_expr_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_038 : type  ttv_stack ttv_result. (((ttv_stack, ttv_result) _menhir_cell1_FIX, ttv_result) _menhir_cell1_bind as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer
      | SUB ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer
      | CONS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer
      | BAR | CCLEAR | CLET | CLOAD | COMMA | CPRINT | CSAVE | ELSE | END | EOF | IN | RPAREN | THEN | WITH ->
          let MenhirCell1_bind (_menhir_stack, _, x) = _menhir_stack in
          let MenhirCell1_FIX (_menhir_stack, _menhir_s) = _menhir_stack in
          let e = _v in
          let _v = _menhir_action_21 e x in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_037 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_E2 -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_E2 (_menhir_stack, _menhir_s) = _menhir_stack in
      let e = _v in
      let _v = _menhir_action_28 e in
      _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_033 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_E1 -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_E1 (_menhir_stack, _menhir_s) = _menhir_stack in
      let e = _v in
      let _v = _menhir_action_27 e in
      _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_028 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SND ->
          let _menhir_stack = MenhirCell1_term (_menhir_stack, _menhir_s, _v) in
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer
      | FST ->
          let _menhir_stack = MenhirCell1_term (_menhir_stack, _menhir_s, _v) in
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer
      | BAR | CCLEAR | CLET | CLOAD | COMMA | CONS | CPRINT | CSAVE | ELSE | END | EOF | EQ | FALSE | GT | ID _ | IN | LPAREN | LT | NIL | NUMBER _ | PLUS | RPAREN | SUB | THEN | TIMES | TRUE | WITH ->
          let _1 = _v in
          let _v = _menhir_action_03 _1 in
          _menhir_goto_app _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  let _menhir_run_000 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_expr_eof =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState000 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NUMBER _v ->
          _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | NIL ->
          _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LAMBDA ->
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IF ->
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ID _v ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FUN ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FIX ->
          _menhir_run_022 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FALSE ->
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | E2 ->
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | E1 ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  let _menhir_run_111 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_repl_command_eof =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState111 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NUMBER _v ->
          _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | NIL ->
          _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LAMBDA ->
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IF ->
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ID _v ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FUN ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FIX ->
          _menhir_run_022 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FALSE ->
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | E2 ->
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | E1 ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CSAVE ->
          _menhir_run_112 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CPRINT ->
          _menhir_run_114 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CLOAD ->
          _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CLET ->
          _menhir_run_117 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CCLEAR ->
          _menhir_run_121 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  let _menhir_run_127 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_script_eof =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | CSAVE ->
          _menhir_run_112 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState127
      | CPRINT ->
          _menhir_run_114 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState127
      | CLOAD ->
          _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState127
      | CLET ->
          _menhir_run_117 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState127
      | CCLEAR ->
          _menhir_run_121 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState127
      | EOF ->
          let _v = _menhir_action_35 () in
          _menhir_run_129 _menhir_stack _v
      | _ ->
          _eRR ()
  
end

let script_eof =
  fun _menhir_lexer _menhir_lexbuf ->
    let _menhir_stack = () in
    let MenhirBox_script_eof v = _menhir_run_127 _menhir_stack _menhir_lexbuf _menhir_lexer in
    v

let repl_command_eof =
  fun _menhir_lexer _menhir_lexbuf ->
    let _menhir_stack = () in
    let MenhirBox_repl_command_eof v = _menhir_run_111 _menhir_stack _menhir_lexbuf _menhir_lexer in
    v

let expr_eof =
  fun _menhir_lexer _menhir_lexbuf ->
    let _menhir_stack = () in
    let MenhirBox_expr_eof v = _menhir_run_000 _menhir_stack _menhir_lexbuf _menhir_lexer in
    v
