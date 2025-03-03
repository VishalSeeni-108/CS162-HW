open Base
open Lamp
open Ast

exception Timeout

(* run a function with specified timeout:
   https://discuss.ocaml.org/t/computation-with-time-constraint/5548/9 *)
let with_timeout timeout f =
  let _ =
    Stdlib.Sys.set_signal Stdlib.Sys.sigalrm
      (Stdlib.Sys.Signal_handle (fun _ -> raise Timeout))
  in
  ignore (Unix.alarm timeout);
  try
    let r = f () in
    ignore (Unix.alarm 0);
    r
  with e ->
    ignore (Unix.alarm 0);
    raise e

(* Unit test utilities *)
let texpr = Alcotest.testable Pretty.expr equal_expr
let tvars = Alcotest.testable Vars.pp Vars.equal
let tty = Alcotest.testable Pretty.ty Ast.equal_ty

let parse s =
  try Parser.Expr.parse_string s
  with _ -> Alcotest.fail ("Failed to parse: " ^ s)

let parse_file filename =
  try Parser.Expr.parse_file filename
  with _ -> Alcotest.fail ("Failed to parse file: " ^ filename)

let parse_ty s =
  try Parser.Ty.parse_string s
  with _ -> Alcotest.fail ("Failed to parse: " ^ s)

(** Helper function to check that the type checker abstractly evaluates 
  * the expression to type t *)
let check_well_typed ~gamma e t () =
  let tact =
    try Typecheck.abstract_eval gamma e
    with Typecheck.Type_error msg -> failwith ("Type error!\n" ^ msg)
  in
  Alcotest.(check' tty)
    ~msg:(Fmt.str "%a" Pretty.expr e)
    ~expected:t ~actual:tact

(** Helper function to check that the type checker abstractly evaluates 
  * the expression (given as a string) to type t *)
let check_well_typed_s ~gamma s t () =
  check_well_typed
    ~gamma:(List.map gamma ~f:(fun (x, t) -> (x, parse_ty t)))
    (parse s) (parse_ty t) ()

let check_well_typed_file s t () =
  check_well_typed ~gamma:[] (parse_file s) (parse_ty t) ()

(** Helper function to check that the type checker determines that the expression is
    ill-typed. *)
let check_ill_typed ~gamma s () =
  try
    let t = Typecheck.abstract_eval gamma (parse s) in
    Alcotest.fail ("abstractly evaluated to " ^ show_ty t)
  with Typecheck.Type_error _ -> ()

let well_typed_tests =
  [
    check_well_typed_s ~gamma:[] (* input expression *) "0"
      (* expected output type *) "Int";
    check_well_typed_s
      ~gamma:[ ("x", "Bool") ]
      (* input expression *) "x" (* expected output type *) "Bool";
    check_well_typed_s
      ~gamma:[]
      (* input expression *) "5 + 5" (* expected output type *) "Int";
    check_well_typed_s
      ~gamma:[]
      (* input expression *) "5 = 5" (* expected output type *) "Bool";
    check_well_typed_s
      ~gamma:[]
      (* input expression *) "if true then 1 else 2" (* expected output type *) "Int";
    check_well_typed_s
      ~gamma:[]
      (* input expression *) "if false then true else false" (* expected output type *) "Bool";
    check_well_typed_s
      ~gamma:[]
      (* input expression *) "lambda x: Bool.5" (* expected output type *) "Bool -> Int";
    check_well_typed_s
      ~gamma:[]
      (* input expression *) "(lambda x: Bool.5) true" (* expected output type *) "Int";
    check_well_typed_s
      ~gamma:[("x", "Int")]
      (* input expression *) "let x = 5 in true" (* expected output type *) "Bool";
    check_well_typed_s
      ~gamma:[]
      (* input expression *) "Nil[Bool]" (* expected output type *) "List[Bool]";
    check_well_typed_s
      ~gamma:[]
      (* input expression *) "1::2::3::Nil[Int]" (* expected output type *) "List[Int]";
      check_well_typed_s
      ~gamma:[]
      (* input expression *) "match (1::2::3::Nil[Int]) with Nil -> 1 | x::xs -> 0 end" (* expected output type *) "Int";
    check_well_typed_file "examples/fib.lp" "Int";
    check_well_typed_file "examples/add_n.lp" "List[Int]";
  ]

let ill_typed_tests =
  [ check_ill_typed ~gamma:[] (* input expression *) "1 + true"; 
    check_ill_typed ~gamma:[] (* input expression *) "if 1 then 1 else 2";
    check_ill_typed ~gamma:[] (* input expression *) "(lambda x: Bool.5) 2";
    check_ill_typed ~gamma:[] (* input expression *) "let x = true in x + 2";
    check_ill_typed ~gamma:[] (* input expression *) "1::false::3::Nil[Int]"; ]

let tests = [ ("well_typed", well_typed_tests); ("ill_typed", ill_typed_tests) ]
