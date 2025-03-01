
(* The type of tokens. *)

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
  | NUMBER of (int)
  | NIL
  | MATCH
  | LT
  | LPAREN
  | LET
  | LAMBDA
  | IS
  | IN
  | IF
  | ID of (string)
  | GT
  | FUN
  | FST
  | FIX
  | FILE of (string)
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

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val script_eof: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Cmd.t list)

val repl_command_eof: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Cmd.t)

val expr_eof: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.expr)
