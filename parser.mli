
(* The type of tokens. *)

type token = 
  | WHILE
  | VERTICALBARVERTICALBAR
  | STRUCT
  | STAR
  | SLASH
  | SIZEOF
  | SEMICOLON
  | RPAR
  | RETURN
  | RBRACE
  | PLUS
  | MINUS
  | LPAR
  | LBRACE
  | INTEGER of (int32)
  | INT
  | IF
  | IDENT of (string)
  | EQOP of (Ptree.binop)
  | EQ
  | EOF
  | ELSE
  | COMP of (Ptree.binop)
  | COMMA
  | BANG
  | ARROW
  | AMPERSANDAMPERSAND

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val file: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ptree.file)
