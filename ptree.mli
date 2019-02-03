
(* Arbres de syntaxe abstraite issus du parsing *)

type loc = Lexing.position * Lexing.position

type ident = { id: string; id_loc: loc }

type typ =
  | Tint
  | Tstructp of ident

type unop =
  | Unot | Uminus

type binop =
  | Beq | Bneq | Blt | Ble | Bgt | Bge | Badd | Bsub | Bmul | Bdiv
  | Band | Bor

type expr =
  { expr_node: expr_node;
    expr_loc : loc }

and expr_node =
  | Econst of int32
  | Eright of lvalue
  | Eassign of lvalue * expr
  | Eunop of unop * expr
  | Ebinop of binop * expr * expr
  | Ecall of ident * expr list
  | Esizeof of ident

and lvalue =
  | Lident of ident
  | Larrow of expr * ident

type decl_var = typ * ident

type stmt =
  { stmt_node: stmt_node;
    stmt_loc : loc }

and stmt_node =
  | Sskip
  | Sexpr of expr
  | Sif of expr * stmt * stmt
  | Swhile of expr * stmt
  | Sblock of block
  | Sreturn of expr

and block =
  decl_var list * stmt list

type decl_struct = ident * decl_var list

type decl_fun = {
  fun_typ : typ;
  fun_name : ident;
  fun_formals : decl_var list;
  fun_body : block
}

type decl =
  | Dstruct of decl_struct
  | Dfun of decl_fun

type file = decl list


