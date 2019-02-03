
(** arbres issus du typage *)

type ident = string

type typ =
  | Tint                  (* int *)
  | Tstructp of structure (* struct S * *)
  | Tvoidstar             (* void * *)
  | Ttypenull             (* le type donné à la constante 0 *)

and structure = {
  str_name  : ident;
  str_fields: (ident, field) Hashtbl.t;
  (* on pourra ajouter plus tard ici la taille totale de la structure *)
}

and field = {
  field_name: string;
  field_typ : typ;
  (* on pourra ajouter plus tard ici la position du champ dans la structure *)
}

type unop = Ptree.unop

type binop = Ptree.binop

type decl_var = typ * ident

type expr = {
  expr_node: expr_node;
  expr_typ : typ        (* chaque expression est décorée par son type *)
}

and expr_node =
  | Econst of int32
  | Eaccess_local of ident
  | Eaccess_field of expr * field
  | Eassign_local of ident * expr
  | Eassign_field of expr * field * expr
  | Eunop of unop * expr
  | Ebinop of binop * expr * expr
  | Ecall of ident * expr list
  | Esizeof of structure

type stmt =
  | Sskip
  | Sexpr of expr
  | Sif of expr * stmt * stmt
  | Swhile of expr * stmt
  | Sblock of block
  | Sreturn of expr

and block =
  decl_var list * stmt list

and decl_fun = {
  fun_typ    : typ;
  fun_name   : ident;
  fun_formals: decl_var list;
  fun_body   : block
}

type file = {
  funs: decl_fun list;
}


