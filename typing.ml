
open Ttree

(* utiliser cette exception pour signaler une erreur de typage *)
exception Error of string

(* hash table for checking if a variable has been declared in the current scope *)
let hasht = (Hashtbl.create 10 : (string, Ttree.typ) Hashtbl.t)

let string_of_type =
  begin
  	function
	  | Tint       -> "int"
	  | Tstructp x -> "struct " ^ x.str_name ^ " *"
	  | Tvoidstar  -> "void*"
	  | Ttypenull  -> "typenull"
  end

let convert_fun_typ = function
	| Ptree.Tint -> Tint
	| Ptree.Tstructp {id = id; id_loc = id_loc} -> (* try *) Hashtbl.find hasht id (* with exception _ -> raise Error "non defined variable in ... (TODO)"   *)
(* 
let add_declaration ident:string typ: (:) = 
	Hashtbl.add hasht ident  *)
(*
let convert_fun_name  fun_name = 


let convert_fun_formals  fun_formals = 


let convert_fun_body  fun_body = 
 *)

let convert_dfun (dfun : Ptree.decl_fun) : Ttree.decl_fun = 
	{
		fun_typ = convert_fun_typ dfun.fun_typ; 
		(* fun_name = convert_fun_name dfun.fun_name; 
		fun_formals = convert_fun_formals dfun.fun_formals; 
		fun_body = convert_fun_body dfun.fun_body *)
		fun_name = "fodase"; 
		fun_formals = []; 
		fun_body = [] , []
	}


let rec program p = match p with
  | Ptree.Dfun dfun :: tail -> { funs = (convert_dfun dfun) :: (program tail).funs }
  | (Ptree.Dstruct x) :: tail -> (* add to hash table; *) program tail 
  | _ -> { funs = [] } (* Empty file *)

