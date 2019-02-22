open Rtltree

let graph = ref Label.M.empty

let generate i =
  let l = Label.fresh () in
  graph := Label.M.add l i !graph;
  l

(* table for local variable access: gets register and type  *)
type var_info = register * (Ttree.typ)
let get_var_info = ref (Hashtbl.create 10 : (string, var_info) Hashtbl.t)

(* table for function info access *)
type fun_info = {
  args: Ttree.decl_var list
}
let get_fun_info = (Hashtbl.create 10 : (string, fun_info) Hashtbl.t)

(* attribute register to certain identifier *)
let attribute_register (decl_var:Ttree.decl_var) register = match decl_var with
  (typ, name) -> Hashtbl.add !get_var_info name (register,typ)

(* allocate register when variable is declared, return register used *)
let allocate_variable (decl_var:Ttree.decl_var) =
  let r = Register.fresh () in
  attribute_register decl_var r;
  r

(* function to (deterministically) get the displacement of a field in memo *)
let get_displacement (stru:Ttree.structure) (field:Ttree.field) =
  let index_of e l =
    let rec index_rec i = function
      | [] -> raise Not_found
      | hd::tl -> if hd = e then i else index_rec (i+1) tl
    in
    index_rec 0 l
  in
  let fields_list = Hashtbl.fold (fun k v acc -> v :: acc) stru.str_fields [] in
  index_of field fields_list

let rec naive_apply_binop op (e1:Ttree.expr) (e2:Ttree.expr) destr destl =
  let r = Register.fresh () in
  let destl = generate (Embinop(op, r, destr, destl)) in
  let destl = expr e2 r destl in
  let destl = expr e1 destr destl in
  destl

and

expr (e:Ttree.expr) (destr:register) (destl:label) : label = match e.expr_node with
  | Ttree.Econst e -> generate (Econst(e, destr, destl))
  | Ttree.Ebinop (op, e1, e2)  ->
    begin
      match op with
      | Beq -> naive_apply_binop Msete e1 e2 destr destl
      | Bneq -> naive_apply_binop Msetne e1 e2 destr destl
      | Blt -> naive_apply_binop Msetl e1 e2 destr destl
      | Ble -> naive_apply_binop Msetle e1 e2 destr destl
      | Bgt -> naive_apply_binop Msetg e1 e2 destr destl
      | Bge -> naive_apply_binop Msetge e1 e2 destr destl
      | Badd -> naive_apply_binop Madd e1 e2 destr destl
      | Bsub -> naive_apply_binop Msub e1 e2 destr destl
      | Bmul -> naive_apply_binop Mmul e1 e2 destr destl
      | Bdiv -> naive_apply_binop Mdiv e1 e2 destr destl
      | Band ->
      (* if e1 is false, result is e1 and we proceed to destl directly;
      otherwise result is e2, we calculate it then go to destl;
      we use test jz because FALSE is the significative value for && *)
      let calculate_second = expr e1 destr destl in
      let testl = generate (Emubranch (Mjz, destr, destl, calculate_second)) in
      expr e2 destr testl
      | Bor ->
      (* if e1 is true, result is e1 and we proceed to destl directly;
      otherwise result is e2, we calculate it then go to destl;
      we use test jnz because TRUE is the significative value for || *)
      let calculate_second = expr e1 destr destl in
      let testl = generate (Emubranch (Mjnz, destr, destl, calculate_second)) in
      expr e2 destr testl
    end
  | Ttree.Eunop (op, e)  ->
    begin
      match op with
      | Uminus ->
      let (zero_expr:Ttree.expr) = {expr_typ = Ttree.Tint; expr_node = (Ttree.Econst Int32.zero)} in
      naive_apply_binop Msub zero_expr e destr destl
      | Unot ->
      (* TODO: avoid overflow and optimize  *)
      let (zero_expr:Ttree.expr) = {expr_typ = Ttree.Tint; expr_node = (Ttree.Econst Int32.zero)} in
      let sub_one = Ops.Maddi (Int32.of_int (-1)) in
      let destl = generate (Emunop (sub_one, destr, destl)) in
      let destl = naive_apply_binop Msub zero_expr e destr destl in
      expr e destr destl
    end
  | Ttree.Eaccess_local name ->
  begin
    let (v:var_info) = Hashtbl.find !get_var_info name in
    match v with
    | (r, _) -> generate (Embinop (Mmov, r, destr, destl))
  end
  | Ttree.Eaccess_field (e, f) ->
  begin
    match e.expr_typ with (Tstructp stru) ->
    let displacement = get_displacement stru f in
    let r_address = Register.fresh () in
    let destl = generate (Eload (r_address, displacement, destr, destl)) in
    expr e r_address destl
  end
  | Ttree.Eassign_local (name, e) ->
  begin
    let (v:var_info) = Hashtbl.find !get_var_info name in
    match v with
    | (r, _) ->
    let destl = generate (Embinop(Mmov, r, destr, destl)) in (* assignment has same value as assigned *)
    expr e r destl;
  end
  | Ttree.Eassign_field (e_address, f, e_value) ->
  begin
    match e_address.expr_typ with (Tstructp stru) ->
    let displacement = get_displacement stru f in
    let r_address = Register.fresh () in
    let destl = generate (Estore (destr, r_address, displacement, destl)) in
    let destl = expr e_value destr destl in
    expr e_address r_address destl
  end
  | Ttree.Ecall (name, expr_list) ->
  (* backup original variable registers *)
  let ancient_get_var_info = Hashtbl.copy !get_var_info in
  (* associate registers with argument names  *)
  let (fun_info:fun_info) = Hashtbl.find get_fun_info name in
  let new_registers = List.map allocate_variable fun_info.args in
  (* generate call *)
  let destl = generate (Ecall (destr, name, new_registers, destl)) in
  (* store calculated arguments in new registers *)
  let combined_list = List.combine expr_list new_registers in
  let calculate_argument pair l = match pair with
    (e,r) -> expr e r l
  in
  (* restore original variable registers *)
  get_var_info := ancient_get_var_info;
  let entry = List.fold_right calculate_argument combined_list destl in
  entry
  | Ttree.Esizeof structure ->
    let num_words = Int32.of_int ((Hashtbl.length structure.str_fields) * Memory.word_size) in
    generate (Econst(num_words, destr, destl))

let rec stmt (s:Ttree.stmt) destl retr exitl = match s with
  | Ttree.Sreturn e ->
    expr e retr exitl
  | Ttree.Sexpr e ->
    expr e retr destl
  | Ttree.Sblock (decl_var_list, stmt_list) ->
  begin
    (* backup original variable registers *)
    let ancient_get_var_info = Hashtbl.copy !get_var_info in
    List.map allocate_variable decl_var_list;
    let entry = List.fold_right (fun s l -> stmt s l retr l ) stmt_list destl in (* retr is not used in this case*)
    (* restore original variable registers *)
    get_var_info := ancient_get_var_info;
    entry
  end
  | Ttree.Sskip -> destl
  | Ttree.Sif (e, s1, s2) ->
  let truel = stmt s1 destl retr exitl in
  let falsel = stmt s2 destl retr exitl in
  let r_check = Register.fresh () in
  let testl = generate (Emubranch (Mjnz, r_check, truel, falsel)) in
  expr e r_check testl
  | Ttree.Swhile (e, s) ->
  let r_check = Register.fresh() in
  let gobackl = Label.fresh () in
  let testl = generate (Emubranch (Mjnz, r_check, gobackl, destl)) in
  graph := Label.M.add gobackl (Egoto(testl)) !graph;
  expr e r_check testl

let deffun (f:Ttree.decl_fun) =
  let r = Register.fresh () in
  let l = Label.fresh () in
  let get_name decl_var = match decl_var with
    (_, name) -> name
  in
  let fun_info = {
    args = f.fun_formals
  }
  in
  Hashtbl.add get_fun_info f.fun_name fun_info;
  let fun_formals = List.map allocate_variable f.fun_formals in
  let fun_locals = Register.S.of_list ((function (d, s) -> List.map allocate_variable d) f.fun_body) in
  (* calculate graph for function body *)
  let fun_entry = stmt (Ttree.Sblock(f.fun_body)) l r l in
  {
    fun_name = f.fun_name;
    fun_result = r;
    fun_exit = l;
    fun_entry = fun_entry;
    fun_formals = fun_formals;
    fun_locals = fun_locals;
    fun_body = !graph;
  }

let program (ttree:Ttree.file) =
  {funs = List.map deffun ttree.funs}



