open Rtltree

let graph = ref Label.M.empty

let generate i =
  let l = Label.fresh () in
  graph := Label.M.add l i !graph;
  l

(* table for local variable access: gets register and type  *)
type variable = register * Ttree.typ
let get_memo_var = (Hashtbl.create 10 : (string, variable) Hashtbl.t)

let rec naive_apply_binop op (e1:Ttree.expr) (e2:Ttree.expr) destr destl =
  let r = Register.fresh () in
  let destl = expr e1 destr destl in
  let destl = expr e2 r destl in
  let destl = generate (Embinop(op, r, destr, destl)) in
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
      | Band -> failwith "TODO"
      | Bor -> failwith "TODO"
    end
  | Ttree.Eunop (op, e)  ->
    begin
      match op with
      | Uminus ->
      let (zero_expr:Ttree.expr) = {expr_typ = Ttree.Tint; expr_node = (Ttree.Econst Int32.zero)} in
      naive_apply_binop Msub zero_expr e destr destl
      | Unot ->
      failwith "TODO"
      (* let (not_expr:Ttree.expr) = {expr_typ = Ttree.Tint; expr_node = (Ttree.Ebinop e )} *)
    end
  | Ttree.Eaccess_local name ->
  begin
    let (v:variable) = Hashtbl.find get_memo_var name in
    match v with
    | (r, Ttree.Tint) -> generate (Eload (r, 0, destr, destl))
    | (_, _) -> failwith "TODO: support access of structures"
  end

let rec stmt (s:Ttree.stmt) destl retr exitl = match s with
  | Ttree.Sreturn e ->
    expr e retr exitl
  | Ttree.Sexpr e ->
    expr e retr destl
  | Ttree.Sblock (decl_var_list, stmt_list) ->
  begin
    let allocate_variable (decl_var:Ttree.decl_var) = match decl_var with
    | Ttree.Tint, name -> Hashtbl.add get_memo_var name (Register.fresh (),Ttree.Tint)
    | _, _ -> failwith "TODO: support allocation of structures"
    in
    List.iter allocate_variable decl_var_list;
    List.fold_right (fun s l -> stmt s l retr l ) stmt_list destl (* retr is not used in this case*)
  end
  | Ttree.Sskip -> destl

let deffun (f:Ttree.decl_fun) =
  let r = Register.fresh () in
  let l = Label.fresh () in
  let fun_entry = stmt (Ttree.Sblock(f.fun_body)) l r l in
  {
    fun_name = f.fun_name;
    fun_result = r;
    fun_exit = l;
    fun_entry = fun_entry;
    fun_formals = [];
    fun_locals = Register.S.empty;
    fun_body = !graph;
  }

let program (ttree:Ttree.file) =
  {funs = List.map deffun ttree.funs}



