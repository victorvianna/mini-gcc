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
      let calculate_second = expr e2 destr destl in
      let testl = generate (Emubranch (Mjz, destr, destl, calculate_second)) in
      expr e1 destr testl
      | Bor ->
      (* if e1 is true, result is e1 and we proceed to destl directly;
      otherwise result is e2, we calculate it then go to destl;
      we use test jnz because TRUE is the significative value for || *)
      let calculate_second = expr e2 destr destl in
      let testl = generate (Emubranch (Mjnz, destr, destl, calculate_second)) in
      expr e1 destr testl
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
    let (v:variable) = Hashtbl.find get_memo_var name in
    match v with
    | (r, Ttree.Tint) -> generate (Eload (r, 0, destr, destl))
    | (_, _) -> failwith "TODO: support access of structures"
  end
  | Ttree.Eassign_local (name, e) ->
  begin
    let (v:variable) = Hashtbl.find get_memo_var name in
    match v with
    | (r, Ttree.Tint) ->
    let destl = generate (Estore (r, destr, 0, destl)) in (* assignment has same value as assigned *)
    expr e r destl;
    | (_, _) -> failwith "TODO: support assignment of structures"
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



