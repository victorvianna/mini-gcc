open Rtltree

exception Error of string

let graph = ref Label.M.empty

let generate i =
  let l = Label.fresh () in
  graph := Label.M.add l i !graph;
  l

(* get register bound to a variable name  *)
let get_var_register = ref (Hashtbl.create 10 : (Ttree.ident, register) Hashtbl.t)

(* allocate and bind register to a declared variable, return register used *)
let allocate_variable (decl_var:Ttree.decl_var) =
  let r = Register.fresh () in
  let (_, name) = decl_var in
  Hashtbl.add !get_var_register name r;
  r

let rec naive_apply_binop op (e1:Ttree.expr) (e2:Ttree.expr) destr destl =
  let r = Register.fresh () in
  let destl = generate (Embinop(op, r, destr, destl)) in
  let destl = expr e2 r destl in
  let destl = expr e1 destr destl in
  destl

and

  apply_binop_two_consts (op:Ttree.binop) c1 c2 destr destl =
  let int32_of_bool b =
    Int32.of_int (if b then 1 else 0) in
  match op with
  | Beq -> generate (Econst(int32_of_bool (Int32.equal c1 c2), destr, destl))
  | Bneq -> generate (Econst(int32_of_bool (not (Int32.equal c1 c2)), destr, destl))
  | Blt -> generate (Econst(int32_of_bool (Int32.compare c1 c2 < 0), destr, destl))
  | Ble -> generate (Econst(int32_of_bool (Int32.compare c1 c2 <= 0), destr, destl))
  | Bgt -> generate (Econst(int32_of_bool (Int32.compare c1 c2 > 0), destr, destl))
  | Bge -> generate (Econst(int32_of_bool (Int32.compare c1 c2 >= 0), destr, destl))
  | Badd -> generate (Econst(Int32.add c1 c2, destr, destl))
  | Bsub -> generate (Econst(Int32.sub c1 c2, destr, destl))
  | Bmul -> generate (Econst(Int32.mul c1 c2, destr, destl))
  (* if dividend is 0, compile naively to fail during execution *)
  | Bdiv -> generate (Econst(Int32.div c1 c2, destr, destl))
  | Band -> generate (Econst(int32_of_bool(not (Int32.equal c1  0l) && not (Int32.equal c2  0l)), destr, destl))
  | Bor -> generate (Econst(int32_of_bool(not (Int32.equal c1  0l) || not (Int32.equal c2  0l)), destr, destl))

and

  apply_binop_one_const (op:Ttree.binop) (e:Ttree.expr) c destr destl =
  let destl =
    match op with
    | Badd -> generate(Emunop(Maddi c, destr, destl))
    | Beq -> generate(Emunop(Msetei c, destr, destl))
    | Bneq -> generate(Emunop(Msetnei c, destr, destl))
    | _ -> raise (Error "apply_binop_one_const applied to invalid operation")
  in
  expr e destr destl

and

  expr (e:Ttree.expr) (destr:register) (destl:label) : label = match e.expr_node with
  | Ttree.Econst e -> generate (Econst(e, destr, destl))
  | Ttree.Ebinop (op, e1, e2)  ->
    begin
      match (op, e1.expr_node, e2.expr_node) with
      | (op, (Ttree.Econst c1), (Ttree.Econst c2)) when op != Bdiv && not (Int32.equal c2 0l) ->
        apply_binop_two_consts op c1 c2 destr destl
      | (op, (Ttree.Econst c), e) when op = Badd || op = Beq || op = Bneq ->
        apply_binop_one_const op e2 c destr destl
      | (op, e, Ttree.Econst c) when op = Badd || op = Beq || op = Bneq ->
        apply_binop_one_const op e1 c destr destl
      | (Bsub, e, Ttree.Econst c) ->
        apply_binop_one_const Badd e1 (Int32.neg c) destr destl
      | (Beq, _, _) -> naive_apply_binop Msete e1 e2 destr destl
      | (Bneq, _, _) -> naive_apply_binop Msetne e1 e2 destr destl
      | (Blt, _, _) -> naive_apply_binop Msetl e1 e2 destr destl
      | (Ble, _, _) -> naive_apply_binop Msetle e1 e2 destr destl
      | (Bgt, _, _) -> naive_apply_binop Msetg e1 e2 destr destl
      | (Bge, _, _) -> naive_apply_binop Msetge e1 e2 destr destl
      | (Badd, _, _) -> naive_apply_binop Madd e1 e2 destr destl
      | (Bsub, _, _) -> naive_apply_binop Msub e1 e2 destr destl
      | (Bmul, _, _) -> naive_apply_binop Mmul e1 e2 destr destl
      | (Bdiv, _, _) -> naive_apply_binop Mdiv e1 e2 destr destl
      | (Band, _, _) ->
        (* convert to 0 or 1 *)
        let normalize = generate(Emunop(Msetnei Int32.zero, destr, destl)) in
        (* if e1 is false, we proceed, otherwise we calculate e2;
           we use test jz because FALSE is the significative value for && *)
        let calculate_second = expr e1 destr normalize in
        let testl = generate (Emubranch (Mjz, destr, normalize, calculate_second)) in
        expr e2 destr testl
      | (Bor, _, _) ->
        (* convert to 0 or 1 *)
        let normalize = generate(Emunop(Msetnei Int32.zero, destr, destl)) in
        (* if e1 is true, we proceed, otherwise we calculate e2;
           we use test jnz because TRUE is the significative value for || *)
        let calculate_second = expr e1 destr normalize in
        let testl = generate (Emubranch (Mjnz, destr, normalize, calculate_second)) in
        expr e2 destr testl
    end
  | Ttree.Eunop(Uminus, e) ->
    begin
      match e.expr_node with
      | Econst c -> generate(Econst ((Int32.neg c), destr, destl))
      (* cannot use apply_binop_one_const on the next line *)
      | _ ->
        let (zero_expr:Ttree.expr) = {expr_typ = Ttree.Tint; expr_node = (Ttree.Econst Int32.zero)} in
        naive_apply_binop Msub zero_expr e destr destl
    end
  | Eunop(Unot, e) ->
    begin
      match e.expr_node with
      | Econst c -> apply_binop_two_consts Beq 0l c destr destl
      | _ ->
        let destl = generate(Emunop(Msetei Int32.zero, destr, destl)) in
        expr e destr destl
    end
  | Ttree.Eaccess_local name ->
    let r = Hashtbl.find !get_var_register name in
    generate (Embinop (Mmov, r, destr, destl))
  | Ttree.Eaccess_field (e, f) ->
    begin
      match e.expr_typ with (Tstructp stru) ->
        let field_index = f.field_pos in
        let r_address = Register.fresh () in
        let destl = generate (Eload (r_address, field_index * Memory.word_size, destr, destl)) in
        expr e r_address destl
                          | _ -> raise (Error "tried to access field of non-struct")
    end
  | Ttree.Eassign_local (name, e) ->
    let r = Hashtbl.find !get_var_register name in
    (* assignment must have the same value as assigned *)
    let destl = generate (Embinop(Mmov, r, destr, destl)) in
    expr e r destl;
  | Ttree.Eassign_field (e_address, f, e_value) ->
    begin
      match e_address.expr_typ with (Tstructp stru) ->
        let field_index = f.field_pos in
        let r_address = Register.fresh () in
        let destl = generate (Estore (destr, r_address, field_index * Memory.word_size, destl)) in
        let destl = expr e_value destr destl in
        expr e_address r_address destl
                                  | _ -> raise (Error "tried to assign field of non-struct")
    end
  | Ttree.Ecall (name, expr_list) ->
    let new_registers = List.map (fun _ -> Register.fresh ()) expr_list in
    (* generate call *)
    let destl = generate (Ecall (destr, name, new_registers, destl)) in
    let combined_list = List.combine expr_list new_registers in
    let calculate_argument pair l = match pair with
        (e,r) -> expr e r l
    in
    (* calculate expressions passed, store them in newly-allocated registers *)
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
    (* backup original variable-register bindings *)
    let ancient_get_var_register = Hashtbl.copy !get_var_register in
    ignore(List.map allocate_variable decl_var_list);
    let entry = List.fold_right (fun s l -> stmt s l retr exitl ) stmt_list destl in
    (* restore original variable-register bindings *)
    get_var_register := ancient_get_var_register;
    entry
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
    let stmtl = stmt s gobackl retr exitl in
    let branchl = generate (Emubranch (Mjnz, r_check, stmtl, destl)) in
    let testl = expr e r_check branchl in
    graph := Label.M.add gobackl (Egoto(testl)) !graph;
    testl

let deffun (f:Ttree.decl_fun) =
  graph := Label.M.empty;
  let r = Register.fresh () in
  let l = Label.fresh () in
  (* bind registers for function arguments and local variables *)
  let fun_formals = List.map allocate_variable f.fun_formals in
  let decl_locals = (function (d, _) -> d) f.fun_body in
  let fun_locals = Register.S.of_list (List.map allocate_variable decl_locals) in
  (* remove decl_var for local variables, because they are already bound  *)
  let pruned_body = (fun (_,s) -> ([], s)) f.fun_body in
  (* calculate graph for function body *)
  let fun_entry = stmt (Ttree.Sblock(pruned_body)) l r l in
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


