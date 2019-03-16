open Rtltree

exception Error of string

(* hash table with physical equality *)
module H = Hashtbl.Make(struct
    type t = Ttree.expr
    let equal = (==)
    let hash = Hashtbl.hash
  end)

let graph = ref Label.M.empty

(*
Hashtbl for veryfing if an expression has been precalculated.
It maps a Ttree.expr node to Some(c) if it evaluates to c,
or None if we have already attempted to precompute but cannot
do it in compilation time (e.g. it involves a variable value)
*)
let precalculated = H.create 10

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

let was_precalculated e =
  (H.mem precalculated e) && (H.find precalculated e != None)

let compute_binop_csts (op:Ttree.binop) c1 c2 =
  let int32_of_bool b = Int32.of_int (if b then 1 else 0) in
  match op with
  | Beq -> int32_of_bool (Int32.equal c1 c2)
  | Bneq -> int32_of_bool (not (Int32.equal c1 c2))
  | Blt -> int32_of_bool (Int32.compare c1 c2 < 0)
  | Ble -> int32_of_bool (Int32.compare c1 c2 <= 0)
  | Bgt -> int32_of_bool (Int32.compare c1 c2 > 0)
  | Bge -> int32_of_bool (Int32.compare c1 c2 >= 0)
  | Badd -> Int32.add c1 c2
  | Bsub -> Int32.sub c1 c2
  | Bmul -> Int32.mul c1 c2
  | Bdiv -> Int32.div c1 c2
  | Band -> int32_of_bool(not (Int32.equal c1  0l) && not (Int32.equal c2  0l))
  | Bor -> int32_of_bool(not (Int32.equal c1  0l) || not (Int32.equal c2  0l))


let compute_unop_cst (op:Ttree.unop) c = match op with
  | Uminus -> compute_binop_csts Bsub 0l c
  | Unot -> compute_binop_csts Beq 0l c


let get_precalc_value e =
  match (H.find precalculated e) with
  | Some (c) -> c
  | _ -> raise (Error "get_precalc_value of non precalculated")

(* translate precalculated expression to single constant node *)
let translate_precalculated e destr destl =
  let c = get_precalc_value e in
  generate(Econst(c, destr, destl))

(* traverse tree (only once in whole program) and attempt to precalculate *)
let rec attempt_precalculate (e:Ttree.expr) =
  let is_div_by_zero (e:Ttree.expr) = match e.expr_node with
    | Ebinop(op, e1, e2) -> (was_precalculated e2) && (get_precalc_value e2 = 0l) && op = Bdiv
    | _ -> false
  in
  if not (H.mem precalculated e) then
    match e.expr_node with
    | Econst c -> H.add precalculated e (Some c);
    | Ebinop (op, e1, e2) ->
      attempt_precalculate e1;
      attempt_precalculate e2;
      (* we let 0 division fail during running time *)
      (* TODO: warn programmer *)
      if (was_precalculated e1) && (was_precalculated e2) && not (is_div_by_zero e) then
        let c1 = get_precalc_value e1 in
        let c2 = get_precalc_value e2 in
        H.add precalculated e (Some (compute_binop_csts op c1 c2))
      else H.add precalculated e None
    | Eunop (op, e2) ->
      attempt_precalculate e2;
      if (was_precalculated e2) then
        let c2 = get_precalc_value e2 in
        H.add precalculated e (Some(compute_unop_cst op c2))
      else H.add precalculated e None
    | _ -> H.add precalculated e None

let rec naive_translate_binop op (e1:Ttree.expr) (e2:Ttree.expr) destr destl =
  let r = Register.fresh () in
  let destl = generate (Embinop(op, r, destr, destl)) in
  let destl = expr e2 r destl in
  let destl = expr e1 destr destl in
  destl

and

  (* evalulate addition and (in)equality with constant values *)
  translate_unary_version_binop (op:Ttree.binop) c destr destl =
  match op with
  | Badd -> generate(Emunop(Maddi c, destr, destl))
  | Beq -> generate(Emunop(Msetei c, destr, destl))
  | Bneq -> generate(Emunop(Msetnei c, destr, destl))
  | _ -> raise (Error "translate_binop_one_precalc applied to invalid operation")

and

  (* translate binop when exactly one expression was precalculated *)
  translate_binop_one_precalc (op:Ttree.binop) (e1:Ttree.expr) (e2:Ttree.expr) destr destl =
  let precalc = if (was_precalculated e1) then e1 else e2 in
  let tocalc = if (precalc = e1) then e2 else e1 in
  let c = get_precalc_value precalc in
  let destl = translate_unary_version_binop op c destr destl  in
  expr tocalc destr destl

and

  expr (e:Ttree.expr) (destr:register) (destl:label) : label =
  if not (was_precalculated e) then attempt_precalculate e else ();
  match e.expr_node with
  | _ when was_precalculated e ->
    let c = get_precalc_value e in
    generate (Econst(c, destr, destl))
  | Ttree.Ebinop (op, e1, e2)  ->
    begin
      match (op, e1.expr_node, e2.expr_node) with
      | _ when (op = Badd || op = Beq || op = Bneq) && ((was_precalculated e1) || was_precalculated e2)->
        translate_binop_one_precalc op e1 e2 destr destl
      | _ when op = Bsub && was_precalculated e2 ->
        let c2 =  get_precalc_value e2 in
        let destl = translate_unary_version_binop Badd (Int32.neg c2) destr destl in
        expr e1 destr destl
      | (Beq, _, _) -> naive_translate_binop Msete e1 e2 destr destl
      | (Bneq, _, _) -> naive_translate_binop Msetne e1 e2 destr destl
      | (Blt, _, _) -> naive_translate_binop Msetl e1 e2 destr destl
      | (Ble, _, _) -> naive_translate_binop Msetle e1 e2 destr destl
      | (Bgt, _, _) -> naive_translate_binop Msetg e1 e2 destr destl
      | (Bge, _, _) -> naive_translate_binop Msetge e1 e2 destr destl
      | (Badd, _, _) -> naive_translate_binop Madd e1 e2 destr destl
      | (Bsub, _, _) -> naive_translate_binop Msub e1 e2 destr destl
      | (Bmul, _, _) -> naive_translate_binop Mmul e1 e2 destr destl
      | (Bdiv, _, _) -> naive_translate_binop Mdiv e1 e2 destr destl
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
  | Ttree.Eunop(op, e) ->
    begin
      match (op, e.expr_node) with
      | _ when was_precalculated e ->
        translate_precalculated e destr destl
      | (Uminus, _) ->
        let (zero_expr:Ttree.expr) = {expr_typ = Ttree.Tint; expr_node = (Ttree.Econst 0l)} in
        naive_translate_binop Msub zero_expr e destr destl
      | (Unot, _) ->
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
  | Ttree.Econst c -> raise (Error "constant expr was not precalculated")

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


