open Rtltree

let graph = ref Label.M.empty

let generate i =
  let l = Label.fresh () in
  graph := Label.M.add l i !graph;
  l

(* functions for instruction selection of arithmetic operations *)
let mk_eq (e1:Ttree.expr) (e2:Ttree.expr) destr destl = match e1.expr_node, e2.expr_node with
| Econst x1, Econst x2 ->
begin
  let r = Register.fresh () in
  destl = generate (Econst (x1, destr, destl));
  destl = generate (Econst (x2, r, destl));
  destl = generate (Embinop (Ops.Msete, r, destr, destl));
  destl
end
let mk_neq (e1:Ttree.expr) (e2:Ttree.expr) destr destl = match e1.expr_node, e2.expr_node with
| Econst x1, Econst x2 ->
begin
  let r = Register.fresh () in
  destl = generate (Econst (x1, destr, destl));
  destl = generate (Econst (x2, r, destl));
  destl = generate (Embinop (Ops.Msetne, r, destr, destl));
  destl
end
let mk_lt (e1:Ttree.expr) (e2:Ttree.expr) destr destl = match e1.expr_node, e2.expr_node with
| Econst x1, Econst x2 ->
begin
  let r = Register.fresh () in
  destl = generate (Econst (x1, destr, destl));
  destl = generate (Econst (x2, r, destl));
  destl = generate (Embinop (Ops.Msetl, r, destr, destl));
  destl
end
let mk_le (e1:Ttree.expr) (e2:Ttree.expr) destr destl = match e1.expr_node, e2.expr_node with
| Econst x1, Econst x2 ->
begin
  let r = Register.fresh () in
  destl = generate (Econst (x1, destr, destl));
  destl = generate (Econst (x2, r, destl));
  destl = generate (Embinop (Ops.Msetle, r, destr, destl));
  destl
end
let mk_gt (e1:Ttree.expr) (e2:Ttree.expr) destr destl = match e1.expr_node, e2.expr_node with
| Econst x1, Econst x2 ->
begin
  let r = Register.fresh () in
  destl = generate (Econst (x1, destr, destl));
  destl = generate (Econst (x2, r, destl));
  destl = generate (Embinop (Ops.Msetg, r, destr, destl));
  destl
end
let mk_ge (e1:Ttree.expr) (e2:Ttree.expr) destr destl = match e1.expr_node, e2.expr_node with
| Econst x1, Econst x2 ->
begin
  let r = Register.fresh () in
  destl = generate (Econst (x1, destr, destl));
  destl = generate (Econst (x2, r, destl));
  destl = generate (Embinop (Ops.Msetge, r, destr, destl));
  destl
end
let mk_add (e1:Ttree.expr) (e2:Ttree.expr) destr destl = match e1.expr_node, e2.expr_node with
| Econst x1, Econst x2 ->
begin
  let r = Register.fresh () in
  destl = generate (Econst (x1, destr, destl));
  destl = generate (Econst (x2, r, destl));
  destl = generate (Embinop (Ops.Madd, r, destr, destl));
  destl
end
let mk_sub (e1:Ttree.expr) (e2:Ttree.expr) destr destl = match e1.expr_node, e2.expr_node with
| Econst x1, Econst x2 ->
begin
  let r = Register.fresh () in
  destl = generate (Econst (x1, destr, destl));
  destl = generate (Econst (x2, r, destl));
  destl = generate (Embinop (Ops.Msub, r, destr, destl));
  destl
end
let mk_mul (e1:Ttree.expr) (e2:Ttree.expr) destr destl = match e1.expr_node, e2.expr_node with
| Econst x1, Econst x2 ->
begin
  let r = Register.fresh () in
  destl = generate (Econst (x1, destr, destl));
  destl = generate (Econst (x2, r, destl));
  destl = generate (Embinop (Ops.Mmul, r, destr, destl));
  destl
end
let mk_div (e1:Ttree.expr) (e2:Ttree.expr) destr destl = match e1.expr_node, e2.expr_node with
| Econst x1, Econst x2 ->
begin
  let r = Register.fresh () in
  destl = generate (Econst (x1, destr, destl));
  destl = generate (Econst (x2, r, destl));
  destl = generate (Embinop (Ops.Mdiv, r, destr, destl));  (* TODO: throw exception if x2 = 0 ? *)
  destl
end
let mk_and (e1:Ttree.expr) (e2:Ttree.expr) destr destl = failwith "TODO"
let mk_or (e1:Ttree.expr) (e2:Ttree.expr) destr destl = failwith "TODO"

let rec expr (e:Ttree.expr) destr destl = match e.expr_node with
  | Ttree.Econst e -> generate (Econst(e, destr, destl))
  | Ttree.Ebinop (op, e1, e2)  ->
    begin
      match op with
      | Beq -> mk_eq e1 e2 destr destl
      | Bneq -> mk_neq e1 e2 destr destl
      | Blt -> mk_lt e1 e2 destr destl
      | Ble -> mk_le e1 e2 destr destl
      | Bgt -> mk_gt e1 e2 destr destl
      | Bge -> mk_ge e1 e2 destr destl
      | Badd -> mk_add e1 e2 destr destl
      | Bsub -> mk_sub e1 e2 destr destl
      | Bmul -> mk_mul e1 e2 destr destl
      | Bdiv -> mk_div e1 e2 destr destl
      | Band -> mk_and e1 e2 destr destl
      | Bor -> mk_or e1 e2 destr destl
    end

let rec stmt (s:Ttree.stmt) destl retr exitl = match s with
  | Ttree.Sreturn e ->
    expr e retr exitl
  | Ttree.Sblock (decl_var_list, stmt_list) ->
      List.fold_right (fun s l -> stmt s l retr l ) stmt_list destl (* retr is not used in this case*)

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



