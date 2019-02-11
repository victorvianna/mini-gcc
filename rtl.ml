open Rtltree

let graph = ref Label.M.empty

let generate i =
  let l = Label.fresh () in
  graph := Label.M.add l i !graph;
  l

let rec expr (e:Ttree.expr) destr destl = match e.expr_node with
  | Ttree.Econst e -> generate (Econst(e, destr, destl))  


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



