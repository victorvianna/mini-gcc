open Ttree

(* utiliser cette exception pour signaler une erreur de typage *)
exception Error of string

(* contains the set of defined local variables *)
module StringSet = Set.Make(String)

let struct_map = Hashtbl.create 10 (* Ttree.ident -> Ttree.structure *)
let var_map = ref (Hashtbl.create 10) (* Ttree.ident -> Ttree.decl_var *)
let fun_map = Hashtbl.create 10 (* Ttree.ident -> Ttree.decl_fun *)

let get_fun_name (dfun : Ptree.decl_fun) =
    dfun.fun_name.id

let smpl_hdr_msg (f : Lexing.position) (s : Lexing.position) =
    Printf.sprintf "line %d, characters %d-%d"
        f.pos_lnum f.pos_cnum s.pos_cnum

let cmplx_hdr_msg (f : Lexing.position) (s : Lexing.position) =
    Printf.sprintf "from line %d, character %d to line %d character %d"
        f.pos_lnum f.pos_cnum s.pos_lnum s.pos_cnum

let get_error_hdr_msg (l : Ptree.loc) =
    let f, s = l in
    if f.pos_lnum = s.pos_lnum
    then smpl_hdr_msg f s
    else cmplx_hdr_msg f s

let get_error_message msg l =
    let hdr = get_error_hdr_msg l in
    Printf.sprintf "%s: %s" hdr msg

let raise_error msg (l : Ptree.loc) =
    let msg = get_error_message msg l in
    raise (Error msg)

let add_fun_putchar () =
    let fun_decl = 
        {
            fun_typ = Tint;
            fun_name = "putchar";
            fun_formals = [Tint, "c"];
            fun_body = [], [];
        } in
    Hashtbl.add fun_map "putchar" fun_decl

let add_fun_sbrk () =
    let fun_decl = 
        {
            fun_typ = Tvoidstar;
            fun_name = "sbrk";
            fun_formals = [Tint, "n"];
            fun_body = [], [];
        } in
    Hashtbl.add fun_map "sbrk" fun_decl

let get_fun_typ (dfun : Ptree.decl_fun) : typ =
    match (dfun.fun_typ : Ptree.typ) with
    | Ptree.Tint -> Tint
    | Ptree.Tstructp id -> Tstructp (Hashtbl.find struct_map id.id)
    | _ -> raise (Error "Wrong function return type")

let get_decl_var (decl: Ptree.decl_var) =
    match decl with
        | Ptree.Tint, i -> Tint, i
        | Ptree.Tstructp st, i-> 
                let st = 
                    try Hashtbl.find struct_map st.id
                    with Not_found -> raise_error "Undefined struct type"
                    st.id_loc
                in
                Tstructp st, i

let get_decl_list (dlist : Ptree.decl_var list) =
    let rec aux acc var_set = function
        | [] -> List.rev acc
        | decl :: tail -> 
                let dvar = get_decl_var decl in
                let var_typ = fst dvar in
                let var_name = snd dvar in
                if StringSet.mem var_name.id var_set
                    then raise_error "Variable redefinition" var_name.id_loc
                    else
                if Hashtbl.mem fun_map var_name.id
                    then raise_error "Function with same name exists"
                        var_name.id_loc
                    else 
                if Hashtbl.mem struct_map var_name.id
                    then raise_error "Struct definition with same name exists"
                        var_name.id_loc
                    else
                Hashtbl.add !var_map var_name.id dvar;
                aux ((var_typ, var_name.id) :: acc) (StringSet.add var_name.id
                var_set) tail
    in
    aux [] (StringSet.empty) dlist

let get_fun_formals (dfun : Ptree.decl_fun) =
    get_decl_list dfun.fun_formals

let equiv_types t1 t2 = match t1, t2 with
    | Tint, Tint | Ttypenull, Ttypenull | Tvoidstar, Tvoidstar -> true
    | Tstructp t1, Tstructp t2 -> t1.str_name = t2.str_name
    | Ttypenull, Tint | Tint, Ttypenull 
    | Ttypenull, Tstructp _ | Tstructp _, Ttypenull 
    | Tvoidstar, Tstructp _ | Tstructp _, Tvoidstar -> true
    | _, _ -> false

(* returns the field of a structure *)
let get_str_field sname (fid : Ptree.ident) =
    let sfield = fid.id in
    let str = Hashtbl.find struct_map sname in
    if not (Hashtbl.mem str.str_fields sfield)
        then let message =
            Printf.sprintf "variable \"%s\" does not contain field \"%s\"" sname fid.id
        in raise_error message fid.id_loc
    else Hashtbl.find str.str_fields sfield

(* continuar *)
let rec get_expr (e : Ptree.expr) =
    match e.expr_node with
    | Ptree.Econst _  -> get_expr_const  e
    | Ptree.Eright _  -> get_expr_right  e
    | Ptree.Eassign _ -> get_expr_assign e
    | Ptree.Eunop _   -> get_expr_unop   e
    | Ptree.Ebinop _  -> get_expr_binop  e
    | Ptree.Ecall _   -> get_expr_call   e
    | Ptree.Esizeof _ -> get_expr_sizeof e
and
get_expr_const (e : Ptree.expr) =
    let Ptree.Econst i = e.expr_node in
    let expr_type = if i = 0l then Ttypenull else Tint in
    {expr_node = Econst i; expr_typ = expr_type}
and
get_expr_unop (e : Ptree.expr) =
    let Ptree.Eunop (op, e1) = e.expr_node in
    let e2 = get_expr e1 in
    match op with
    | Ptree.Uminus ->
            if equiv_types e2.expr_typ Tint
            then {expr_node = Eunop (op, e2); expr_typ = Tint}
            else raise (Error "Wrong operand type")
    | Ptree.Unot -> {expr_node = Eunop (op, e2); expr_typ = Tint}
and
get_expr_assign (e : Ptree.expr) =
    let Ptree.Eassign(l, e1) = e.expr_node in
    let e2 = get_expr e1 in
    let e2 =
        begin
            match e2.expr_node with
            | Econst (0l) -> {expr_node = e2.expr_node; expr_typ = Ttypenull}
            | _ -> e2
        end in
    match l with
    | Ptree.Lident id ->
        let var_typ, _ = Hashtbl.find !var_map id.id in
        if equiv_types e2.expr_typ var_typ
        then {expr_node = Eassign_local (id.id, e2); expr_typ = var_typ}
        else raise (Error "Invalid assignment: invalid assigned type")
    | Ptree.Larrow (e3, id) ->
        let e4 = get_expr e3 in
        begin match e4.expr_typ with
            | Tstructp s ->
                let sf = Hashtbl.find s.str_fields id.id in
                if equiv_types e2.expr_typ sf.field_typ
                    then
                        {
                            expr_node = Eassign_field (e4, sf, e2);
                            expr_typ = sf.field_typ
                        }
                else raise_error  "Invalid assignment" id.id_loc
            | _ -> raise (Error "Undefined struct")
        end
and
get_expr_binop (e : Ptree.expr) =
    let Ptree.Ebinop(bop, _e1, _e2) = e.expr_node in
    match bop with
    | Beq | Bneq | Blt | Ble | Bgt| Bge -> 
        let e1 = get_expr _e1 in
        let e2 = get_expr _e2 in
        if equiv_types e1.expr_typ e2.expr_typ
        then {expr_node = Ebinop (bop, e1, e2); expr_typ = Tint}
        else raise (Error "Wrong operand types")
    | Badd | Bsub | Bmul | Bdiv ->
        let e1 = get_expr _e1 in
        let e2 = get_expr _e2 in
        if equiv_types e1.expr_typ Tint && equiv_types e2.expr_typ Tint
        then {expr_node = Ebinop (bop, e1, e2); expr_typ = Tint} 
        else raise (Error "Wrong operand types")
    | Band | Bor -> 
        let e1 = get_expr _e1 in
        let e2 = get_expr _e2 in
        {expr_node = Ebinop (bop, e1, e2); expr_typ = Tint}
and
get_expr_call (e : Ptree.expr) =
    let Ptree.Ecall (id, elist) = e.expr_node in
    let fdecl_fun =
        try Hashtbl.find fun_map id.id 
        with Not_found -> raise_error (Printf.sprintf "Undefined function \"%s\""
        id.id) id.id_loc
    in
    let formals_types = List.map fst fdecl_fun.fun_formals in
    let args = List.map get_expr elist in
    let args_types = List.map (fun x -> x.expr_typ) args in
    let rec valid_arg_type flist alist =
        match flist, alist with
            | [], [] -> true
            | x::xs, y::ys -> equiv_types x y && valid_arg_type xs ys
            | _ -> raise (Error "Wrong number of arguments")
    in
    if valid_arg_type formals_types args_types
    then {expr_node = Ecall (id.id, args); expr_typ = fdecl_fun.fun_typ}
    else raise_error "Invalid argument type" id.id_loc
and
get_expr_right (e : Ptree.expr) =
    let Ptree.Eright l = e.expr_node in
    match l with
    | Ptree.Lident id -> 
            begin
            try let var = Hashtbl.find !var_map id.id in
                {expr_node = Eaccess_local id.id; expr_typ = fst var}
            with 
                Not_found ->
                    try let str = Hashtbl.find struct_map id.id in
                    {expr_node = Eaccess_local id.id; expr_typ = Tstructp str}
                    with Not_found -> raise_error "undefined variable" id.id_loc
            end
    | Ptree.Larrow (e1, id) ->
            let e2 = get_expr e1 in
            begin
                match e2.expr_typ with
                | Tstructp struct_object ->
                        let st_field =
                            get_str_field struct_object.str_name id in
                        {
                            expr_node = Eaccess_field (e2, st_field);
                            expr_typ = st_field.field_typ
                        }
                | _ -> raise_error "Not a structure pointer" id.id_loc
            end 
and 
get_expr_sizeof (e : Ptree.expr) =
    let Ptree.Esizeof id = e.expr_node in
    let st = Hashtbl.find struct_map id.id in
    {expr_node = Esizeof st; expr_typ = Tint}
    

let rec get_stmt (stmt : Ptree.stmt) =
    match stmt.stmt_node with
    | Ptree.Sskip -> Sskip
    | Ptree.Sexpr e -> Sexpr (get_expr e)
    | Ptree.Sif (e, s1, s2) -> Sif (get_expr e, get_stmt s1, get_stmt s2)
    | Ptree.Swhile (e, s) -> Swhile (get_expr e, get_stmt s)
    | Ptree.Sblock b ->
            let hash_tbl_copy = Hashtbl.copy !var_map in
            let tblock = Sblock (get_block b) in
            var_map := Hashtbl.copy hash_tbl_copy; tblock
    | Ptree.Sreturn e -> Sreturn (get_expr e)
and
get_block (b : Ptree.block) =
    let decls, stmts = b in
    let decls = get_decl_list decls in
    let stmts = get_stmt_list stmts in
    decls, stmts
and
get_stmt_list (stmts : Ptree.stmt list) =
    let rec aux acc = function
        | [] -> List.rev acc
        | stmt :: tail -> aux ((get_stmt stmt) :: acc) tail
    in
    aux [] stmts
    

let get_fun_body (dfun : Ptree.decl_fun) =
    get_block dfun.fun_body

let process_dfun (dfun : Ptree.decl_fun) =
    Hashtbl.clear !var_map;
    let fun_name = get_fun_name dfun in
    let fun_type = get_fun_typ dfun in
    if Hashtbl.mem fun_map fun_name
        then raise_error "Already defined function" dfun.fun_name.id_loc
        else
    if Hashtbl.mem struct_map fun_name
        then raise_error "Struct with the same name already defined"
             dfun.fun_name.id_loc
        else
    let fun_formals = get_fun_formals dfun in
    let dummy_fun =
        {
            fun_typ = fun_type;
            fun_name = fun_name;
            fun_formals = fun_formals;
            fun_body = ([], [])
        } in
    Hashtbl.add fun_map fun_name dummy_fun;
    let fun_body = get_fun_body dfun in
    let new_fun =
        {
            fun_typ = fun_type;
            fun_name = fun_name;
            fun_formals = fun_formals;
            fun_body = fun_body;
        }
    in
    Hashtbl.add fun_map fun_name new_fun;
    new_fun

let process_dstr (dstr : Ptree.decl_struct) =
    let str_name, fields_list = dstr in
    if Hashtbl.mem struct_map str_name.id
        then raise_error "Struct already defined" str_name.id_loc
        else
    if Hashtbl.mem fun_map str_name.id
        then raise_error "Function with same name already defined"
        str_name.id_loc
        else 
    let htbl = Hashtbl.create (List.length fields_list) in
    let str = {str_name = str_name.id; str_fields = htbl} in
    Hashtbl.add struct_map str_name.id str;
    let translate_type (t : Ptree.typ) =
        match t with
        | Ptree.Tint -> Tint
        | Ptree.Tstructp id ->
                if Hashtbl.mem struct_map id.id
                    then Tstructp (Hashtbl.find struct_map id.id)
                    else raise_error "Undefined structure" id.id_loc in
    let add_decl_var_to_htbl (t, id : Ptree.typ * Ptree.ident) =
        if Hashtbl.mem htbl id.id
            then raise_error "Variable already defined"
                id.id_loc
            else 
        Hashtbl.add htbl id.id {field_name = id.id; field_typ = (translate_type t)} in
    List.iter add_decl_var_to_htbl fields_list

let program p = 
    add_fun_sbrk ();
    add_fun_putchar();
    let rec aux acc = function
        | [] -> List.rev acc
        | Ptree.Dfun dfun :: tail -> aux (process_dfun dfun :: acc) tail
        | Ptree.Dstruct dstr :: tail -> process_dstr dstr; aux acc tail
    in
    {funs = aux [] p}
