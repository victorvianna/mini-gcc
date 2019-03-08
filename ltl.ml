open Ltltree
open Format

exception Error of string

(* Functions to print the coloring *)
let print_color fmt = function
  | Reg hr    -> fprintf fmt "%a" Register.print hr
  | Spilled n -> fprintf fmt "stack %d" n
let print cm =
  Register.M.iter
    (fun r cr -> printf "%a -> %a@\n" Register.print r print_color cr) cm

type arcs = { prefs: Register.set; intfs: Register.set }
type igraph = arcs Register.map

type color = Ltltree.operand
type coloring = color Register.map

let graph = ref Label.M.empty

let generate i = (* should move this to a common file later *)
  let l = Label.fresh () in
  graph := Label.M.add l i !graph;
  l

let make l_info =
  let interf_graph = ref Register.M.empty in
  let add_edge r1 r2 is_interf =
    let arcs =
      try Register.M.find r1 !interf_graph
      with Not_found ->
        let empty = {prefs = Register.S.empty; intfs = Register.S.empty} in
        interf_graph := Register.M.add r1 empty !interf_graph;
        empty
    in
    let arcs =
      if r1 = r2 then arcs (* avoid self-edge *)
      else if is_interf then {arcs with intfs = Register.S.add r2 arcs.intfs}
      else {arcs with prefs = Register.S.add r2 arcs.prefs}
    in
    (* keep only interference edges in case there are both *)
    let arcs =
      if (Register.S.mem r2 arcs.intfs) && (Register.S.mem r2 arcs.prefs) then
        {arcs with prefs = Register.S.remove r2 arcs.prefs}
      else
        arcs
    in
    interf_graph := Register.M.add r1 arcs !interf_graph;
    ()
  in
  let add_bi_edge r1 r2 is_interf =
    add_edge r1 r2 is_interf;
    add_edge r2 r1 is_interf;
    ()
  in
  let add_preference_edges (i:Ertl.live_info) = match i.instr with
  | Ertltree.Embinop(Mmov, r1, r2, _) ->
    add_bi_edge r1 r2 false
  | _ -> ()
  in
  let add_interference_edges (i:Ertl.live_info)  =
    let defs = i.defs in
    let outs = i.outs in
    Register.S.iter (fun def -> Register.S.iter (fun out -> add_bi_edge def out true) outs) defs;
    ()
  in
  Hashtbl.iter (fun _ i -> add_preference_edges i) l_info;
  Hashtbl.iter (fun _ i -> add_interference_edges i) l_info;
  !interf_graph

(* Returns a pair (todo, pcolors_map).

   todo: set of pseudo-registers for which we want
         to find a suitable actual register.
   pcolors_map: set of possible actual registers for
         each pseudo-register in todo.
 *)
let get_todo_pcolors_from_graph graph =
  let todo = ref Register.S.empty in
  let pcolors_map = ref Register.M.empty in
  let add_reg_to_set reg =
    todo := Register.S.add reg !todo in
  let get_potential_colors interfs =
    Register.S.diff Register.allocatable interfs in
  let () = Register.M.iter
            (fun reg reg_arcs ->
              (* we only add reg to todo if it's a pseudo-register *)
              let interfs = reg_arcs.intfs in
                if Register.is_pseudo reg then
                let () = add_reg_to_set reg in
                let pot_colors = get_potential_colors interfs in
                pcolors_map := Register.M.add reg pot_colors !pcolors_map)
            graph in
  todo, pcolors_map

(* criterium: the register has at least one possible color *)
let fourth_crit todo pcolors_map graph color_map =
  (* criterium: the register has at least one possible color *)
  let satisfies_crit r = 
    let pcolors = Register.M.find r pcolors_map in
    not (Register.S.is_empty pcolors) in
  let solution = Register.S.filter satisfies_crit todo in
  try
    let r = Register.S.choose solution in
    let pcolors = Register.M.find r pcolors_map in
    let c = Register.S.choose pcolors in
    Some (r, c)
  with Not_found -> None

(* criterium: the register has a preference whose color is known *)
let third_crit todo pcolors_map graph color_map =
  let is_colored r =
        Register.M.mem r color_map in
  let satisfies_crit r = 
    let r_arcs = Register.M.find r graph in
    let prefs = r_arcs.prefs in
    Register.S.exists is_colored prefs in
  let solution = Register.S.filter satisfies_crit todo in
  try
    let r = Register.S.choose solution in
    let r_arcs = Register.M.find r graph in
    let prefs = r_arcs.prefs in
    let colored_prefs = Register.S.filter is_colored prefs in
    let colored_pref = Register.S.choose colored_prefs in
    let c = match Register.M.find colored_pref color_map with
      | Reg col -> col
      | _ -> raise (Error "It should had a physical register associated")
    in Some (r, c)
  with Not_found -> fourth_crit todo pcolors_map graph color_map

(* criterium: the register has only one possible color *)
let second_crit todo pcolors_map graph color_map =
  let satisfies_crit r = 
    let pcolors = Register.M.find r pcolors_map in
    Register.S.cardinal pcolors = 1 in
  let solution = Register.S.filter satisfies_crit todo in
  try
    let r = Register.S.choose solution in
    let pcolors = Register.M.find r pcolors_map in
    let c = Register.S.choose pcolors in
    Some (r, c)
  with Not_found -> third_crit todo pcolors_map graph color_map

(* criterium: the register has only one possible color and the
register has a preference edge to this color *)
let first_crit todo pcolors_map graph color_map =
  let satisfies_crit r = 
    let pcolors = Register.M.find r pcolors_map in
    let r_arcs = Register.M.find r graph in
    if Register.S.cardinal pcolors = 1 then
      let c = Register.S.choose pcolors in
      Register.S.mem c r_arcs.prefs
    else false in
  let solution = Register.S.filter satisfies_crit todo in
  try
    let r = Register.S.choose solution in
    let pcolors = Register.M.find r pcolors_map in
    let c = Register.S.choose pcolors in
    Some (r, c)
  with Not_found -> second_crit todo pcolors_map graph color_map

(* tries to find a new pseudo-register to color *)
let next_reg_color_pair todo pcolors_map grph color_map = 
  first_crit todo pcolors_map grph color_map

let color_graph graph =
  let color_map = ref Register.M.empty in
  (* color all physical register present in the graph *)
  let () = Register.M.iter
             (fun r r_arcs -> if Register.is_hw r then
                                color_map := Register.M.add r (Reg r) !color_map)
             graph in
  let n_regs_stack = ref 0 in
  let todo, pcolors_map = get_todo_pcolors_from_graph graph in
  let spill r =
    (* spills the pseudo-register r *)
    let () = todo := Register.S.remove r !todo in
    let () = incr n_regs_stack in
    color_map := Register.M.add r (Spilled (!n_regs_stack * -8)) !color_map in
  (* remove color c from the set of possible colors of register r *)
  let remove_color_from_pcolors r c =
    if Register.M.mem r !pcolors_map then
    let pcolors = Register.M.find r !pcolors_map in
    let pcolors = Register.S.remove c pcolors in
    pcolors_map := Register.M.add r pcolors !pcolors_map in
  (* sets the colors of register r to the color c and
     removes the color c from all interferences of c *)
  let set_reg_color r c =
    let () = todo := Register.S.remove r !todo in
    let () = color_map := Register.M.add r (Reg c) !color_map in
    let r_arcs = Register.M.find r graph in
    let r_intfs = r_arcs.intfs in
    Register.S.iter (fun reg -> remove_color_from_pcolors reg c) r_intfs in
  while not (Register.S.is_empty !todo) do
        match next_reg_color_pair !todo !pcolors_map graph !color_map with
        | None -> let r = Register.S.choose !todo in spill r
        | Some (r, c) -> set_reg_color r c
  done; !color_map, !n_regs_stack

let lookup c r =
  if Register.is_hw r then Reg r else Register.M.find r c

let translate_Eload r1 i r2 l c =
    let op1 = lookup c r1 in
    let op2 = lookup c r2 in
    match op1 with
    | Reg pr1 -> (* register r1 is physical *)
       begin
         match op2 with
         | Reg pr2 -> Eload (pr1, i, pr2, l)
         | Spilled pos -> 
            let l = generate (Embinop (Mmov, Reg Register.tmp1, op2, l)) in
            Eload (pr1, i, Register.tmp1, l)
       end
    | Spilled pos1 -> (* register r1 is on the stack *)
       begin
         match op2 with
         | Reg pr2 -> 
            let l = generate (Eload (Register.tmp1, i, pr2, l)) in
            Embinop (Mmov, op1, Reg Register.tmp1, l)
         | Spilled pos2 ->
            let l = generate (Embinop (Mmov, Reg Register.tmp2, op2, l)) in
            let l = generate (Eload (Register.tmp1, i, Register.tmp2, l)) in
            Embinop (Mmov, op1, Reg Register.tmp1, l)
       end

let translate_Estore r1 r2 i l c =
    let op1 = lookup c r1 in
    let op2 = lookup c r2 in
    match op1 with
    | Reg pr1 -> (* register r1 is physical *)
       begin
         match op2 with
         | Reg pr2 -> Estore (pr1, pr2, i, l)
         | Spilled _ -> 
            let l = generate (Estore (pr1, Register.tmp1, i, l)) in
            Embinop (Mmov, op2, Reg Register.tmp1, l)
       end
    | Spilled _ -> (* register r1 is on the stack *)
       begin
         match op2 with
         | Reg pr2 -> 
            let l = generate (Estore (Register.tmp1, pr2, i, l)) in
            Embinop (Mmov, op1, Reg Register.tmp1, l)
         | Spilled pos2 ->
            let l = generate (Estore (Register.tmp1, Register.tmp2, i, l)) in
            let l = generate (Embinop (Mmov, op2, Reg Register.tmp2, l)) in
            Embinop (Mmov, op1, Reg Register.tmp1, l)
       end

let translate_Embinop op r1 r2 l c =
  let op1 = lookup c r1 in
  let op2 = lookup c r2 in
  if op = Ops.Mmov && op1 = op2 then Egoto l else
  match op2 with
  | Reg _ ->
     Embinop (op, op1, op2, l)
  | Spilled _ ->
     if op = Mmul then (* in this case, the second operand should be a register *)
       let l = generate (Embinop (Mmov, Reg Register.tmp1, op2, l)) in
       let l = generate (Embinop (Mmul, op1, Reg Register.tmp1, l)) in
       Embinop (Mmov, op2, Reg Register.tmp1, l)
     else
     begin
       match op1 with
       | Reg _ -> Embinop (op, op1, op2, l)
       | Spilled _ ->
          let l = generate (Embinop (op, Reg Register.tmp1, op2, l)) in
          Embinop (Mmov, op1, Reg Register.tmp1, l)
     end

let instr c frame_size = function
  | Ertltree.Econst (n, r, l) -> Econst (n, lookup c r, l)
  | Ertltree.Ereturn -> Ereturn
  | Ertltree.Ecall (id, i, l) -> Ecall (id, l)
  | Ertltree.Egoto l -> Egoto l
  | Ertltree.Ealloc_frame l ->
     let word_size = 8 in
     let l = generate (Econst (Int32.of_int (-word_size * frame_size), Reg Register.rsp, l)) in
     let l = generate (Embinop (Mmov, Reg Register.rsp, Reg Register.rbp, l)) in
     Epush (Reg Register.rbp, l)
  | Ertltree.Edelete_frame l ->
     let l = generate (Epop (Register.rbp, l)) in
     Embinop (Mmov, Reg Register.rbp, Reg Register.rsp, l)
  | Ertltree.Eload (r1, i, r2, l) ->
     translate_Eload r1 i r2 l c
  | Ertltree.Estore (r1, r2, i, l) ->
     translate_Estore r1 r2 i l c
  | Ertltree.Emunop (op, r, l) ->
     Emunop (op, lookup c r, l)
  | Ertltree.Embinop (op, r1, r2, l) ->
     translate_Embinop op r1 r2 l c
     
    
let translate_fun (f:Ertltree.deffun) =
  let l_info = Ertl.liveness !Ertl.graph in
  let interf_graph = make l_info in
  let colors = color_graph interf_graph in
  (* TODO: translate ertl instructions *)
  {
    fun_name = f.fun_name;
    fun_entry = f.fun_entry;
    fun_body = !graph
  }

let program (file:Ertltree.file) =
  {funs = List.map translate_fun file.funs}
