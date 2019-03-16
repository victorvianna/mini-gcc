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

let original_intfs = ref Register.M.empty

let generate i = (* should move this to a common file later *)
  let l = Label.fresh () in
  graph := Label.M.add l i !graph;
  l

let k = Register.S.cardinal Register.allocatable
let spilled_regs = ref 0

let no_preference_edge r r_arcs =
  Register.S.is_empty r_arcs.prefs

let reg_degree g r =
  try
    let r_arcs = Register.M.find r g in
    Register.S.cardinal r_arcs.prefs + Register.S.cardinal r_arcs.intfs
  with Not_found -> raise (Error "Empty interference graph")

let minimal_degree_reg g =
  let min_r = ref (fst (Register.M.choose g)) in
  let min_degree = ref (reg_degree g !min_r) in
  let update_best r r_arcs =
    let r_degree = reg_degree g r in
    if r_degree < !min_degree then
      begin
        min_r := r;
        min_degree := r_degree
      end
  in
  Register.M.iter update_best g;
  !min_r

let satisfies_crit_when_v2_is_hw g v1 v2 =
  let v1_arcs = Register.M.find v1 g in
  let v2_arcs = Register.M.find v2 g in
  let candidates =
    Register.S.filter
      (fun s -> reg_degree g s >= k || Register.is_pseudo s) v1_arcs.intfs
  in
  Register.S.for_all (fun s -> Register.S.mem s v2_arcs.intfs) candidates

let satisfies_crit_when_v2_is_pseudo g v1 v2 =
  let v1_arcs = Register.M.find v1 g in
  let v2_arcs = Register.M.find v2 g in
  let candidates =
    Register.S.filter
      (fun s -> reg_degree g s >= k || Register.is_hw s) v1_arcs.intfs
  in
  Register.S.for_all (fun s -> Register.S.mem s v2_arcs.intfs) candidates

let find_george_edge g =
  let may_be_merged_with v1 v2 =
    if Register.is_hw v2 then satisfies_crit_when_v2_is_hw g v1 v2
    else satisfies_crit_when_v2_is_pseudo g v1 v2
  in
  let has_george_edge v =
    let v_arcs = Register.M.find v g in
    let candidates = Register.S.filter (may_be_merged_with v) v_arcs.prefs in
    not (Register.S.is_empty candidates)
  in
  let v1_candidates =
    Register.M.filter (fun r a -> has_george_edge r) g
  in
  if Register.M.is_empty v1_candidates then None
  else
    let v1, v1_arcs = Register.M.choose v1_candidates in
    let v2_candidates = Register.S.filter (may_be_merged_with v1) v1_arcs.prefs in
    Some (v1, Register.S.choose v2_candidates)

let forget_pref_edges g v =
  let v_arcs = Register.M.find v g in
  let new_graph = ref (Register.M.add v {v_arcs with prefs = Register.S.empty} g) in
  Register.S.iter
    (fun s -> let s_arcs = Register.M.find s !new_graph in
      let new_prefs = Register.S.remove v s_arcs.prefs in
      new_graph := Register.M.add s {s_arcs with prefs = new_prefs} !new_graph
    ) v_arcs.prefs;
  !new_graph

let minimal_cost_reg g =
  let cost v =
    let n_uses = 1 (*Register.M.find v n_uses_graph*) in
    let degree = reg_degree g v in
    (float_of_int n_uses) /. (float_of_int degree)
  in
  let best_v = ref (fst (Register.M.choose g)) in
  let min_cost = ref (cost !best_v) in
  Register.M.iter
    (fun r a ->
       let r_cost = cost r in
       if r_cost < !min_cost then
         begin min_cost := r_cost; best_v := r end
    ) g;
  !best_v

let remove_reg g v =
  let v_arcs = Register.M.find v g in
  let new_graph = ref (Register.M.remove v g) in
  Register.S.iter
    (fun s ->
       let s_arcs = Register.M.find s !new_graph in
       let new_intfs = Register.S.remove v s_arcs.intfs in
       let new_prefs = Register.S.remove v s_arcs.prefs in
       new_graph := Register.M.add s {prefs = new_prefs; intfs = new_intfs} !new_graph
    ) (Register.S.union v_arcs.intfs v_arcs.prefs);
  !new_graph

let lookup c r =
  if Register.is_hw r then Reg r else Register.M.find r c

let color_possible g c v =
  if Register.is_hw v then Some v
  else
    let v_arcs = Register.M.find v g in
    let neighbour_colors = ref (Register.S.empty) in
    Register.S.iter
      (fun s -> match lookup c s with
         | Reg hr -> neighbour_colors := Register.S.add hr !neighbour_colors
         | Spilled _ -> ()
      ) v_arcs.intfs;
    let possible_colors = Register.S.diff Register.allocatable (Register.M.find v !original_intfs)
    in
    Register.S.choose_opt (Register.S.diff possible_colors !neighbour_colors)

(* function to keep only interf edges in case there are both *)
let clean_double edges =
  {intfs = edges.intfs; prefs = Register.S.diff edges.prefs edges.intfs}

let fusion g v1 v2 =
  let soldado = Register.M.find v1 !original_intfs in
  let cauim = Register.M.find v2 !original_intfs in
  original_intfs := Register.M.add v2 (Register.S.union soldado cauim) !original_intfs;
  let v1_arcs = Register.M.find v1 g in
  let v1_new_intfs = Register.S.remove v2 v1_arcs.intfs in
  let v1_new_prefs = Register.S.remove v2 v1_arcs.prefs in
  let v2_arcs = Register.M.find v2 g in
  let v2_new_intfs = Register.S.remove v1 v2_arcs.intfs in
  let v2_new_prefs = Register.S.remove v1 v2_arcs.prefs in
  let v2_new_intfs = Register.S.union v1_new_intfs v2_new_intfs in
  let v2_new_prefs = Register.S.union v1_new_prefs v2_new_prefs in
  let new_graph = ref g in
  new_graph := Register.M.add v2 {prefs = v2_new_prefs; intfs = v2_new_intfs} !new_graph;
  new_graph := Register.M.remove v1 !new_graph;
  let transform_v1 = (fun v -> if v = v1 then v2 else v) in
  (* convert v1 to v2 in every edge list *)
  Register.M.iter (fun s s_arcs ->
      let new_intfs = Register.S.map transform_v1 s_arcs.intfs in
      let new_prefs = Register.S.map transform_v1 s_arcs.prefs in
      new_graph := Register.M.add s {prefs = new_prefs; intfs = new_intfs} !new_graph
    ) !new_graph;
  (* clean double edges that may have been created *)
  Register.M.map (fun arcs -> clean_double arcs) !new_graph

let rec simplify g =
  let no_pref_edges_graph = Register.M.filter no_preference_edge g in
  if not (Register.M.is_empty no_pref_edges_graph) then
    begin
      let min_degree_r = minimal_degree_reg no_pref_edges_graph in
      let min_degree = reg_degree no_pref_edges_graph min_degree_r in
      if min_degree < k then select g min_degree_r
      else coalesce g
    end
  else coalesce g
and
  coalesce g =
  match find_george_edge g with
  | Some (v1, v2) ->
    let g = fusion g v1 v2 in
    let c = simplify g in
    let c = Register.M.add v1 (lookup c v2) c in
    c
  | None ->
    freeze g
and
  freeze g =
  if (Register.M.is_empty g) then spill g
  else
    let min_r = minimal_degree_reg g in
    let min_degree = reg_degree g min_r in
    if min_degree < k then
      let g = forget_pref_edges g min_r in
      simplify g
    else
      spill g
and
  spill g =
  if Register.M.is_empty g then
    Register.M.empty
  else
    let v = minimal_cost_reg g in
    select g v
and
  select g v =
  let g_v = remove_reg g v in
  let c = simplify g_v in
  match color_possible g c v with
  | Some x -> Register.M.add v (Reg x) c
  | None ->
    let word_size = 8 in
    incr spilled_regs;
    Register.M.add v (Spilled (!spilled_regs * -word_size)) c

let color_george_appel graph =
  original_intfs := Register.M.empty;
  Register.M.iter
    (fun s s_arcs ->
       let intfs = s_arcs.intfs in
       let hw_intfs = Register.S.filter (fun r -> Register.is_hw r) intfs in
       original_intfs := Register.M.add s hw_intfs !original_intfs
    ) graph;
  simplify graph

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
  let is_colored_and_possible pcolors r =
    if not (Register.M.mem r color_map) then false
    else match lookup color_map r with
      | Reg hw_reg -> Register.S.mem hw_reg pcolors
      | _ -> false in
  let satisfies_crit r =
    let r_arcs = Register.M.find r graph in
    let prefs = r_arcs.prefs in
    let pcolors = Register.M.find r pcolors_map in
    Register.S.exists (is_colored_and_possible pcolors) prefs in
  let solution = Register.S.filter satisfies_crit todo in
  try
    let r = Register.S.choose solution in
    let r_arcs = Register.M.find r graph in
    let prefs = r_arcs.prefs in
    let pcolors = Register.M.find r pcolors_map in
    let colored_prefs = Register.S.filter (is_colored_and_possible pcolors) prefs in
    let colored_pref = Register.S.choose colored_prefs in
    let c = match lookup color_map colored_pref with
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

let compare_operands op1 op2 =
  match op1, op2 with
  | Reg r1, Reg r2 -> r1 = r2
  | Spilled i1, Spilled i2 -> i1 = i2
  | _ -> false

let translate_Embinop op r1 r2 l c =
  let op1 = lookup c r1 in
  let op2 = lookup c r2 in
  if op = Ops.Mmov && compare_operands op1 op2 then Egoto l else
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

let translate_Emubranch branch r l1 l2 c =
  let op = lookup c r in
  Emubranch (branch, op, l1, l2)

let translate_Embbranch branch r1 r2 l1 l2 c =
  let op1 = lookup c r1 in
  let op2 = lookup c r2 in
  match op1, op2 with
  | Reg _, _ | _ , Reg _ -> Embbranch (branch, op1, op2, l1, l2)
  | _, _ ->
    let l = generate (Embbranch (branch, Reg Register.tmp1, op2, l1, l2)) in
    Embinop (Mmov, op1, Reg Register.tmp1, l)

let translate_Eget_param idx dst_reg dst_label color_map n_pars =
  let dst_op = lookup color_map dst_reg in
  let n_pars_hw_regs = List.length Register.parameters in
  if idx < n_pars_hw_regs then
    let src_op = lookup color_map (List.nth Register.parameters idx) in
    Embinop (Mmov, src_op, dst_op, dst_label)
  else
    let word_size = 8 in
    let offset_idx = n_pars - idx + 1 in
    let offset = offset_idx * word_size in
    match dst_op with
    | Reg r_color -> Eload (Register.rbp, offset, r_color, dst_label)
    | _ ->
      let l = generate (Embinop (Mmov, Reg Register.tmp1, dst_op, dst_label)) in
      Eload (Register.rbp, offset, Register.tmp1, l)


let translate_Ealloc_frame frame_size l =
  if frame_size > 0 then
    let word_size = 8 in
    let i32_shift = Int32.of_int (-word_size * frame_size) in
    let l = generate (Emunop (Maddi i32_shift, Reg Register.rsp, l)) in
    let l = generate (Embinop (Mmov, Reg Register.rsp, Reg Register.rbp, l)) in
    Epush (Reg Register.rbp, l)
  else
    Egoto l

let translate_Edelete_frame frame_size l =
  if frame_size > 0 then
    let l = generate (Epop (Register.rbp, l)) in
    Embinop (Mmov, Reg Register.rbp, Reg Register.rsp, l)
  else
    Egoto l

let translate_Epush_param r l color_map =
  let word_size = 8 in
  let i32_shift = Int32.of_int (-word_size) in
  match lookup color_map r with
  | Reg c ->
    let l = generate (Estore (c, Register.rsp, 0, l)) in
    Emunop (Maddi i32_shift, Reg Register.rsp, l)
  | Spilled off as op ->
    let l = generate (Estore (Register.tmp1, Register.rsp, 0, l)) in
    let l = generate (Emunop (Maddi i32_shift, Reg Register.rsp, l)) in
    Embinop (Mmov, op, Reg Register.tmp1, l)

let instr c frame_size n_pars = function
  | Ertltree.Econst (n, r, l) -> Econst (n, lookup c r, l)
  | Ertltree.Ereturn -> Ereturn
  | Ertltree.Ecall (id, i, l) -> Ecall (id, l)
  | Ertltree.Egoto l -> Egoto l
  | Ertltree.Ealloc_frame l ->
    translate_Ealloc_frame frame_size l
  | Ertltree.Edelete_frame l ->
    translate_Edelete_frame frame_size l
  | Ertltree.Eload (r1, i, r2, l) ->
    translate_Eload r1 i r2 l c
  | Ertltree.Estore (r1, r2, i, l) ->
    translate_Estore r1 r2 i l c
  | Ertltree.Emunop (op, r, l) ->
    Emunop (op, lookup c r, l)
  | Ertltree.Embinop (op, r1, r2, l) ->
    translate_Embinop op r1 r2 l c
  | Ertltree.Emubranch (branch, r, l1, l2) ->
    translate_Emubranch branch r l1 l2 c
  | Ertltree.Embbranch (branch, r1, r2, l1, l2) ->
    translate_Embbranch branch r1 r2 l1 l2 c
  | Ertltree.Eget_param (i, r, l) ->
    translate_Eget_param i r l c n_pars
  | Ertltree.Epush_param (r, l) ->
    translate_Epush_param r l c

let translate_fun (f:Ertltree.deffun) =
  let l_info = Ertl.liveness f.fun_body in
  let interf_graph = make l_info in
  let color_map = color_george_appel interf_graph in
  let frame_size = !spilled_regs in
  (* we have to clear the graph because otherwise we might have
     conflict if two different functions have the same label *)
  let () = graph := Label.M.empty in
  let visited_labels = ref Label.S.empty in
  let rec dfs (label : label) =
    if not (Label.S.mem label !visited_labels) then
      let () = visited_labels := Label.S.add label !visited_labels in
      let ertl_inst = Label.M.find label f.fun_body in
      let succs = Ertltree.succ ertl_inst in
      let ltl_inst = instr color_map frame_size f.fun_formals ertl_inst in
      let () = graph := Label.M.add label ltl_inst !graph in
      List.iter dfs succs in
  let () = dfs f.fun_entry in
  {
    fun_name = f.fun_name;
    fun_entry = f.fun_entry;
    fun_body = !graph
  }

let program (file:Ertltree.file) =
  {funs = List.map translate_fun file.funs}
