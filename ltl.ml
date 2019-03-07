open Ltltree

type arcs = { prefs: Register.set; intfs: Register.set }
type igraph = arcs Register.map

type color = Ltltree.operand
type coloring = color Register.map

let graph = ref Label.M.empty
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
    let c = Register.S.choose colored_prefs in
    Some (r, c)
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
  (* add all physical registers to map *)
  let () = Register.S.iter
             (fun s -> color_map := Register.M.add s (Reg s) !color_map)
             Register.allocatable
  in
  let n_regs_stack = ref 0 in
  let todo, pcolors_map = get_todo_pcolors_from_graph graph in
  let spill r =
    (* spills the pseudo-register r *)
    let () = todo := Register.S.remove r !todo in
    let () = color_map := Register.M.add r (Spilled !n_regs_stack) !color_map in
    incr n_regs_stack in
  (* remove color c from the set of possible colors of register r *)
  let remove_color_from_pcolors r c =
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

let instr c frame_size = function
  | Ertltree.Econst (n, r, l) ->
      Econst (n, lookup c r, l)

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
