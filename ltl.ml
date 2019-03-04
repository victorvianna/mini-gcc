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
  let todo = ref Register.set.empty in
  let pcolors_map = ref Register.map.empty in
  let add_reg_to_set reg =
    todo := Register.set.add reg !todo in
  let get_potential_colors interfs =
    Register.set.difference Register.allocatable interfs in
  let () = Register.map.iter
            (fun reg interfs ->
              (* we only add reg to todo if it's a pseudo-register *)
                if Register.is_pseudo reg then
                let () = add_reg_to_set reg in
                let pot_colors = get_potential_colors interfs in
                pcolors_map := Register.map.add reg pot_colors !pcolors_map)
            graph in
  todo, pcolors_map

let color graph =
  let color_map = ref Register.map.empty in
  let n_regs_stack = ref 0 in
  let todo, pcolors_map = get_todo_pcolors_from_graph graph in
  let potential_colors_map = create_potential_colors_map todo graph in
  let find_reg = (* tries to find a new pseudo-register to color *)
    blablabla in
  let spill = (* spills a new pseudo-register *)
    blablbal in
  let set_reg_color r c = (* sets the color of r to c *)
    blablabla in
  let rec iterate () =
    let () =
      begin
        match find_reg with
        | None -> spill
        | Some r, c -> set_reg_color r c
      end in
    if Register.set.is_empty todo then iterate ()
  color_map, !n_regs_stack

let lookup c r =
  if Register.is_hw r then Reg r else Register.M.find r c

let instr c frame_size = function
  | Ertltree.Econst (n, r, l) ->
      Econst (n, lookup c r, l)

let translate_fun (f:Ertltree.deffun) =
  let l_info = Ertl.liveness !Ertl.graph in
  let interf_graph = make l_info in
  let colors = color interf_graph in
  (* TODO: translate ertl instructions *)
  {
    fun_name = f.fun_name;
    fun_entry = f.fun_entry;
    fun_body = !graph
  }

let program (file:Ertltree.file) =
  {funs = List.map translate_fun file.funs}
