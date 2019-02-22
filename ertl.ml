open Ertltree

(* utiliser cette exception pour signaler une erreur *)
exception Error of string

let graph = ref Label.M.empty

let generate i = (* should move this to a common file later *)
  let l = Label.fresh () in
  graph := Label.M.add l i !graph;
  l

let instr = function
| Rtltree.Econst (r, n, destl) ->
  Econst (r, n, destl)
| Rtltree.Eload (r1, d, r2, destl) ->
  Eload (r1, d, r2, destl)
| Rtltree.Estore (r1, r2, d, destl) ->
  Estore (r1, r2, d, destl)
| Rtltree.Emunop (op, r, destl) ->
  Emunop (op, r, destl)
| Rtltree.Embinop (op, r1, r2, destl) ->
begin
  match op with
  | Mdiv ->
          let l = generate (Embinop (Mmov, Register.rax, r2, destl)) in
          let l = generate (Embinop (Mdiv, r1, Register.rax, l)) in
          Embinop (Mmov, r2, Register.rax, l)
  | _ ->
    Embinop (op, r1, r2, destl)
end
| Rtltree.Emubranch (mubranch, r, l1, l2) ->
  Emubranch (mubranch, r, l1, l2)
| Rtltree.Embbranch (mbbranch, r1, r2, l1, l2) ->
  Embbranch (mbbranch, r1, r2, l1, l2)
| Rtltree.Egoto (destl) ->
  Egoto (destl)
| Rtltree.Ecall (r, f, rl, l) ->
    let n_hw_registers = List.length Register.parameters in
    let n_regs_stack = max (List.length rl - n_hw_registers) 0 in
    let word_size = 8 in
    let rec move_registers cnt destl ertl_instr =
        begin function
        | [] -> ertl_instr
        | x :: tail ->
            let new_l = generate ertl_instr in
            if cnt = n_hw_registers - 1 then
                let new_inst =
                    Embinop (Mmov, x, List.nth Register.parameters cnt, new_l)
                in move_registers (cnt + 1) new_l new_inst (List.rev tail)
            else if cnt < n_hw_registers then
                let new_inst =
                    Embinop (Mmov, x, List.nth Register.parameters cnt, new_l)
                in move_registers (cnt + 1) new_l new_inst tail
            else
                let new_inst = Epush_param (x, new_l)
                in move_registers (cnt + 1) new_l new_inst tail
        end in
    (* pop parameters passed on the stack *)
    let l = if n_regs_stack > 0 then
        let shift = n_regs_stack * word_size in
        generate (Emunop (Maddi (Int32.of_int shift), Register.rsp, l))
        else l in
    (* move return value to register r *)
    let l = generate (Embinop (Mmov, Register.result, r, l)) in
    (* call function *)
    let call_instr = Ecall (f, List.length rl - n_regs_stack, l) in
    move_registers 0 l call_instr rl


let _succ = function
    | Rtltree.Econst (_, _, l)
    | Rtltree.Eload (_, _, _, l)
    | Rtltree.Estore (_, _, _, l)
    | Rtltree.Emunop (_, _, l)
    | Rtltree.Embinop (_, _, _, l)
    | Rtltree.Egoto l
    | Rtltree.Ecall (_, _, _, l)
        -> [l]
    | Rtltree.Emubranch (_, _, l1, l2)
    | Rtltree.Embbranch (_, _, _, l1, l2)
        -> [l1; l2]

let generate_alloc_frame first_instr =
    let l = generate first_instr in
    Ealloc_frame l

let save_callee_saved_regs first_instr fresh_regs =
    let rec aux instr regs = function
        | [] -> instr
        | x :: tl ->
                let l = generate instr in
                let fresh_reg = List.hd regs in
                let new_instr =
                    Embinop (Mmov, x, fresh_reg, l) in
                aux new_instr (List.tl regs) tl
    in aux first_instr (List.rev fresh_regs) (List.rev Register.callee_saved)

let generate_get_arguments first_instr fun_locals =
    let n_hw_registers = List.length Register.parameters in
    let word_size = 8 in
    let rec aux instr cnt = function
        | [] -> instr
        | x :: tl ->
                let l = generate instr in
                if cnt = n_hw_registers - 1 then
                    let new_instr =
                        Embinop (Mmov, List.nth Register.parameters cnt, x, l)
                    in aux new_instr (cnt + 1) (List.rev tl)
                else if cnt < n_hw_registers then
                    let new_instr =
                        Embinop (Mmov, List.nth Register.parameters cnt, x, l)
                    in aux new_instr (cnt + 1) tl
                else
                    let shift = (2 + cnt - n_hw_registers) * word_size in
                    let new_inst =
                        Eload (Register.rbp, shift, x, l)
                    in aux new_inst (cnt + 1) tl
    in aux first_instr 0 fun_locals

let restore_callee_saved l =
    let rec aux cnt l fresh_regs = function
        | [] -> l, List.rev fresh_regs
        | x :: tl ->
                let reg = List.nth Register.callee_saved cnt in
                let fresh_reg = Register.fresh () in
                let new_instr =
                    Embinop (Mmov, fresh_reg, reg, l) in
                aux (cnt + 1) (generate new_instr) (fresh_reg :: fresh_regs) tl
    in aux 0 l [] Register.callee_saved

let translate_fun (f : Rtltree.deffun) =
    let fun_name = f.fun_name in
    let fun_formals = List.length f.fun_formals in
    let fun_entry = f.fun_entry in
    let fun_exit = f.fun_exit in
    let fun_body = f.fun_body in
    let l = generate Ereturn in
    let l = generate (Edelete_frame l) in
    let (l, fresh_regs) = restore_callee_saved l in
    let () = graph := Label.M.add fun_exit
        (Embinop (Mmov, f.fun_result, Register.rax, l)) !graph in
    let visit f g entry =
        let visited = Hashtbl.create 97 in
        let () = Hashtbl.add visited fun_exit () in
        let rec visit l =
            if not (Hashtbl.mem visited l) then
            let () = Hashtbl.add visited l () in
            let i = Label.M.find l g in
            let () = f l i in
            List.iter visit (_succ i)
        in
        visit entry in
    let () = visit (fun l rtl_instr ->
        let ertl_instr = instr rtl_instr in
        graph := Label.M.add l ertl_instr !graph) f.fun_body fun_entry in
    let first_instr = Label.M.find fun_entry !graph in
    let first_instr = generate_get_arguments first_instr f.fun_formals in
    let first_instr = save_callee_saved_regs first_instr fresh_regs in
    let first_instr = generate_alloc_frame first_instr in
    let () = graph := Label.M.add fun_entry first_instr !graph in
    let fun_locals = f.fun_locals in
    let fun_locals = Register.S.union fun_locals (Register.S.of_list fresh_regs)
    in
    {
        fun_name = fun_name;
        fun_formals = fun_formals;
        fun_locals = fun_locals;
        fun_entry = fun_entry;
        fun_body = !graph;
    }

let program (rtltree : Rtltree.file) : file =
  {funs = List.map translate_fun rtltree.funs}

(************ LIVENESS  ************)
open Format

type live_info = {
  instr: Ertltree.instr;
  succ: Label.t list;    (* successeurs *)
  mutable pred: Label.set;       (* prédécesseurs *)
  defs: Register.set;    (* définitions *)
  uses: Register.set;    (* utilisations *)
  mutable  ins: Register.set;    (* variables vivantes en entrée *)
  mutable outs: Register.set;    (* variables vivantes en sortie *)
}


let liveness (g:cfg) : live_info Label.map =
  let (all_info:live_info Label.map) = Label.M.empty in
  let get_info l = Label.M.find l all_info in
  (* 1.basic filling of info table *)
  let all_labels = List.of_seq (Seq.map (function (k, v) -> k) (Label.M.to_seq g)) in
  let dfs (entry:label) : unit =
     let add_info (l:label) (i:instr) :unit =
        let (def, use) = def_use i in
        let info = {
          instr = i;
          succ = succ(i);
          pred = Label.S.empty;
          defs = Register.S.of_list def;
          uses = Register.S.of_list use;
          ins = Register.S.empty;
          outs = Register.S.empty
        } in
        Label.M.add l info all_info;
        ()
    in
    if not (Label.M.mem entry all_info) (* do not visit more than once *)
    then visit add_info g entry
  in
  List.iter dfs all_labels; (* graph may be disconnected *)
  (* 2.fill pred field, i.e., use reverse edges *)
  let add_pred pred succ =
    let info_succ = get_info succ in
    Label.S.add pred info_succ.pred;
    ()
  in
  let add_all_pred pred pred_info =
    List.iter (add_pred pred) pred_info.succ
  in
  Label.M.iter add_all_pred all_info;
  (* 3. Kildall's algorithm *)
  let update_in_outs l =
    let info = get_info l in
    info.outs <- List.fold_left (fun ins succ -> Register.S.union ins (get_info succ).ins) Register.S.empty info.succ;
    info.ins <- Register.S.union info.uses (Register.S.diff info.outs info.defs)
  in
  let rec kildall ws :unit=
    if(not (Label.S.is_empty ws)) then
    begin
        let l = Label.S.choose ws in
        let tail = Label.S.remove l ws in
        let info = get_info l in
        let old_in = info.ins in
        update_in_outs l;
        if Register.S.equal old_in info.ins
        then kildall tail
        else kildall (Label.S.union info.pred tail)
    end
    else
        ()
  in
  kildall (Label.S.of_list all_labels);
  all_info

let print_set = Register.print_set

let print_live_info fmt (li:live_info) =
  fprintf fmt "d={%a}@ u={%a}@ i={%a}@ o={%a}"
    print_set li.defs print_set li.uses print_set li.ins print_set li.outs
