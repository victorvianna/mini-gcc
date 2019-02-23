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


let succ = function
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


let translate_fun (f : Rtltree.deffun) =
    let fun_name = f.fun_name in
    let fun_formals = List.length f.fun_formals in
    let fun_locals = f.fun_locals in
    let fun_entry = f.fun_entry in
    let fun_exit = f.fun_exit in
    let fun_body = f.fun_body in
    let () = graph := Label.M.add fun_exit Ereturn !graph in
    let visit f g entry =
        let visited = Hashtbl.create 97 in
        let () = Hashtbl.add visited fun_exit () in
        let rec visit l =
            if not (Hashtbl.mem visited l) then
            let () = Hashtbl.add visited l () in
            let i = Label.M.find l g in
            let () = f l i in
            List.iter visit (succ i)
        in
        visit entry in
    let () = visit (fun l rtl_instr ->
        let ertl_instr = instr rtl_instr in
        graph := Label.M.add l ertl_instr !graph) f.fun_body fun_entry in
    {
        fun_name = fun_name;
        fun_formals = fun_formals;
        fun_locals = fun_locals;
        fun_entry = fun_entry;
        fun_body = !graph;
    }

let program (rtltree : Rtltree.file) : file =
  {funs = List.map translate_fun rtltree.funs}
