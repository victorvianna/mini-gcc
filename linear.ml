open X86_64

exception Error of string
   
let visited_labels = Hashtbl.create 17
type instr = Code of X86_64.text | Label of Label.t
let code = ref []
let emit l instr = code := Code instr :: Label l :: !code
let emit_wl instr = code := Code instr :: !code
let emit_label l = code := Label l :: !code
let labels = Hashtbl.create 17
let need_label l = Hashtbl.add labels l ()

let operand ltl_operand =
  match ltl_operand with
  | Ltltree.Reg r -> reg (register64 r)
  | Ltltree.Spilled offset -> ind ~ofs:(offset) rbp 
  
let rec lin ltl_map l =
  if not (Hashtbl.mem visited_labels l) then begin
    Hashtbl.add visited_labels l ();
    instr ltl_map l (Label.M.find l ltl_map)
  end else begin
    need_label l;
    emit_wl (jmp (l :> string))
  end

and instr ltl_map l = function
  | Ltltree.Econst (n, r, l1) ->
      emit l (movq (imm32 n) (operand r)); lin ltl_map l1
  | Ltltree.Eload (r1, i, r2, l1) ->
     let op1 = ind ~ofs:i (register64 r1) in
     let op2 = reg (register64 r2) in
     emit l (movq op1 op2); lin ltl_map l1
  | Ltltree.Estore (r1, r2, i, l1) ->
     let op1 = reg (register64 r1) in
     let op2 = ind ~ofs:i (register64 r2) in
     emit l (movq op1 op2); lin ltl_map l1
  | Ltltree.Egoto l1 ->
     if Hashtbl.mem visited_labels l1 then
       emit_wl (jmp (l1 :> string))
     else
       begin
         emit_label l; lin ltl_map l1
       end
    
       
  | _ -> raise (Error "undefined")
