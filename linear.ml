open X86_64

exception Error of string
   
let visited_labels = Hashtbl.create 17
type instr = Code of X86_64.text | Label of Label.t
let code = ref []
let emit label instr = code := Code instr :: Label label :: !code
let emit_wl instr = code := Code instr :: !code
let labels = Hashtbl.create 17
let need_label label = Hashtbl.add labels label ()

let operand ltl_operand =
  match ltl_operand with
  | Ltltree.Reg r -> reg (register64 r)
  | Ltltree.Spilled offset -> ind ~ofs:(offset) rbp 
  
let rec lin ltl_map label =
  if not (Hashtbl.mem visited_labels label) then begin
    Hashtbl.add visited_labels label ();
    instr ltl_map label (Label.M.find label ltl_map)
  end else begin
    need_label label;
    emit_wl (jmp (label :> string))
  end

and instr ltl_map label = function
  | Ltltree.Econst (n, r, l1) ->
      emit label (movq (imm32 n) (operand r)); lin ltl_map l1
  | _ -> raise (Error "undefined")
