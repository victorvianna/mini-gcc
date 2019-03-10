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

(* produces an 64-bit operand if possible *)
let operand ltl_operand =
  match ltl_operand with
  | Ltltree.Reg r -> reg (register64 r)
  | Ltltree.Spilled offset -> ind ~ofs:(offset) rbp 
                            
(* produces an 8-bit operand if possible *)
let operand8 ltl_operand =
  match ltl_operand with
  | Ltltree.Reg r -> reg (register8 (register64 r))
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
  | Ltltree.Ereturn ->
     emit l ret
  | Ltltree.Emunop (unop, op, l1) ->
     begin
       match unop with
       | Maddi i32 -> 
          let op1 = imm32 i32 in
          let op2 = operand op in
          emit l (addq op1 op2); lin ltl_map l1
       | Msetei i32 ->
          let op1 = imm32 i32 in
          let op2 = operand op in
          let op2_8bits = operand8 op in
          emit l (cmpq op1 op2); emit_wl (sete op2_8bits); lin ltl_map l1
       | Msetnei i32 ->
          let op1 = imm32 i32 in
          let op2 = operand op in
          let op2_8bits = operand8 op in
          emit l (cmpq op1 op2); emit_wl (setne op2_8bits); lin ltl_map l1
     end
  | Ltltree.Embinop (binop, op1, op2, l1) ->
     let op2_8bits = operand8 op2 in
     let op1 = operand op1 in
     let op2 = operand op2 in
     begin
       match binop with
       | Mmov -> 
          emit l (movq op1 op2); lin ltl_map l1
       | Madd ->
          emit l (addq op1 op2); lin ltl_map l1
       | Msub ->
          emit l (subq op1 op2); lin ltl_map l1
       | Mmul ->
          emit l (imulq op1 op2); lin ltl_map l1
       | Mdiv ->
          emit l (idivq op1); lin ltl_map l1
       | Msete ->
          emit l (cmpq op1 op2); emit_wl (sete op2_8bits); lin ltl_map l1
       | Msetne ->
          emit l (cmpq op1 op2); emit_wl (setne op2_8bits); lin ltl_map l1
       | Msetl ->
          emit l (cmpq op1 op2); emit_wl (setl op2_8bits); lin ltl_map l1
       | Msetle ->
          emit l (cmpq op1 op2); emit_wl (setle op2_8bits); lin ltl_map l1
       | Msetg ->
          emit l (cmpq op1 op2); emit_wl (setg op2_8bits); lin ltl_map l1
       | Msetge ->
          emit l (cmpq op1 op2); emit_wl (setge op2_8bits); lin ltl_map l1
     end
  | Emubranch (branch, op, l1, l2) ->
     let op1 = operand op in
     begin
       match branch with
       | Mjz ->
          emit l (testq op1 op1);
          if not (Hashtbl.mem visited_labels l2) then
            begin
              emit_wl (jz (l1 :> string)); lin ltl_map l2; lin ltl_map l1
            end
          else if not (Hashtbl.mem visited_labels l1) then
            begin
              emit_wl (jnz (l2 :> string)); lin ltl_map l1; lin ltl_map l2
            end
          else
            begin
              emit_wl (jz (l1 :> string)); emit_wl (jmp (l2 :> string))
            end
       | Mjnz ->
          emit l (testq op1 op1);
          if not (Hashtbl.mem visited_labels l2) then
            begin
              emit_wl (jnz (l1 :> string)); lin ltl_map l2; lin ltl_map l1
            end
          else if not (Hashtbl.mem visited_labels l1) then
            begin
              emit_wl (jz (l2 :> string)); lin ltl_map l1; lin ltl_map l2
            end
          else
            begin
              emit_wl (jnz (l1 :> string)); emit_wl (jmp (l2 :> string))
            end
       | Mjlei i32 ->
          emit l (cmpq (imm32 i32) op1);
          if not (Hashtbl.mem visited_labels l2) then
            begin
              emit_wl (jle (l1 :> string)); lin ltl_map l2; lin ltl_map l1
            end
          else if not (Hashtbl.mem visited_labels l1) then
            begin
              emit_wl (jg (l2 :> string)); lin ltl_map l1; lin ltl_map l2
            end
          else
            begin
              emit_wl (jle (l1 :> string)); emit_wl (jmp (l2 :> string))
            end
       | Mjgi i32 ->
          emit l (cmpq (imm32 i32) op1);
          if not (Hashtbl.mem visited_labels l2) then
            begin
              emit_wl (jg (l1 :> string)); lin ltl_map l2; lin ltl_map l1
            end
          else if not (Hashtbl.mem visited_labels l1) then
            begin
              emit_wl (jle (l2 :> string)); lin ltl_map l1; lin ltl_map l2
            end
          else
            begin
              emit_wl (jg (l1 :> string)); emit_wl (jmp (l2 :> string))
            end
     end
     
     
  | _ -> raise (Error "undefined")
