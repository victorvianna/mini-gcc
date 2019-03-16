open X86_64
open Format

exception Error of string

let visited_labels = Hashtbl.create 17
type instr = Code of X86_64.text | Label of Label.t
let code = ref []
let emit l instr = code := Code instr :: Label l :: !code
let emit_wl instr = code := Code instr :: !code
let emit_label l = code := Label l :: !code
let labels = Hashtbl.create 17
let need_label l = Hashtbl.add labels l ()

let funs = ref []

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

let get_fun_entry fun_name =
  let rec aux = function
    | [] -> raise (Error "function not found")
    | (fun_def : Ltltree.deffun) :: fun_def_list ->
      if fun_def.fun_name = fun_name then
        fun_def
      else
        aux fun_def_list
  in aux !funs

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
      begin
        need_label l1; emit l (jmp (l1 :> string))
      end
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
        emit l cqto;
        emit_wl (idivq op1); lin ltl_map l1
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
    begin
      match branch with
      | Mjz ->
        let op1 = operand8 op in
        let tmp_op = reg (register8 (register64 Register.tmp1)) in
        emit l (movb op1 tmp_op);
        emit_wl (testb tmp_op tmp_op);
        if not (Hashtbl.mem visited_labels l2) then
          begin
            need_label l1; emit_wl (jz (l1 :> string)); lin ltl_map l2; lin ltl_map l1
          end
        else if not (Hashtbl.mem visited_labels l1) then
          begin
            need_label l2; emit_wl (jnz (l2 :> string)); lin ltl_map l1; lin ltl_map l2
          end
        else
          begin
            need_label l1; need_label l2; emit_wl (jz (l1 :> string)); emit_wl (jmp (l2 :> string))
          end
      | Mjnz ->
        let op1 = operand8 op in
        let tmp_op = reg (register8 (register64 Register.tmp1)) in
        emit l (movb op1 tmp_op);
        emit_wl (testb tmp_op tmp_op);
        if not (Hashtbl.mem visited_labels l2) then
          begin
            need_label l1; emit_wl (jnz (l1 :> string)); lin ltl_map l2; lin ltl_map l1
          end
        else if not (Hashtbl.mem visited_labels l1) then
          begin
            need_label l2; emit_wl (jz (l2 :> string)); lin ltl_map l1; lin ltl_map l2
          end
        else
          begin
            need_label l1; need_label l2; emit_wl (jnz (l1 :> string)); emit_wl (jmp (l2 :> string))
          end
      | Mjlei i32 ->
        let op1 = operand op in
        emit l (cmpq (imm32 i32) op1);
        if not (Hashtbl.mem visited_labels l2) then
          begin
            need_label l1; emit_wl (jle (l1 :> string)); lin ltl_map l2; lin ltl_map l1
          end
        else if not (Hashtbl.mem visited_labels l1) then
          begin
            need_label l2; emit_wl (jg (l2 :> string)); lin ltl_map l1; lin ltl_map l2
          end
        else
          begin
            need_label l1; need_label l2; emit_wl (jle (l1 :> string)); emit_wl (jmp (l2 :> string))
          end
      | Mjgi i32 ->
        let op1 = operand op in
        emit l (cmpq (imm32 i32) op1);
        if not (Hashtbl.mem visited_labels l2) then
          begin
            need_label l1; emit_wl (jg (l1 :> string)); lin ltl_map l2; lin ltl_map l1
          end
        else if not (Hashtbl.mem visited_labels l1) then
          begin
            need_label l2; emit_wl (jle (l2 :> string)); lin ltl_map l1; lin ltl_map l2
          end
        else
          begin
            need_label l1; need_label l2; emit_wl (jg (l1 :> string)); emit_wl (jmp (l2 :> string))
          end
    end
  | Embbranch (branch, op1, op2, l1, l2) ->
    let op1 = operand op1 in
    let op2 = operand op2 in
    emit l (cmpq op1 op2);
    begin
      match branch with
      | Mjl ->
        if not (Hashtbl.mem visited_labels l2) then
          begin
            need_label l1; emit_wl (jl (l1 :> string)); lin ltl_map l2; lin ltl_map l1
          end
        else if not (Hashtbl.mem visited_labels l1) then
          begin
            need_label l2; emit_wl (jge (l2 :> string)); lin ltl_map l1; lin ltl_map l2
          end
        else
          begin
            need_label l1; need_label l2; emit_wl (jl (l1 :> string)); emit_wl (jmp (l2 :> string))
          end
      | Mjle ->
        if not (Hashtbl.mem visited_labels l2) then
          begin
            need_label l1; emit_wl (jle (l1 :> string)); lin ltl_map l2; lin ltl_map l1
          end
        else if not (Hashtbl.mem visited_labels l1) then
          begin
            need_label l2; emit_wl (jg (l2 :> string)); lin ltl_map l1; lin ltl_map l2
          end
        else
          begin
            need_label l1; need_label l2; emit_wl (jle (l1 :> string)); emit_wl (jmp (l2 :> string))
          end
    end
  | Epush (op, l1) ->
    let op = operand op in
    emit l (pushq op); lin ltl_map l1
  | Ecall (id, l1) ->
    let fun_name =
      try
        let fun_def = get_fun_entry id in
        fun_def.fun_name
      with
        Error _ ->
        if id = "sbrk" || id = "putchar" then id
        else raise (Error "Call to undefined function") in
    emit l (call fun_name); lin ltl_map l1
  | Epop (r, l1) ->
    emit l (popq (register64 r)); lin ltl_map l1

let necessary_label = function
  | Code _ -> true
  | Label l -> Hashtbl.mem labels l

let translate_function (fun_def : Ltltree.deffun) =
  code := [];
  lin fun_def.fun_body fun_def.fun_entry;
  code := List.rev !code;
  code := List.filter necessary_label !code;
  !code

let concatenate_asm_text asm_text = function
  | Code c -> asm_text ++ c
  | Label l -> asm_text ++ (label (l :> string))

let program (file : Ltltree.file) =
  funs := file.funs;
  let code_text = ref (nop ++ (globl "main")) in
  let add_function_code (fun_def : Ltltree.deffun) =
    let new_code_text_fragment = translate_function fun_def in
    code_text := !code_text ++ (label fun_def.fun_name);
    code_text := List.fold_left concatenate_asm_text !code_text new_code_text_fragment in
  List.iter add_function_code !funs;
  {text = !code_text; data = nop}
