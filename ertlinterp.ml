
open Format
open Ops
open Ertltree

type state = {
  mem: Machine.t;
  fundef: (string, deffun) Hashtbl.t;
}

exception Error of string
let error s = raise (Error s)

let zero = 0L
let one = 1L

let load st = Machine.load st.mem
let store st = Machine.store st.mem
let get st = Machine.get st.mem
let set st = Machine.set st.mem
let push st = Machine.push st.mem
let push_register st = Machine.push_register st.mem
let pop_in_register st = Machine.pop_in_register st.mem
let rsp = Register.rsp and rbp = Register.rbp

let bool b = if b then one else zero

let unop st op r =
  let v = get st r in
  let v = match op with
    | Maddi n -> Int64.add (Int64.of_int32 n) v
    | Msetei n -> bool (Int64.of_int32 n = v)
    | Msetnei n -> bool (Int64.of_int32 n <> v) in
  set st r v

let binop st op r1 r2 =
  let v1 = get st r1 in
  let v2 = get st r2 in
  let v2 = match op with
    | Mmov -> assert false
    | Madd -> Int64.add v2 v1
    | Msub -> Int64.sub v2 v1
    | Mmul -> Int64.mul v2 v1
    | Mdiv when r2 <> Register.rax -> error "div: r2 must be %rax"
    | Mdiv -> Int64.div v2 v1
    | Msete  -> bool (v2 =  v1)
    | Msetne -> bool (v2 <> v1)
    | Msetl  -> bool (v2 <  v1)
    | Msetle -> bool (v2 <= v1)
    | Msetg  -> bool (v2 >  v1)
    | Msetge -> bool (v2 >= v1) in
  set st r2 v2

let rec exec st gr l =
  let i =
    try Label.M.find l gr
    with Not_found -> error ("unknown label " ^ (l :> string )) in
  match i with
  | Econst (n, r, l) ->
    set st r (Int64.of_int32 n);
    exec st gr l
  | Eload (r1, ofs, r2, l) ->
    let p = get st r1 in
    let v = Machine.load st.mem p ~ofs in
    set st r2 v;
    exec st gr l
  | Estore (r1, r2, ofs, l) ->
    let p = get st r2 in
    let v = get st r1 in
    Machine.store st.mem p ~ofs v;
    exec st gr l
  | Emunop (op, r1, l) ->
    unop st op r1;
    exec st gr l
  | Embinop (Mmov, r1, r2, l) ->
    let v1 = get st r1 in
    set st r2 v1;
    exec st gr l
  | Embinop (op, r1, r2, l) ->
    binop st op r1 r2;
    exec st gr l
  | Emubranch (op, r, l1, l2) ->
    let v = get st r in
    let b = match op with
      | Mjz     -> v = zero
      | Mjnz    -> v <> zero
      | Mjlei n -> v <= Int64.of_int32 n
      | Mjgi n  -> v > Int64.of_int32 n in
    exec st gr (if b then l1 else l2)
  | Embbranch (op, r1, r2, l1, l2) ->
    let v1 = get st r1 in
    let v2 = get st r2 in
    let b = match op with
      | Mjl  -> v2 < v1
      | Mjle -> v2 <= v1 in
    exec st gr (if b then l1 else l2)
  | Ecall ("sbrk", _, l) ->
    let n = get st Register.rdi in
    let v = Machine.malloc st.mem (Int64.to_int n) in
    set st Register.rax v;
    exec st gr l
  | Ecall ("putchar", _, l) ->
    let n = get st Register.rdi in
    Format.printf "%c" (Char.chr (Int64.to_int n));
    set st Register.rax n;
    exec st gr l
  | Ecall (x, _, l) ->
    call st x;
    exec st gr l
  | Egoto l ->
    exec st gr l
  | Ealloc_frame l ->
    push_register st rbp;
    set st rbp (get st rsp);
    exec st gr l
  | Edelete_frame l ->
    pop_in_register st rbp;
    exec st gr l
  | Eget_param (ofs, r, l) ->
    set st r (load st (get st rbp) ~ofs);
    exec st gr l
  | Epush_param (r, l) ->
    push st (get st r);
    exec st gr l
  | Ereturn ->
    ignore (Machine.pop st.mem); (* dépile l'adresse de retour fictive *)
    ()

and call st x =
  let f =
    try Hashtbl.find st.fundef x
    with Not_found -> error ("unknown function " ^ x) in
  Machine.push st.mem zero; (* adresse de retour fictive *)
  let stf = { st with mem = Machine.fresh_registers st.mem } in
  Register.S.iter (fun r -> set stf r zero) f.fun_locals;
  exec stf f.fun_body f.fun_entry

let program p =
  let fundef = Hashtbl.create 16 in
  List.iter (fun d -> Hashtbl.add fundef d.fun_name d) p.funs;
  let st = { mem = Machine.create (); fundef; } in
  call st "main";
  get st Register.rax
