
open Format
open Ops
open Memory
open Rtltree

type state = {
  mem: Memory.t;
  fundef: (string, deffun) Hashtbl.t;
  regs: (Register.t, Memory.value) Hashtbl.t;
}

exception Error of string
let error s = raise (Error s)

let get st r =
  try Hashtbl.find st.regs r
  with Not_found -> error ("unknown register " ^ (r :> string))

let set st r v = Hashtbl.replace st.regs r v

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
    | Mdiv -> Int64.div v2 v1
    | Msete  -> bool (v2 =  v1)
    | Msetne -> bool (v2 <> v1)
    | Msetl  -> bool (v2 <  v1)
    | Msetle -> bool (v2 <= v1)
    | Msetg  -> bool (v2 >  v1)
    | Msetge -> bool (v2 >= v1) in
  set st r2 v2

let rec exec st gr exitl l =
  if l <> exitl then
  let i =
    try Label.M.find l gr
    with Not_found -> error ("unknown label " ^ (l :> string )) in
  match i with
  | Econst (n, r, l) ->
    set st r (Int64.of_int32 n);
    exec st gr exitl l
  | Eload (r1, ofs, r2, l) ->
    let p = get st r1 in
    let v = Memory.get st.mem p ~ofs in
    set st r2 v;
    exec st gr exitl l
  | Estore (r1, r2, ofs, l) ->
    let p = get st r2 in
    let v = get st r1 in
    Memory.set st.mem p ~ofs v;
    exec st gr exitl l
  | Emunop (op, r1, l) ->
    unop st op r1;
    exec st gr exitl l
  | Embinop (Mmov, r1, r2, l) ->
    let v1 = get st r1 in
    set st r2 v1;
    exec st gr exitl l
  | Embinop (op, r1, r2, l) ->
    binop st op r1 r2;
    exec st gr exitl l
  | Emubranch (op, r, l1, l2) ->
    let v = get st r in
    let b = match op with
      | Mjz     -> v = zero
      | Mjnz    -> v <> zero
      | Mjlei n -> v <= Int64.of_int32 n
      | Mjgi n  -> v > Int64.of_int32 n in
    exec st gr exitl (if b then l1 else l2)
  | Embbranch (op, r1, r2, l1, l2) ->
    let v1 = get st r1 in
    let v2 = get st r2 in
    let b = match op with
      | Mjl  -> v2 < v1
      | Mjle -> v2 <= v1 in
    exec st gr exitl (if b then l1 else l2)
  | Ecall (r1, "sbrk", [r2], l) ->
    let n = get st r2 in
    let v = Memory.malloc st.mem (Int64.to_int n) in
    set st r1 v;
    exec st gr exitl l
  | Ecall (r1, "putchar", [r2], l) ->
    let n = get st r2 in
    Format.printf "%c" (Char.chr (Int64.to_int n));
    set st r1 n;
    exec st gr exitl l
  | Ecall (r, x, rl, l) ->
    let v = call st x rl in
    set st r v;
    exec st gr exitl l
  | Egoto l ->
    exec st gr exitl l

and call st x rl =
  let f =
    try Hashtbl.find st.fundef x
    with Not_found -> error ("unknown function " ^ x) in
  let stf = { st with regs = Hashtbl.create 16 } in
  Register.S.iter (fun r -> set stf r zero) f.fun_locals;
  List.iter2 (fun p r -> set stf p (get st r)) f.fun_formals rl;
  exec stf f.fun_body f.fun_exit f.fun_entry;
  try Hashtbl.find stf.regs f.fun_result
  with Not_found -> zero (* pas de return => valeur arbitraire *)

let program p =
  let fundef = Hashtbl.create 16 in
  List.iter (fun d -> Hashtbl.add fundef d.fun_name d) p.funs;
  let st = { mem = Memory.create (); fundef; regs = Hashtbl.create 1 } in
  call st "main" []
