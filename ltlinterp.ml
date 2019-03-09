
open Format
open Ops
open Memory
open Ltltree

let zero = 0L
let one = 1L

exception Error of string
let error s = raise (Error s)

type state = {
     mem: Machine.t;
  fundef: (string, deffun) Hashtbl.t;
}

let load st = Machine.load st.mem
let store st = Machine.store st.mem
let getr st = Machine.get st.mem
let setr st = Machine.set st.mem
let push st = Machine.push st.mem
let pop st = Machine.pop st.mem
let push_register st = Machine.push_register st.mem
let pop_in_register st = Machine.pop_in_register st.mem
let rsp = Register.rsp and rbp = Register.rbp

let no_pseudo r =
  error ("pseudo register " ^ r ^ " not allowed in LTL code")

let get st = function
  | Spilled ofs -> load st ~ofs (getr st rbp)
  | Reg r when Register.is_pseudo r -> no_pseudo (r :> string)
  | Reg r -> getr st r

let set st o v = match o with
  | Spilled ofs -> store st ~ofs (getr st rbp) v
  | Reg r when Register.is_pseudo r -> no_pseudo (r :> string)
  | Reg r -> setr st r v

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
    | Mdiv when r2 <> Reg Register.rax -> error "div: r2 must be %rax"
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
    let p = getr st r1 in
    let v = load st ~ofs p in
    setr st r2 v;
    exec st gr l
  | Estore (r1, r2, ofs, l) ->
    let p = getr st r2 in
    let v = getr st r1 in
    store st ~ofs p v;
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
  | Ecall ("sbrk", l) ->
    let n = getr st Register.rdi in
    let v = Machine.malloc st.mem (Int64.to_int n) in
    setr st Register.rax v;
    exec st gr l
  | Ecall ("putchar", l) ->
    let n = getr st Register.rdi in
    Format.printf "%c" (Char.chr (Int64.to_int n));
    setr st Register.rax n;
    exec st gr l
  | Ecall (x, l) ->
    call st x;
    exec st gr l
  | Egoto l ->
    exec st gr l
  | Epush (r, l) ->
    let v = get st r in
    push st v;
    exec st gr l
  | Epop (r, l) ->
    let v = pop st in
    setr st r v;
    exec st gr l
  | Ereturn ->
    ignore (pop st) (* dépile l'adresse de retour fictive *)

and call st x =
  let f =
    try Hashtbl.find st.fundef x
    with Not_found -> error ("unknown function " ^ x) in
  push st 0L; (* adresse de retour fictive *)
  exec st f.fun_body f.fun_entry

let program p =
  let fundef = Hashtbl.create 16 in
  List.iter (fun d -> Hashtbl.add fundef d.fun_name d) p.funs;
  let st = { mem = Machine.create (); fundef; } in
  call st "main";
  getr st Register.rax
