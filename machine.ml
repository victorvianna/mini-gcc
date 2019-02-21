
module H = Hashtbl

exception Error of string
let error s = raise (Error s)
let seg_fault () = raise (Error "seg fault")

let word_size = 8

type ptr = int64
type value = int64

let zero = 0L
let null = zero
let one = 1L

let rsp = Register.rsp

type t = {
  sbrk: ptr ref; (* "ref" pour partager ce pointeur entre différents appels *)
  mem: value array;
  maxptr: ptr;
  (* on distingue registres physiques et pseudos,
     car un appel doit sauvegarder les pseudo-registres *)
  regs: (Register.t, value) H.t;
  hwregs: (Register.t, value) H.t;
}

let create ?(words=65536) () =
  let hwregs = H.create 16 in
  let maxptr = Int64.of_int (word_size * words) in
  H.add hwregs Register.rsp maxptr;
  { sbrk = ref 8L; (* 0 réservé pour null *)
    mem = Array.make words 0L;
    maxptr = maxptr;
    regs = H.create 16;
    hwregs = hwregs; }

let fresh_registers m =
  { m with regs = H.create 16 }

let malloc m n =
  if n < 0 then invalid_arg "malloc";
  if n mod word_size <> 0 then invalid_arg "malloc";
  let p = !(m.sbrk) in
  m.sbrk := Int64.add p (Int64.of_int n);
  p

let get st r =
 if Register.is_hw r then
   try H.find st.hwregs r with Not_found -> zero
 else
   try H.find st.regs r
   with Not_found -> error ("unknown register " ^ (r :> string))

let set st r v =
  H.replace (if Register.is_hw r then st.hwregs else st.regs) r v

let index m ptr ofs =
  let ptr = Int64.add ptr (Int64.of_int ofs) in
  if ptr <= zero || (ptr >= !(m.sbrk) && ptr < get m rsp) || ptr >= m.maxptr
  then seg_fault ();
  let i = Int64.to_int ptr in
  if i mod word_size <> 0 then error "pointer not aligned";
  i / word_size

let load m p ~ofs =
  let i = index m p ofs in
  m.mem.(i)

let store m p ~ofs v =
  let i = index m p ofs in
  m.mem.(i) <- v

let push m v =
  let ptr = Int64.sub (get m rsp) 8L in
  set m rsp ptr;
  store m ptr ~ofs:0 v

let pop m =
  let ptr = get m rsp in
  let v = load m ptr ~ofs:0 in
  set m rsp (Int64.add ptr 8L);
  v

let push_register m r =
  push m (get m r)

let pop_in_register m r =
  set m r (pop m)
