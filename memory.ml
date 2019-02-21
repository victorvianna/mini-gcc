
exception Error of string
let error s = raise (Error s)
let seg_fault () = raise (Error "seg fault")

let word_size = 8

type ptr = int64
type value = int64

let zero = 0L
let one = 1L

type t = {
  mutable sbrk: ptr; (* next pointer available *)
  mem: (ptr, value array) Hashtbl.t;
}

let create () = {
  sbrk = 8L; (* 0 is reserved for null pointers *)
  mem = Hashtbl.create 65536;
}

let malloc m n =
  if n < 0 then invalid_arg "malloc";
  if n mod word_size <> 0 then invalid_arg "malloc";
  let p = m.sbrk in
  m.sbrk <- Int64.add p (Int64.of_int n); (* +1 would be OK as well *)
  Hashtbl.add m.mem p (Array.make (n / word_size) 0L);
  p

let index b ofs =
  if ofs mod word_size <> 0 then error "pointer not aligned";
  let i = ofs / word_size in
  if i < 0 || i >= Array.length b then error "access out of bounds";
  i

let get m p ~ofs =
  let b = try Hashtbl.find m.mem p with Not_found -> seg_fault () in
  let i = index b ofs in
  b.(i)

let set m p ~ofs v =
  let b = try Hashtbl.find m.mem p with Not_found -> seg_fault () in
  let i = index b ofs in
  b.(i) <- v



