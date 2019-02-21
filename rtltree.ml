
(** {2 Register Transfer Language} *)

open Ops

type register = Register.t

type label = Label.t

(** Les différents instructions RTL.
    Chaque instruction contient la ou les étiquettes
    suivantes dans le graphe de flot de contrôle. *)
type instr =
  | Econst of int32 * register * label
  | Eload of register * int * register * label
  | Estore of register * register * int * label
  | Emunop of munop * register * label
  | Embinop of mbinop * register * register * label
    (** attention au sens : [op r1 r2] représente [r2 <- r2 op r1] *)
  | Emubranch of mubranch * register * label * label
  | Embbranch of mbbranch * register * register * label * label
    (** attention au sens : [br r1 r2] représente [r2 br r1] *)
  | Ecall of register * string * register list * label
  | Egoto of label

type cfg = instr Label.map
  (** Un graphe de flot de contrôle est un dictionnaire associant à des
      étiquettes des instructions RTL. *)

(** Une fonction RTL. *)
type deffun = {
  fun_name   : string;
  fun_formals: register list;
  fun_result : register;
  fun_locals : Register.set;
  (** toutes les variables locales de la fonction maintenant regroupées ici *)
  fun_entry  : label;
  fun_exit   : label;
  fun_body   : cfg;
}

(** Un programme RTL. *)
type file = {
  funs : deffun list;
}

(** {2 Fonctions d'impression, pour debugger} *)

open Format
open Pp

let print_instr fmt = function
  | Econst (n, r, l) ->
      fprintf fmt "mov $%ld %a  --> %a" n Register.print r Label.print l
  | Eload (r1, n, r2, l) ->
      fprintf fmt "mov %d(%a) %a  --> %a"
        n Register.print r1 Register.print r2 Label.print l
  | Estore (r1, r2, n, l) ->
      fprintf fmt "mov %a %d(%a)  --> %a"
        Register.print r1 n Register.print r2 Label.print l
  | Emunop (op, r1, l) ->
      fprintf fmt "%a %a  --> %a" print_munop op
        Register.print r1 Label.print l
  | Embinop (Mmov, r1, r2, l) ->
      fprintf fmt "mov %a %a  --> %a"
        Register.print r1 Register.print r2 Label.print l
  | Embinop (op, r1, r2, l) ->
      fprintf fmt "%a %a %a  --> %a"
	print_mbinop op Register.print r1 Register.print r2 Label.print l
  | Emubranch (op, r, l1, l2) ->
      fprintf fmt "%a %a  --> %a, %a"
	print_mubranch op Register.print r Label.print l1 Label.print l2
  | Embbranch (op, r1, r2, l1, l2) ->
      fprintf fmt "%a %a %a  --> %a, %a"
	print_mbbranch op
        Register.print r1 Register.print r2 Label.print l1 Label.print l2
  | Ecall (r, x, rl, l) ->
      fprintf fmt "%a <- call %s(@[%a@])  --> %a"
	Register.print r x (print_list comma Register.print) rl Label.print l
  | Egoto l ->
      fprintf fmt "goto %a" Label.print l

let print_graph fmt (g: cfg) (entry: label) (exit: label) =
  let visited = Hashtbl.create 97 in
  let rec visit l =
    if not (Hashtbl.mem visited l) && l <> exit then begin
      Hashtbl.add visited l ();
      let i = Label.M.find l g in
      fprintf fmt "%a: %a@\n" Label.print l print_instr i;
      match i with
	| Econst (_,_,l)
	| Eload (_,_,_,l)
	| Estore (_,_,_,l)
	| Emunop (_,_,l)
	| Embinop (_,_,_,l)
	| Ecall (_,_,_,l)
	| Egoto l ->
	    visit l
	| Emubranch (_,_,l1,l2)
	| Embbranch (_,_,_,l1,l2) ->
	    visit l1; visit l2
    end
  in
  visit entry

let print_deffun fmt f =
  fprintf fmt "%a %s(@[%a@])@\n" Register.print f.fun_result f.fun_name
    (print_list comma Register.print) f.fun_formals;
  fprintf fmt "  @[";
  fprintf fmt "entry : %a@\n" Label.print f.fun_entry;
  fprintf fmt "exit  : %a@\n" Label.print f.fun_exit;
  fprintf fmt "locals: @[%a@]@\n" Register.print_set f.fun_locals;
  print_graph fmt f.fun_body f.fun_entry f.fun_exit;
  fprintf fmt "@]@."

let print_file fmt p =
  fprintf fmt "=== RTL ==================================================@\n";
  List.iter (print_deffun fmt) p.funs
