open Ertltree

let graph = ref Label.M.empty

let generate i = (* should move this to a common file later *)
  let l = Label.fresh () in
  graph := Label.M.add l i !graph;

let instr = function
| Rtltree.Econst (r, n, destl) ->
  Econst (r, n, destl)
| Rtltree.Eload (r1, d, r2, destl) ->
  Eload (r1, d, r2, destl)
| Rtltree.Estore (r1, r2, d, destl) ->
  Estore (r1, r2, d, destl)
| Rtltree.Emunop (op, r, destl) ->
  Emunop (op, r, destl)
| Rtltree.Embinop (op, r1, r2, destl) ->
begin
  match op with
  | Mdiv ->
    generate (Embinop (Mmov, r2, Register.rax, destl))
  | _ ->
    Embinop (op, r1, r2, destl)
end
| Rtltree.Emubranch (mubranch, r, l1, l2) ->
  Emubranch (mubranch, r, l1, l2)
| Rtltree.Embbranch (mbbranch, r1, r2, l1, l2) ->
  Embbranch (mbbranch, r1, r2, l1, l2)
| Rtltree.Egoto (destl) ->
  Egoto (l)

let program (rtltree:Rtltree.file) :Ertltree.file =
  {funs = []}