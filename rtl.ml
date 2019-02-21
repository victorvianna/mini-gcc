open Rtltree

let graph = ref Label.M.empty

let generate i =
  let l = Label.fresh () in
  graph := Label.M.add l i !graph;
  l


let program (ttree:Ttree.file) =
  {funs = []}



