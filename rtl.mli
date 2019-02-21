open Rtltree

val program: Ttree.file -> Rtltree.file 

val deffun: Ttree.decl_fun -> Rtltree.deffun

val generate: Rtltree.instr -> label

val expr: Ttree.expr -> register -> label -> label  

val stmt: Ttree.stmt -> label -> register -> label -> label


val deffun: Ttree.decl_fun -> Rtltree.deffun




