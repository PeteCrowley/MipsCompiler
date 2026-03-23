signature FRAME =
sig
  type frame
  type access
  datatype frag =
    PROC of {body: Tree.stm, frame: frame}
  | STRING of Temp.label * string
  val newFrame: {name: Temp.label, formals: bool list} -> frame
  val name: frame -> Temp.label
  val formals: frame -> access list
  val allocLocal: frame -> bool -> access
  val FP: Temp.temp (* frame pointer register *)
  val RV: Temp.temp (* return value register *)
  val wordsize: int
  val exp: access -> Tree.exp -> Tree.exp
  val addPrologueEpliogue: frame * Tree.exp -> Tree.stm
  val externalCall: string * Tree.exp list -> Tree.exp
(* Will need to add the string function below eventually, think I can omit it for now tho - Pete *)
(* val string : string -> Tree.exp *)
end
