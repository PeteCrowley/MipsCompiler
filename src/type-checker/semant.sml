structure Semant:
sig

  val transProg: Absyn.exp -> unit
end =
struct
  (* type venv = Env.enventry Symbol.table *)
  type tenv = Types.ty Symbol.table
  fun transProg exp = ()
end
