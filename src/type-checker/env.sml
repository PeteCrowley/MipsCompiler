structure Env:
sig
  type ty
  type enventry = ty
  val base_tenv: ty Symbol.table (*predefined types*)
  val base_venv: enventry Symbol.table (*predefined functions*)
end =
struct
  type ty = Types.ty
  type enventry = ty
  val base_tenv: ty Symbol.table =
    let val table = Symbol.empty
    (* then here add all pre-existing types *)
    in table
    end
  val base_venv: enventry Symbol.table =
    let
      val table = Symbol.empty
      (*TODO: debug entries--remove after decs are working*)
      val debug_fun = Types.ARROW ([Types.STRING, Types.INT], Types.INT)
    (* then here add all stdlib functions in appendix a*)
    in
      (*TODO: debug entries--remove after decs are working*)
      Symbol.enter (table, Symbol.symbol "debug_int", Types.INT);
      Symbol.enter (table, Symbol.symbol "debug_str", Types.STRING);
      Symbol.enter (table, Symbol.symbol "debug_fun", debug_fun)
    end
end
