structure Env:
sig
  type ty
  (* should we separate functions like appel or use the Arrow type like Drew? *)
  datatype enventry =
    VarEntry of {ty: ty}
  | FunEntry of {formals: ty list, result: ty}
  val base_tenv: ty Symbol.table (*predefined types*)
  val base_venv: enventry Symbol.table (*predefined functions*)
end =
struct
  type ty = Types.ty
  datatype enventry =
    VarEntry of {ty: ty}
  | FunEntry of {formals: ty list, result: ty}
  val base_tenv: ty Symbol.table =
    let val table = Symbol.empty
    (* then here add all pre-existing types *)
    in table
    end
  val base_venv: enventry Symbol.table =
    let val table = Symbol.empty
    (* then here add all stdlib functions in appendix a*)
    in table
    end
end
