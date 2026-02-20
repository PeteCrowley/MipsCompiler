structure Env:
sig
  type ty
  (* should we separate functions like appel or use the Arrow type like Drew? *)
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
    let val table = Symbol.empty
    (* then here add all stdlib functions in appendix a*)
    in table
    end
end
