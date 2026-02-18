signature Env =
sig
  type access
  type ty
  datatype enventry =
    ty (*not sure if this is necessary now that functions are
        also types*)
  val base_tenv: ty Symbol.table (*predefined types*)
  val base_venv: enventry Symbol.table (*predefined functions*)
end
