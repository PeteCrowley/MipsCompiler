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
  fun addPairToTable ((sym, value), table) = Symbol.enter (table, sym, value)
  val base_tenv: ty Symbol.table =
    let val table = Symbol.empty
        val pairs = [
          (Symbol.symbol "int", Types.INT),
          (Symbol.symbol "string", Types.STRING)
          ]
    in foldl addPairToTable table pairs
    end
  val base_venv: enventry Symbol.table =
    let val table = Symbol.empty
        val pairs = [
          (Symbol.symbol "print", Types.ARROW ([Types.STRING], Types.UNIT)),
          (Symbol.symbol "flush", Types.ARROW ([], Types.UNIT)),
          (Symbol.symbol "getchar", Types.ARROW ([], Types.STRING)),
          (Symbol.symbol "ord", Types.ARROW ([Types.STRING], Types.INT)),
          (Symbol.symbol "chr", Types.ARROW ([Types.INT], Types.STRING)),
          (Symbol.symbol "size", Types.ARROW ([Types.STRING], Types.INT)),
          (Symbol.symbol "substring", Types.ARROW ([Types.STRING, Types.INT, Types.INT], Types.STRING)),
          (Symbol.symbol "concat", Types.ARROW ([Types.STRING, Types.STRING], Types.STRING)),
          (Symbol.symbol "not", Types.ARROW ([Types.INT], Types.INT)),
          (Symbol.symbol "exit", Types.ARROW ([Types.INT], Types.UNIT)),
          (Symbol.symbol "debug_int", Types.INT),
          (Symbol.symbol "debug_str", Types.STRING),
          (Symbol.symbol "debug_fun", Types.ARROW ([Types.STRING, Types.INT], Types.INT))
        ]
    in foldl addPairToTable table pairs
    end
end
