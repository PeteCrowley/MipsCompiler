structure Env:
sig
  type ty
  type enventry
  val base_tenv: ty Symbol.table (*predefined types*)
  val base_venv: ((enventry Symbol.table) * bool) (*predefined functions*)
  val getFunEntry: Translate.level * ty * bool list -> enventry
end =
struct
  type ty = Types.ty
  datatype enventry = VarEntry of {access: Translate.access, ty: ty}
                | FunEntry of {level: Translate.level, label: Temp.label, ty: ty}
  fun addPairToTable ((sym, value), table) = Symbol.enter (table, sym, value)
  fun getFunEntry (parentLevel, ty, formals) =
    let
      val funLabel = Temp.newlabel()
    in
      FunEntry {level = Translate.newLevel {
        parent = parentLevel, name = funLabel, formals = formals
    }, label = funLabel, ty = ty}
    end
    
  val base_tenv: ty Symbol.table =
    let
      val table = Symbol.empty
      val pairs =
        [ (Symbol.symbol "int", Types.INT)
        , (Symbol.symbol "string", Types.STRING)
        ]
    in
      foldl addPairToTable table pairs
    end
  val base_venv: ((enventry Symbol.table) * bool) =
    let
      val table = Symbol.empty
      val pairs =
        [ (Symbol.symbol "print", getFunEntry (Translate.outermost, Types.ARROW ([Types.STRING], Types.UNIT), [false]))
        , (Symbol.symbol "flush", getFunEntry (Translate.outermost, Types.ARROW ([], Types.UNIT), []))
        , (Symbol.symbol "getchar", getFunEntry (Translate.outermost, Types.ARROW ([], Types.STRING), []))
        , (Symbol.symbol "ord", getFunEntry (Translate.outermost, Types.ARROW ([Types.STRING], Types.INT), [false]))
        , (Symbol.symbol "chr", getFunEntry (Translate.outermost, Types.ARROW ([Types.INT], Types.STRING), [false]))
        , (Symbol.symbol "size", getFunEntry (Translate.outermost, Types.ARROW ([Types.STRING], Types.INT), [false]))
        , ( Symbol.symbol "substring"
          , getFunEntry (Translate.outermost, Types.ARROW ([Types.STRING, Types.INT, Types.INT], Types.STRING), [false, false, false])
          )
        , ( Symbol.symbol "concat"
          , getFunEntry (Translate.outermost, Types.ARROW ([Types.STRING, Types.STRING], Types.STRING), [false, false])
          )
        , (Symbol.symbol "not", getFunEntry (Translate.outermost, Types.ARROW ([Types.INT], Types.INT), [false]))
        , (Symbol.symbol "exit", getFunEntry (Translate.outermost, Types.ARROW ([Types.INT], Types.UNIT), [false]))
        ]
    in
      ((foldl addPairToTable table pairs), false)
    end
end
