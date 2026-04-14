structure Env:
sig
  type ty
  datatype enventry =
    VarEntry of {access: Translate.access, ty: ty}
  | FunEntry of {level: Translate.level, label: Temp.label, ty: ty}
  val base_tenv: ty Symbol.table (*predefined types*)
  val base_venv: enventry Symbol.table (*predefined functions*)
  val getFunEntry: Translate.level * ty * bool list -> enventry
  val getNamedFunEntry: Translate.level * ty * bool list * string -> enventry
end =
struct
  type ty = Types.ty
  datatype enventry =
    VarEntry of {access: Translate.access, ty: ty}
  | FunEntry of {level: Translate.level, label: Temp.label, ty: ty}
  fun addPairToTable ((sym, value), table) = Symbol.enter (table, sym, value)
  fun getFunEntry (parentLevel, ty, formals) =
    let
      val funLabel = Temp.newlabel ()
    in
      FunEntry
        { level =
            Translate.newLevel
              {parent = parentLevel, name = funLabel, formals = formals}
        , label = funLabel
        , ty = ty
        }
    end

  fun getNamedFunEntry (parentLevel, ty, formals, name) =
    let
      val funLabel = Temp.namedlabel name
    in
      FunEntry
        { level =
            Translate.newLevel
              {parent = parentLevel, name = funLabel, formals = formals}
        , label = funLabel
        , ty = ty
        }
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
  val base_venv: enventry Symbol.table =
    let
      val table = Symbol.empty
      val pairs =
        [ ( Symbol.symbol "print"
          , getNamedFunEntry
              ( Translate.outermost
              , Types.ARROW ([Types.STRING], Types.UNIT)
              , [false]
              , "tig_print"
              )
          )
        , ( Symbol.symbol "flush"
          , getNamedFunEntry
              ( Translate.outermost
              , Types.ARROW ([], Types.UNIT)
              , []
              , "tig_flush"
              )
          )
        , ( Symbol.symbol "getchar"
          , getNamedFunEntry
              ( Translate.outermost
              , Types.ARROW ([], Types.STRING)
              , []
              , "tig_getchar"
              )
          )
        , ( Symbol.symbol "ord"
          , getNamedFunEntry
              ( Translate.outermost
              , Types.ARROW ([Types.STRING], Types.INT)
              , [false]
              , "tig_ord"
              )
          )
        , ( Symbol.symbol "chr"
          , getNamedFunEntry
              ( Translate.outermost
              , Types.ARROW ([Types.INT], Types.STRING)
              , [false]
              , "tig_chr"
              )
          )
        , ( Symbol.symbol "size"
          , getNamedFunEntry
              ( Translate.outermost
              , Types.ARROW ([Types.STRING], Types.INT)
              , [false]
              , "tig_size"
              )
          )
        , ( Symbol.symbol "substring"
          , getNamedFunEntry
              ( Translate.outermost
              , Types.ARROW ([Types.STRING, Types.INT, Types.INT], Types.STRING)
              , [false, false, false]
              , "tig_substring"
              )
          )
        , ( Symbol.symbol "concat"
          , getNamedFunEntry
              ( Translate.outermost
              , Types.ARROW ([Types.STRING, Types.STRING], Types.STRING)
              , [false, false]
              , "tig_concat"
              )
          )
        , ( Symbol.symbol "not"
          , getNamedFunEntry
              ( Translate.outermost
              , Types.ARROW ([Types.INT], Types.INT)
              , [false]
              , "tig_not"
              )
          )
        , ( Symbol.symbol "exit"
          , getNamedFunEntry
              ( Translate.outermost
              , Types.ARROW ([Types.INT], Types.UNIT)
              , [false]
              , "tig_exit"
              )
          )
        ]
    in
      foldl addPairToTable table pairs
    end
end
