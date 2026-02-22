structure Semant:
sig
  val transProg: Absyn.exp -> unit

  type expty
  type venv
  type tenv

  val transVar: venv * tenv * Absyn.var -> expty
  val transExp: venv * tenv -> Absyn.exp -> expty
  val transDec: venv * tenv * Absyn.dec -> {venv: venv, tenv: tenv}
  val transTy: tenv * Absyn.ty -> Types.ty

end =
struct
  type expty = {exp: Translate.exp, ty: Types.ty}
  type venv = Env.enventry Symbol.table
  type tenv = Types.ty Symbol.table

  fun transProg exp = ()

  fun transVar (venv, tenv, var) = {exp = (), ty = Types.BOTTOM}

  fun transExp (venv, tenv) =
    let
      fun checkExp (Absyn.VarExp var) = {exp = (), ty = Types.BOTTOM}
        | checkExp Absyn.NilExp = {exp = (), ty = Types.NIL}
        | checkExp (Absyn.IntExp _) = {exp = (), ty = Types.INT}
        | checkExp (Absyn.StringExp (_, _)) = {exp = (), ty = Types.STRING}
        | checkExp
            (Absyn.CallExp
               {func: Absyn.symbol, args: Absyn.exp list, pos: Absyn.pos}) =
            (* {exp = (), ty = Symbol.look (venv, func)} *)
            { exp = ()
            , ty = Types.BOTTOM
            }
        | checkExp
            (Absyn.OpExp
               { left: Absyn.exp
               , oper: Absyn.oper
               , right: Absyn.exp
               , pos: Absyn.pos
               }) = {exp = (), ty = Types.NIL}
        | checkExp
            (Absyn.RecordExp
               { fields: (Absyn.symbol * Absyn.exp * Absyn.pos) list
               , typ: Absyn.symbol
               , pos: Absyn.pos
               }) = {exp = (), ty = Types.NIL}
        | checkExp (Absyn.SeqExp explist) = {exp = (), ty = Types.NIL}
        | checkExp
            (Absyn.AssignExp {var: Absyn.var, exp: Absyn.exp, pos: Absyn.pos}) =
            {exp = (), ty = Types.UNIT}
        | checkExp
            (Absyn.IfExp
               { test: Absyn.exp
               , then': Absyn.exp
               , else': Absyn.exp option
               , pos: Absyn.pos
               }) = {exp = (), ty = Types.NIL}
        | checkExp
            (Absyn.WhileExp {test: Absyn.exp, body: Absyn.exp, pos: Absyn.pos}) =
            {exp = (), ty = Types.NIL}
        | checkExp
            (Absyn.ForExp
               { var: Absyn.symbol
               , escape: bool ref
               , lo: Absyn.exp
               , hi: Absyn.exp
               , body: Absyn.exp
               , pos: Absyn.pos
               }) = {exp = (), ty = Types.NIL}
        | checkExp (Absyn.BreakExp _) = {exp = (), ty = Types.BOTTOM}
        | checkExp
            (Absyn.LetExp
               {decs: Absyn.dec list, body: Absyn.exp, pos: Absyn.pos}) =
            {exp = (), ty = Types.NIL}
        | checkExp
            (Absyn.ArrayExp
               { typ: Absyn.symbol
               , size: Absyn.exp
               , init: Absyn.exp
               , pos: Absyn.pos
               }) = {exp = (), ty = Types.NIL}
    (* | checkExp _ = {exp = (), ty = Types.BOTTOM} *)
    in
      checkExp
    end

  fun transDec (venv, tenv, dec) = {venv = venv, tenv = tenv}
  fun transTy (tenv, ty) = Types.BOTTOM
end
