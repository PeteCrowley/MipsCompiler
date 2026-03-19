structure FindEscape:
sig
  val findEscape: Absyn.exp -> unit
end =
struct
  type depth = int
  type escEnv = (depth * bool ref) Symbol.table

  fun symbolEscapes (env: escEnv, d: depth, sym: Symbol.symbol) : unit =
    case Symbol.look (env, sym) of
      SOME (envDepth, escapes) => (if d > envDepth then escapes := true else ())
    | NONE => ()

  fun traverseVar (env: escEnv, d: depth, s: Absyn.var) : unit =
    case s of
      Absyn.SimpleVar (sym, _) => symbolEscapes (env, d, sym)
    (* a.b.c.d, we should only worry about a escaping? *)
    | Absyn.FieldVar (var, _, _) => traverseVar (env, d, var)
    (* a[b[0]], we need to check both b and a for escape *)
    | Absyn.SubscriptVar (var, exp, _) =>
        (traverseExp (env, d, exp); traverseVar (env, d, var))

  and traverseExp (env: escEnv, d: depth, s: Absyn.exp) : unit =
    case s of
      Absyn.VarExp var => traverseVar (env, d, var)
    | Absyn.NilExp => ()
    | Absyn.IntExp _ => ()
    | Absyn.StringExp _ => ()
    | Absyn.CallExp {func: Symbol.symbol, args: Absyn.exp list, pos: Absyn.pos} =>
        ( symbolEscapes (env, d, func)
        (* test all exps in list for escapes *)
        ; map (fn exp => traverseExp (env, d, exp)) args
        ; ()
        )
    | Absyn.OpExp
        {left: Absyn.exp, oper: Absyn.oper, right: Absyn.exp, pos: Absyn.pos} =>
        (traverseExp (env, d, left); traverseExp (env, d, right))
    (* TODO: does this symbol get added to the env? *)
    | Absyn.RecordExp
        { fields: (Symbol.symbol * Absyn.exp * Absyn.pos) list
        , typ: Symbol.symbol
        , pos: Absyn.pos
        } => (map (fn (_, exp, _) => traverseExp (env, d, exp)) fields; ())
    | Absyn.SeqExp exps =>
        (map (fn (exp, _) => traverseExp (env, d, exp)) exps; ())
    | Absyn.AssignExp {var: Absyn.var, exp: Absyn.exp, pos: Absyn.pos} =>
        (traverseVar (env, d, var); traverseExp (env, d, exp))
    | Absyn.IfExp
        { test: Absyn.exp
        , then': Absyn.exp
        , else': Absyn.exp option
        , pos: Absyn.pos
        } =>
        ( traverseExp (env, d, test)
        ; traverseExp (env, d, then')
        ; (case else' of
             SOME elseExp => traverseExp (env, d, elseExp)
           | NONE => ())
        )
    | Absyn.WhileExp {test: Absyn.exp, body: Absyn.exp, pos: Absyn.pos} =>
        (traverseExp (env, d, test); traverseExp (env, d, body))
    (* | Absyn.ForExp of *)
    (*     {var: symbol, escape: bool ref, lo: Absyn.exp, hi: Absyn.exp, body: Absyn.exp, pos: Absyn.pos} *)
    | Absyn.BreakExp _ => ()
  (* | Absyn.LetExp of {decs: dec list, body: Absyn.exp, pos: Absyn.pos} *)
  (* | Absyn.ArrayExp of {typ: symbol, size: Absyn.exp, init: Absyn.exp, pos: Absyn.pos} *)

  and traverseDecs (env: escEnv, d: depth, s: Absyn.dec list) : escEnv = env

  fun findEscape (prog: Absyn.exp) : unit =
    let val escEnv: escEnv = Symbol.empty
    in traverseExp (escEnv, 0, prog)
    end
end
