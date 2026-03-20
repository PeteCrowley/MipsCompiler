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
    | Absyn.CallExp {func: Symbol.symbol, args: Absyn.exp list, ...} =>
        ( symbolEscapes (env, d, func)
        (* test all exps in list for escapes *)
        ; map (fn exp => traverseExp (env, d, exp)) args
        ; ()
        )
    | Absyn.OpExp {left: Absyn.exp, right: Absyn.exp, ...} =>
        (traverseExp (env, d, left); traverseExp (env, d, right))
    (* TODO: does this symbol get added to the env? *)
    | Absyn.RecordExp
        {fields: (Symbol.symbol * Absyn.exp * Absyn.pos) list, ...} =>
        (map (fn (_, exp, _) => traverseExp (env, d, exp)) fields; ())
    | Absyn.SeqExp exps =>
        (map (fn (exp, _) => traverseExp (env, d, exp)) exps; ())
    | Absyn.AssignExp {var: Absyn.var, exp: Absyn.exp, ...} =>
        (traverseVar (env, d, var); traverseExp (env, d, exp))
    | Absyn.IfExp
        {test: Absyn.exp, then': Absyn.exp, else': Absyn.exp option, ...} =>
        ( traverseExp (env, d, test)
        ; traverseExp (env, d, then')
        ; (case else' of
             SOME elseExp => traverseExp (env, d, elseExp)
           | NONE => ())
        )
    | Absyn.WhileExp {test: Absyn.exp, body: Absyn.exp, ...} =>
        (traverseExp (env, d, test); traverseExp (env, d, body))
    | Absyn.ForExp
        { var: Symbol.symbol
        , escape: bool ref
        , lo: Absyn.exp
        , hi: Absyn.exp
        , body: Absyn.exp
        , pos: Absyn.pos
        } =>
        (let
           val loopDec = Absyn.VarDec
             { name = var
             , escape = escape
             , typ = SOME (Symbol.symbol "int", pos)
             , init = lo
             , pos = pos
             }
           val forEnv = traverseDecs (env, d, [loopDec])
         in
           traverseExp (forEnv, d, hi);
           traverseExp (forEnv, d, body)
         end)
    | Absyn.BreakExp _ => ()
    | Absyn.LetExp {decs: Absyn.dec list, body: Absyn.exp, ...} =>
        (* let may redefine vars, but it still stays at the same depth *)
        let val newEnv = traverseDecs (env, d, decs)
        in traverseExp (newEnv, d, body)
        end
    | Absyn.ArrayExp
        (* the compiler will already know the size of this type so it should be
         * fine *)
        { size: Absyn.exp
        , init: Absyn.exp
        , ...
        } => (traverseExp (env, d, size); traverseExp (env, d, init))


  and traverseDecs (env: escEnv, d: depth, s :: rest : Absyn.dec list) : escEnv =
        let
          fun traverseFundec (env: escEnv, d: int, fundec: Absyn.fundec) : unit =
            let
              val funEnv =
                foldl
                  (fn (field, env) =>
                     Symbol.enter (env, #name field, (d, #escape field))) env
                  (#params fundec)
            in
              traverseExp (funEnv, d, #body fundec)
            end
        in
          (case s of
             Absyn.FunctionDec fundecs =>
               (* Increment depth level *)
               ( map (fn fundec => traverseFundec (env, d + 1, fundec)) fundecs
               ; env
               )
           | Absyn.VarDec
               { name: Symbol.symbol
               , escape: bool ref
               , typ: (Symbol.symbol * Absyn.pos) option
               , init: Absyn.exp
               , pos: Absyn.pos
               } =>
               ( traverseExp (env, d, init)
               ; Symbol.enter (env, name, (d, escape))
               )
           | Absyn.TypeDec _ => env)
        end
    | traverseDecs (env: escEnv, d: depth, []) = env

  fun findEscape (prog: Absyn.exp) : unit =
    let val escEnv: escEnv = Symbol.empty
    in traverseExp (escEnv, 0, prog)
    end
end
