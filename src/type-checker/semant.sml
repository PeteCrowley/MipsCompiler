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

  fun checkInt ({exp, ty}, pos) = case ty of 
                                    Types.INT => ()
                                    | _ => ErrorMsg.error pos "expected integer"

  fun checkOrderable({exp = e1, ty = t1}, {exp = e2, ty = t2}, pos) = 
        case t1 of 
          Types.INT => (case t2 of 
              Types.INT => ()
              | _ => ErrorMsg.error pos "expected integer"
            )
          | Types.STRING => (case t2 of 
              Types.STRING => ()
              | _ => ErrorMsg.error pos "expected string"
            )
          | _ => ErrorMsg.error pos "expected integer or string"

  fun checkEqualable({exp = e1, ty = t1}, {exp = e2, ty = t2}, pos) = 
        case t1 of 
          Types.INT => (case t2 of 
              Types.INT => ()
              | _ => ErrorMsg.error pos "expected integer"
            )
          | Types.STRING => (case t2 of 
              Types.STRING => ()
              | _ => ErrorMsg.error pos "expected string"
            )
          | Types.RECORD (_, uq1) => (case t2 of 
              Types.RECORD (_, uq2) => (if uq1 = uq2 then () else ErrorMsg.error pos "incompatible record types")
              | _ => ErrorMsg.error pos "expected record type"
            )
          | Types.ARRAY (_, uq1) => (case t2 of 
              Types.ARRAY (_, uq2) => (if uq1 = uq2 then () else ErrorMsg.error pos "incompatible array types")
              | _ => ErrorMsg.error pos "expected array type"
            )
          | _ => ErrorMsg.error pos "expected integer, string, record, or array"

  fun transExp (venv, tenv) =
    let
      fun checkExp (Absyn.VarExp var) = 
          let val {exp = e, ty = ty} = trvar var
          in
            {exp = (), ty = ty}
          end
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
            (Absyn.OpExp{left, oper, right, pos}) = (
              case oper of
                (Absyn.PlusOp | Absyn.MinusOp | Absyn.TimesOp | Absyn.DivideOp) => (checkInt(checkExp left, pos); checkInt(checkExp right, pos))
                | (Absyn.LeOp | Absyn.LtOp | Absyn.GeOp | Absyn.GtOp) => checkOrderable(checkExp left, checkExp right, pos)
                | (Absyn.EqOp | Absyn.NeqOp) => checkEqualable(checkExp left, checkExp right, pos);
              {exp = (), ty = Types.INT} 
            )
              
          
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
      and trvar (Absyn.SimpleVar(id, pos)) = 
        (case Symbol.look(venv, id)
          of SOME ty => {exp = (), ty = ty}
          | NONE => (ErrorMsg.error pos ("undefined variable " ^ Symbol.name id); {exp=(), ty=Types.INT})
        )
        | trvar (Absyn.FieldVar(v, id, pos)) = 
            let
              val {exp = e, ty = ty} = trvar v
            in
              case ty of
              Types.RECORD (record_func, uq) => 
                  let 
                    fun do_ids_match (symb, typ) = symb = id
                    val rec_fields = record_func ()
                    val matching_id = List.find do_ids_match rec_fields
                  in
                    case matching_id
                      of SOME (_, typ) => {exp = (), ty=typ}
                      | NONE => (ErrorMsg.error pos ("field " ^ Symbol.name id ^ " on record type");
                                {exp = (), ty = Types.INT}) (* could make this message more useful *)  
                  end
              | _ => (ErrorMsg.error pos ("dot of field " ^ Symbol.name id ^ " on non-record type");
                      {exp = (), ty = Types.INT}) (* could make this message more useful *) 
            end
        | trvar (Absyn.SubscriptVar(v, exp, pos)) = 
            let
              val {exp = e, ty = ty} = trvar v
            in
              case ty of
                Types.ARRAY (t, uq) => {exp = (), ty = t} 
                | _ => (ErrorMsg.error pos "subscript of variable which is not an array";
                        {exp = (), ty = Types.INT}) (* could make this message more useful *)
            end
        
    in
      checkExp
    end

  fun transDec (venv, tenv, dec) = {venv = venv, tenv = tenv}
  fun transTy (tenv, ty) = Types.BOTTOM
end
