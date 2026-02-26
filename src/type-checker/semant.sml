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


  fun transVar (venv, tenv, var) = {exp = (), ty = Types.BOTTOM}
  fun transDec (venv, tenv, dec) = {venv = venv, tenv = tenv}
  fun transTy (tenv, ty) = Types.BOTTOM

  (* Returns true if t1 is a subtype of t2 *)
  fun isSubtype (t1, t2) =
    let
      (* Functions are contravariant over their arguments *)
      fun functionArgsContravariant ([], []) = true
        | functionArgsContravariant (ty1 :: rest1, ty2 :: rest2) =
            isSubtype (ty2, ty1)
            andalso functionArgsContravariant (rest1, rest2)
        | functionArgsContravariant (_, _) = false
    in
      case (t1, t2) of
        (Types.BOTTOM, _) => true
      | (_, Types.UNIT) => true
      | (Types.INT, Types.INT) => true
      | (Types.STRING, Types.STRING) => true
      | (Types.NIL, Types.NIL) => true
      | (Types.NIL, Types.RECORD _) => true
      | (Types.NIL, Types.ARRAY _) => true
      | (Types.RECORD (_, uq1), Types.RECORD (_, uq2)) => uq1 = uq2
      | (Types.ARRAY (_, uq1), Types.ARRAY (_, uq2)) => uq1 = uq2
      | (Types.ARROW (args1, ret1), Types.ARROW (args2, ret2)) =>
          isSubtype (ret1, ret2)
          andalso functionArgsContravariant (args1, args2)
      | _ => false
    end

  (* Returns true if t1 is exactly the same as t2 *)
  fun areTypesEqual (t1, t2) =
    isSubtype (t1, t2) andalso isSubtype (t2, t1)

  fun leastUpperBound (Types.BOTTOM, t2) = t2
    | leastUpperBound (t1, Types.BOTTOM) = t1
    | leastUpperBound (Types.NIL, Types.RECORD r) = Types.RECORD r
    | leastUpperBound (Types.RECORD r, Types.NIL) = Types.RECORD r
    | leastUpperBound (Types.NIL, Types.ARRAY a) = Types.ARRAY a
    | leastUpperBound (Types.ARRAY a, Types.NIL) = Types.ARRAY a
    | leastUpperBound (t1, t2) =
        if areTypesEqual (t1, t2) then t1 else Types.UNIT

  fun checkInt ({exp, ty}, pos) =
    if not (isSubtype (ty, Types.INT)) then
      ErrorMsg.error pos "expected integer"
    else
      ()

  fun checkOrderable ({exp = e1, ty = t1}, {exp = e2, ty = t2}, pos) =
    case leastUpperBound (t1, t2) of
      Types.BOTTOM => ()
    | Types.INT => ()
    | Types.STRING => ()
    | _ => ErrorMsg.error pos "expected int * int or str * str"

  fun checkEqualable ({exp = e1, ty = t1}, {exp = e2, ty = t2}, pos) =
    if isSubtype (t1, t2) orelse isSubtype (t2, t1) then
      case leastUpperBound (t1, t2) of
        Types.BOTTOM => ()
      | Types.INT => ()
      | Types.STRING => ()
      | Types.NIL => ()
      | Types.RECORD _ => ()
      | Types.ARRAY _ => ()
      | _ =>
          ErrorMsg.error pos "expected integer, string, record, or array tuple"
    else
      ErrorMsg.error pos "incompatible types in comparison"


  fun transExp (venv, tenv) =
    let
      fun checkExp (Absyn.VarExp var) =
            let val {exp = e, ty = ty} = trvar var
            in {exp = (), ty = ty}
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
        | checkExp (Absyn.OpExp {left, oper, right, pos}) =
            ( case oper of
                (Absyn.PlusOp | Absyn.MinusOp | Absyn.TimesOp | Absyn.DivideOp) =>
                  ( checkInt (checkExp left, pos)
                  ; checkInt (checkExp right, pos)
                  )
              | (Absyn.LeOp | Absyn.LtOp | Absyn.GeOp | Absyn.GtOp) =>
                  checkOrderable (checkExp left, checkExp right, pos)
              | (Absyn.EqOp | Absyn.NeqOp) =>
                  checkEqualable (checkExp left, checkExp right, pos)
            ; {exp = (), ty = Types.INT}
            )
        | checkExp (Absyn.RecordExp {fields, typ, pos}) =
            (case Symbol.look (tenv, typ) of
             (* condition 1: the type must be defined as a record type *)
               SOME (Types.RECORD rec_type) =>
                 let
                   val (rec_func, uq) = rec_type
                   val expectedFieldList = rec_func ()
                   val numFieldsGiven = List.length fields
                   val numFieldsExpected = List.length expectedFieldList
                   (* condition 2: the number of fields given must match the number of fields expected *)
                   val () =
                     if numFieldsGiven < numFieldsExpected then
                       ErrorMsg.error pos
                         ("Not enough fields given for record type "
                          ^ Symbol.name typ)
                     else if numFieldsGiven > numFieldsExpected then
                       ErrorMsg.error pos
                         ("Too many fields given for record type "
                          ^ Symbol.name typ)
                     else
                       ()
                   fun checkRecordFields (providedFields, []) = ()
                     | checkRecordFields (providedFields, (id, ty) :: rest) =
                         ( case
                             List.find (fn (s, expr, p) => s = id)
                               providedFields
                           of
                             SOME (s, expr, p) =>
                               let
                                 val {exp = e, ty = expr_ty} = checkExp expr
                               in
                                 if not (isSubtype (ty, expr_ty)) then
                                   ErrorMsg.error p
                                     ("Field " ^ Symbol.name id
                                      ^ " has incorrect type for record type "
                                      ^ Symbol.name typ)
                                 else
                                   ()
                               end
                           | NONE =>
                               ErrorMsg.error pos
                                 ("Unrecognized field " ^ Symbol.name id
                                  ^ " on record type " ^ Symbol.name typ)
                         ; checkRecordFields (providedFields, rest)
                         )
                 in
                   (* condition 3: all the expected fields must be provided with the correct expression type *)
                   checkRecordFields (fields, expectedFieldList);
                   {exp = (), ty = Types.RECORD rec_type}
                 end
             | _ =>
                 ( ErrorMsg.error pos "Undefined record type"
                 ; {exp = (), ty = Types.NIL}
                 ))
        | checkExp (Absyn.SeqExp explist) =
            (case explist of
               [] => {exp = (), ty = Types.UNIT}
             | [(expr, pos)] => checkExp expr
             | (expr, pos) :: rest =>
                 (checkExp expr; checkExp (Absyn.SeqExp rest)))
        | checkExp (Absyn.AssignExp {var, exp, pos}) =
            let
              val {exp = e1, ty = expr_ty} = checkExp exp
              val {exp = e2, ty = var_ty} = trvar var
            in
              if isSubtype (expr_ty, var_ty) then
                ()
              else
                ErrorMsg.error pos
                  "Incompatible type between variable and expression";
              {exp = (), ty = Types.UNIT}
            end
        | checkExp (Absyn.IfExp {test, then', else', pos}) =
            let
              val {exp = _, ty = then_ty} = checkExp then'
              val {exp = _, ty = else_ty} =
                case else' of
                  SOME else_exp => checkExp else_exp
                | NONE => {exp = (), ty = Types.UNIT}

            in
              checkInt ((checkExp test), pos);
              {exp = (), ty = leastUpperBound (then_ty, else_ty)}
            end

        | checkExp (Absyn.WhileExp {test, body, pos}) =
            ( checkInt (checkExp test, pos)
            ; case checkExp body of
                {exp = _, ty = Types.UNIT} => ()
              | _ =>
                  ErrorMsg.error pos
                    "Body of while loop produces non-unit value"
            ; {exp = (), ty = Types.UNIT}
            )

        | checkExp (Absyn.ForExp {var, escape, lo, hi, body, pos}) =
            let
              val newVenv =
                Symbol.enter
                  ( venv
                  , var
                  , Types.INT
                  ) (* not sure how to make sure this new var is read-only *)
            in
              checkInt (checkExp lo, pos);
              checkInt (checkExp hi, pos);
              case transExp (newVenv, tenv) body of
                {exp = _, ty = Types.UNIT} => ()
              | _ =>
                  ErrorMsg.error pos
                    "Body of while loop produces non-unit value";
              {exp = (), ty = Types.UNIT}
            end
        | checkExp (Absyn.BreakExp _) = {exp = (), ty = Types.BOTTOM}
        | checkExp (Absyn.LetExp {decs, body, pos}) = {exp = (), ty = Types.NIL}
        | checkExp (Absyn.ArrayExp {typ, size, init, pos}) =
            let
              val {exp = _, ty = initType} = checkExp init
            in
              checkInt (checkExp size, pos);
              case Symbol.look (venv, typ) of
                SOME (Types.ARRAY (typeInArr, uq)) =>
                  (* Arrays are invariant over their type *)
                  (if not (areTypesEqual (typeInArr, initType)) then
                     ( ErrorMsg.error pos
                         ("Initializing expression of type "
                          ^ Types.typeToString initType
                          ^ " incorrect type for array of type "
                          ^ Symbol.name typ)
                     ; {exp = (), ty = Types.BOTTOM}
                     )
                   else
                     {exp = (), ty = Types.ARRAY (typeInArr, uq)})
              | SOME t =>
                  ( ErrorMsg.error pos
                      ("Tried to initialize array with non-array type "
                       ^ Symbol.name typ)
                  ; {exp = (), ty = Types.BOTTOM}
                  )
              | NONE =>
                  ( ErrorMsg.error pos
                      ("Tried to initialize array with undefined type "
                       ^ Symbol.name typ)
                  ; {exp = (), ty = Types.BOTTOM}
                  )
            end
      (* | checkExp _ = {exp = (), ty = Types.BOTTOM} *)
      and trvar (Absyn.SimpleVar (id, pos)) =
            (case Symbol.look (venv, id) of
               SOME ty => {exp = (), ty = ty}
             | NONE =>
                 ( ErrorMsg.error pos ("undefined variable " ^ Symbol.name id)
                 ; {exp = (), ty = Types.INT}
                 ))
        | trvar (Absyn.FieldVar (v, id, pos)) =
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
                    case matching_id of
                      SOME (_, typ) => {exp = (), ty = typ}
                    | NONE =>
                        ( ErrorMsg.error pos
                            ("field " ^ Symbol.name id
                             ^ " not found on record type")
                        ; {exp = (), ty = Types.INT}
                        ) (* could make this message more useful *)
                  end
              | _ =>
                  ( ErrorMsg.error pos
                      ("dot of field " ^ Symbol.name id ^ " on non-record type")
                  ; {exp = (), ty = Types.INT}
                  ) (* could make this message more useful *)
            end
        | trvar (Absyn.SubscriptVar (v, exp, pos)) =
            let
              val {exp = e, ty = ty} = trvar v
              val {exp = e2, ty = t2} = checkExp exp
              val () =
                case t2 of
                  Types.INT => ()
                | _ => ErrorMsg.error pos "subscript must be an integer"
            in
              case ty of
                Types.ARRAY (t, uq) => {exp = (), ty = t}
              | _ =>
                  ( ErrorMsg.error pos
                      "subscript of variable which is not an array"
                  ; {exp = (), ty = Types.INT}
                  ) (* could make this message more useful *)
            end

    in
      checkExp
    end

  fun transProg exp =
    let in transExp (Env.base_venv, Env.base_tenv) exp; ()
    end
end
