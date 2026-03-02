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

  fun transTy (tenv, ty) = Types.BOTTOM
      

  (* Returns true if t1 is a subtype of t2 *)
  fun isSubtype (t1, t2) =
    let in
      case (t1, t2) of
        (Types.BOTTOM, _) => true
      | (_, Types.UNIT) => true
      | (Types.INT, Types.INT) => true
      | (Types.READ_ONLY_INT, Types.INT) => true
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
  (* Functions are contravariant over their arguments *)
  and functionArgsContravariant ([], []) = true
    | functionArgsContravariant (ty1 :: rest1, ty2 :: rest2) =
        isSubtype (ty2, ty1) andalso functionArgsContravariant (rest1, rest2)
    | functionArgsContravariant (_, _) = false

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

  fun isTypeMismatch (t1, t2) =
    not (isSubtype (t1, t2) orelse isSubtype (t2, t1))
    orelse
    (areTypesEqual (t1, Types.UNIT)
     andalso
     not
       (areTypesEqual (t2, Types.UNIT) orelse areTypesEqual (t2, Types.BOTTOM)))
    orelse
    (areTypesEqual (t2, Types.UNIT)
     andalso
     not
       (areTypesEqual (t1, Types.UNIT) orelse areTypesEqual (t1, Types.BOTTOM)))

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

  fun transDec (venv, tenv, Absyn.VarDec {name, escape, typ, init, pos}) =
        let
          val {exp = e, ty = exp_ty} = transExp (venv, tenv) init
          val type_annotation =
            case typ of
              SOME (t_symbol, _) =>
                (case Symbol.look (tenv, t_symbol) of
                   SOME t => SOME t
                 | NONE =>
                     ( ErrorMsg.error pos
                         ("Undefined type " ^ Symbol.name t_symbol)
                     ; NONE
                     ))
            | NONE => NONE

        in
          (* Does not handle duplicate declarations since I'm not 100% the expected behavior *)
          case type_annotation of
            SOME t =>
              if not (isSubtype (exp_ty, t)) then
                ( ErrorMsg.error pos
                    ("Type mismatch between for var dec " ^ Symbol.name name
                     ^ " between type annotation " ^ Types.typeToString t
                     ^ " and init expr of type " ^ Types.typeToString exp_ty)
                ; {tenv = tenv, venv = Symbol.enter (venv, name, Types.BOTTOM)}
                )
              else
                {tenv = tenv, venv = Symbol.enter (venv, name, t)}
          | NONE =>
              if areTypesEqual (exp_ty, Types.NIL) then
                ( ErrorMsg.error pos
                    ("Init expression for nil must have type annotation")
                ; {tenv = tenv, venv = Symbol.enter (venv, name, Types.BOTTOM)}
                )
              else
                {tenv = tenv, venv = Symbol.enter (venv, name, exp_ty)}
        end
    (* Just need to make this handle recursive type defs *)
    | transDec (venv, tenv, Absyn.TypeDec dec_list) =
        let
          fun processTyDec ({name, ty=Absyn.NameTy (existingTypeName, p), pos}, tenv') =
              let
                val nameType = (case Symbol.look (tenv', existingTypeName) of
                    SOME t => t
                  | NONE =>
                      ( ErrorMsg.error p ("undefined type " ^ Symbol.name existingTypeName)
                      ; Types.BOTTOM
                      ))
              in
                Symbol.enter (tenv', name, nameType)
              end
              
          | processTyDec ({name, ty=Absyn.ArrayTy (sym, p), pos}, tenv') =
              let
                val typeInArray =
                  case Symbol.look (tenv', sym) of
                    SOME t => t
                  | NONE =>
                      ( ErrorMsg.error pos ("undefined type " ^ Symbol.name sym)
                      ; Types.BOTTOM
                      )
              in
                Symbol.enter (tenv', name, Types.ARRAY (typeInArray, ref ()))
              end
              
          | processTyDec ({name, ty=Absyn.RecordTy fieldList, pos}, tenv') =
              let
                fun f ({escape, name=fieldName, pos=pos', typ}, acc) =
                  let
                    val ty =
                      case Symbol.look (tenv', typ) of
                        SOME t => t
                      | NONE => if typ=name
                            then Types.RECORD (recFunction, ref ()) 
                            else
                              ( ErrorMsg.error pos' ("undefined type " ^ Symbol.name typ)
                              ; Types.BOTTOM
                              )
                  in
                    (fieldName, ty) :: acc
                  end
                and recFunction () = foldl f [] fieldList
              in
                Symbol.enter(tenv', name, Types.RECORD (recFunction, ref ()))
              end
        in
          {venv = venv, tenv = foldl processTyDec tenv dec_list}
        end
    | transDec (venv, tenv, Absyn.FunctionDec dec_list) =
        let
          fun getParamTypes ({name, escape, typ, pos}, acc) =
            let
              val paramType =
                case Symbol.look (tenv, typ) of
                  SOME t => t
                | NONE =>
                    ( ErrorMsg.error pos
                        ("Undefined type " ^ Symbol.name typ ^ " for field "
                         ^ Symbol.name name)
                    ; Types.BOTTOM
                    )
            in
              paramType :: acc
            end

          fun readInFunctionHeader
            ({body, name, params, pos, result}, (venv', nameEnv)) =
            let
              val fieldTypeList = foldr getParamTypes [] params
              val returnType =
                case result of
                  SOME (t_symbol, _) =>
                    (case Symbol.look (tenv, t_symbol) of
                       SOME t => t
                     | NONE =>
                         ( ErrorMsg.error pos
                             ("Undefined type " ^ Symbol.name t_symbol)
                         ; Types.BOTTOM
                         ))
                | NONE => Types.UNIT
              val () =
                case Symbol.look (nameEnv, name) of
                  SOME v =>
                    ErrorMsg.error pos
                      ("Duplicate function names " ^ Symbol.name name
                       ^ " in function declaration group")
                | NONE => ()
            in
              ( Symbol.enter
                  (venv', name, Types.ARROW (fieldTypeList, returnType))
              , Symbol.enter (nameEnv, name, 1)
              )
            end

          (* A lot of code duplication from getParamTypes but it's probably fine *)
          fun addParamTypesToVenv ({name, escape, typ, pos}, venv') =
            let
              val paramType =
                case Symbol.look (tenv, typ) of
                  SOME t => t
                | NONE =>
                    ( ErrorMsg.error pos
                        ("Undefined type " ^ Symbol.name typ ^ " for field "
                         ^ Symbol.name name)
                    ; Types.BOTTOM
                    )
            in
              Symbol.enter (venv', name, paramType)
            end

          fun typeCheckFunction ({body, name, params, pos, result}, venv') =
            let
              val functionVenv = foldl addParamTypesToVenv venv' params
              val returnType =
                case Symbol.look (venv', name) of
                  SOME (Types.ARROW (fl, retType)) => retType
                | _ =>
                    ( ErrorMsg.error pos
                        ("Undefined function " ^ Symbol.name name)
                    ; Types.BOTTOM
                    ) (* Should never hit this case *)
              val {exp = e, ty = bodyType} = transExp (functionVenv, tenv) body
            in
              if
                not (isSubtype (bodyType, returnType))
                orelse
                (areTypesEqual (returnType, Types.UNIT)
                 andalso
                 not
                   (areTypesEqual (bodyType, Types.UNIT)
                    orelse areTypesEqual (bodyType, Types.BOTTOM)))
              then
                ErrorMsg.error pos
                  ("Type mismatch between for function dec " ^ Symbol.name name
                   ^ " between type annotation " ^ Types.typeToString returnType
                   ^ " and body of type " ^ Types.typeToString bodyType)
              else
                ()
            end
          val emptyNameSet: int Symbol.table = Symbol.empty
          val (venvWithFunctionGroup, nameList) =
            foldl readInFunctionHeader (venv, emptyNameSet) dec_list
          val () =
            List.app (fn dec => typeCheckFunction (dec, venvWithFunctionGroup))
              dec_list
        in
          {tenv = tenv, venv = venvWithFunctionGroup}
        end

  and transExp (venv, tenv) =
    let
      fun checkExp (Absyn.VarExp var) =
            let val {exp = e, ty = ty} = trvar var
            in {exp = (), ty = ty}
            end
        | checkExp Absyn.NilExp = {exp = (), ty = Types.NIL}
        | checkExp (Absyn.IntExp _) = {exp = (), ty = Types.INT}
        | checkExp (Absyn.StringExp (_, _)) = {exp = (), ty = Types.STRING}

        (*TODO: implement function call checking*)
        | checkExp
            (Absyn.CallExp
               {func: Absyn.symbol, args: Absyn.exp list, pos: Absyn.pos}) =
            (*check that func actually exists as a function in our venv*)
            (case Symbol.look (venv, func) of
               SOME (Types.ARROW (fargs, ret)) =>
                 let
                   fun arg_to_ty (arg: Absyn.exp) =
                     #ty (checkExp arg)
                   (*convert tylist backwards and then reverse*)
                   val arg_tylist = rev
                     (foldl
                        (fn (arg, arg_tylist) => (arg_to_ty arg) :: arg_tylist)
                        ([] : Types.ty list) args)
                 in
                   (*check that args match arrow_type args.*)
                   if
                     functionArgsContravariant
                       (arg_tylist, fargs) (*return return type of function*)
                   then
                     {exp = (), ty = ret}
                   else
                     ( ErrorMsg.error pos
                         ("Function args do not match for function "
                          ^ Symbol.name func)
                     ; {exp = (), ty = Types.NIL}
                     )
                 end
             | _ =>
                 ( ErrorMsg.error pos
                     ("Undefined function " ^ (Symbol.name func))
                 ; {exp = (), ty = Types.NIL}
                 ))

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
                                 ("Field " ^ Symbol.name id
                                  ^ " not found in initialization of record type "
                                  ^ Symbol.name typ)
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
              case var_ty of
                Types.READ_ONLY_INT =>
                  ErrorMsg.error pos
                    ("Tried to assign value to read only integer")
              | _ => ();
              if isSubtype (expr_ty, var_ty) then
                ()
              else
                ErrorMsg.error pos
                  ("Cannot assign " ^ Types.typeToString expr_ty ^ " to "
                   ^ Types.typeToString var_ty);
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
              if isTypeMismatch (then_ty, else_ty) then
                ErrorMsg.error pos
                  ("Type mismatch in if then else statement:\n \tthen type: "
                   ^ Types.typeToString then_ty ^ "\n\telse type: "
                   ^ Types.typeToString else_ty)
              else
                ();
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
                  , Types.READ_ONLY_INT
                  ) (* not sure how to make sure this new var is read-only *)
            in
              checkInt (checkExp lo, pos);
              checkInt (checkExp hi, pos);
              case transExp (newVenv, tenv) body of
                {exp = _, ty = Types.UNIT} => ()
              | _ =>
                  ErrorMsg.error pos "Body of for loop produces non-unit value";
              {exp = (), ty = Types.UNIT}
            end
        | checkExp (Absyn.BreakExp _) = {exp = (), ty = Types.BOTTOM}
        | checkExp (Absyn.LetExp {decs, body, pos}) =
            let
              fun processDec (dec, {venv = v, tenv = t}) = transDec (v, t, dec)
              val {venv = new_venv, tenv = new_tenv} =
                foldl processDec {venv = venv, tenv = tenv} decs
            in
              transExp (new_venv, new_tenv) body
            end
        | checkExp (Absyn.ArrayExp {typ, size, init, pos}) =
            let
              val {exp = _, ty = initType} = checkExp init
            in
              checkInt (checkExp size, pos);
              case Symbol.look (tenv, typ) of
                SOME (Types.ARRAY (typeInArr, uq)) =>
                  (* Arrays are invariant over their type *)
                  (if not (isSubtype (initType, typeInArr)) then
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
                 ; {exp = (), ty = Types.BOTTOM}
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
                             ^ " not found on record type variable "
                             ^ Symbol.name id)
                        ; {exp = (), ty = Types.BOTTOM}
                        )
                  end
              | t =>
                  ( ErrorMsg.error pos
                      ("dot of field " ^ Symbol.name id ^ " on non-record type "
                       ^ Types.typeToString t)
                  ; {exp = (), ty = Types.BOTTOM}
                  )
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
              | t =>
                  ( ErrorMsg.error pos
                      ("subscript of variable of non-array type "
                       ^ Types.typeToString t)
                  ; {exp = (), ty = Types.BOTTOM}
                  ) (* could make this message more useful *)
            end

    in
      checkExp
    end

  fun transProg exp =
    let in transExp (Env.base_venv, Env.base_tenv) exp; ()
    end
end
