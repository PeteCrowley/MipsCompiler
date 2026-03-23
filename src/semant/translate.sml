structure Translate: TRANSLATE = 
struct 
    structure Frame = MipsFrame
    datatype exp =  Ex of Tree.exp
                    | Nx of Tree.stm
                    | Cx of Temp.label * Temp.label -> Tree.stm
         

    datatype levelTree = EMPTY
        | NODE of levelTree * Frame.frame * unit ref (* Parent, frame , unique id *)

    type level = levelTree
    type access = level * Frame.access

    val frags : Frame.frag list ref = ref []

    val outermost = NODE(EMPTY, Frame.newFrame {name = Temp.newlabel(), formals = []}, ref ())
    val newLevel = 
        let
            fun f {parent: level, name: Temp.label, formals: bool list} = 
                let
                  val formalsAndStaticLink = true :: formals
                in
                  NODE(parent, Frame.newFrame {name = name, formals = formalsAndStaticLink}, ref ())
                end 
        in
            f
        end

    val formals = 
        let
            fun getFormalAccesses level =
                let
                val frame = case level of
                    EMPTY => raise Fail "Outermost level has no frame"
                    | NODE (_, frame, _) => frame
                val frameFormals = case Frame.formals frame of
                                        [] => raise Fail "Frame has no frame pointer stored"
                                        | fp::rest => rest
                val translateFormals = List.map (fn access => (level, access)) frameFormals
                in
                    translateFormals
                end
        in
            getFormalAccesses
        end

    val allocLocal = 
        let
            fun f lev esc = 
                let
                    val (parent, frame, uq) = case lev of
                        EMPTY => raise Fail "Outermost level has no frame"
                        | NODE (parent, frame, uq) => (parent, frame, uq)
                    val frameAccess = Frame.allocLocal frame esc
                in
                    (NODE(parent, frame, uq), frameAccess)
                end
        in
            f
        end

    fun seq [s] = s
        | seq (a::l) = Tree.SEQ(a, seq l)
        | seq [] = Tree.EXP(Tree.CONST 0)

    fun unEx (Ex e) = e
        | unEx (Nx s) = Tree.ESEQ(s, Tree.CONST 0)
        | unEx (Cx genstm) = 
            let
              val r = Temp.newtemp()
              val t = Temp.newlabel()
              val f = Temp.newlabel()
            in
              Tree.ESEQ(seq[Tree.MOVE(Tree.TEMP r, Tree.CONST 1),
                              genstm(t, f),
                              Tree.LABEL f,
                              Tree.MOVE(Tree.TEMP r, Tree.CONST 0),
                              Tree.LABEL t
                            ], Tree.TEMP r)
            end

    fun unNx (Ex e) = Tree.EXP e
        | unNx (Nx s) = s
        | unNx (Cx genstm) = 
            let
                val t = Temp.newlabel()
                val f = Temp.newlabel()
            in
                seq[genstm(t, f), Tree.LABEL f, Tree.LABEL t]
            end 
    
    fun unCx (Ex (Tree.CONST 0)) = (fn (t, f) => Tree.JUMP (Tree.NAME f, [f]))
        | unCx (Ex (Tree.CONST 1)) = (fn (t, f) => Tree.JUMP (Tree.NAME t, [t])) 
        | unCx (Ex e) = (fn (t, f) => Tree.CJUMP(Tree.EQ, e, Tree.CONST 1, t, f))
        | unCx (Nx _) = (fn (t, f) => Tree.EXP(Tree.CONST 0)) (* never hit *)
        | unCx (Cx genstm) = genstm

    fun getDummyExp () = Nx (Tree.EXP (Tree.CONST 0))

    fun printTree exp = 
        let
          val irExp = unNx exp
        in
          Printtree.printtree (TextIO.stdOut, irExp)
        end

    fun followStaticLinks (NODE(parLev, _, uq'), currFpAddr, targetUq) = 
            if targetUq = uq' then currFpAddr else followStaticLinks(parLev, Tree.MEM currFpAddr, targetUq)    (* since static links are stored at offset 0 *)
          | followStaticLinks (EMPTY, _, _) = raise Fail "Went past outermost level without finding variable's frame"

    fun simpleVar ((NODE(accParentLev, accFrame, accUq), frameAcc), NODE(parentLev, frame, uq)) = 
        let
            (* for debugging print access frame and variable frame *)
          (* val () = print ("Access frame: " ^ Symbol.name (Frame.name accFrame) ^ ", variable frame: " ^ Symbol.name (Frame.name frame) ^ "\n") *)
          val framePointerAddrExp = followStaticLinks (NODE(parentLev, frame, uq), Tree.TEMP Frame.FP, accUq)
        in
            (* printTree (Ex (Frame.exp frameAcc framePointerAddrExp)); *)
            Ex (Frame.exp frameAcc framePointerAddrExp)
        end
    | simpleVar (_, _) = raise Fail "simpleVar called on EMPTY level"

    fun intExp i = Ex (Tree.CONST i)

    fun assignExp (location, value) = 
        let
            val locExp = unEx location
            val valExp = unEx value
            val () = case locExp of
                Tree.MEM _ => ()
              | Tree.TEMP _ => ()
              | _ => raise Fail "Left-hand side of assignment must be a storage location"
        in
            Nx (Tree.MOVE(locExp, valExp))
        end

    fun expList exps = 
        let
            fun seq [] = Tree.EXP (Tree.CONST 0)
              | seq [e] = unNx e
              | seq (e::es) = Tree.SEQ(unNx e, seq es)
        in
            Nx (seq exps)
        end

    fun functionDec (level, body) = 
        let
            val bodyExp = unEx body
            val f = case level of
                EMPTY => raise Fail "Outermost level has no frame"
              | NODE (_, frame, _) => frame
        in
            Nx (Frame.addPrologueEpliogue (f, bodyExp))
        end

    fun functionCall (level, funcLevel, funcLabel, args) = 
        let
            val (calleeGrandPar, parentFrame, parentUq) = case funcLevel of
                EMPTY => raise Fail "Function level cannot be outermost level"
              | NODE (EMPTY, _, _) => raise Fail "cannot call outermost function"
              | NODE (NODE parentLev, _, _) => parentLev
            val staticLink =  followStaticLinks (level, Tree.TEMP Frame.FP, parentUq)
            val argExps = List.map unEx args
        in
            Ex (Tree.CALL(Tree.NAME funcLabel, staticLink::argExps))
        end

    fun addExp (e1, e2) = Ex (Tree.BINOP(Tree.PLUS, unEx e1, unEx e2))
    fun subExp (e1, e2) = Ex (Tree.BINOP(Tree.MINUS, unEx e1, unEx e2))
    fun mulExp (e1, e2) = Ex (Tree.BINOP(Tree.MUL, unEx e1, unEx e2))
    fun divExp (e1, e2) = Ex (Tree.BINOP(Tree.DIV, unEx e1, unEx e2))

    fun ltExp (e1, e2) = Cx (fn (t,f) => (Tree.CJUMP(Tree.LT, unEx e1, unEx e2, t, f)))
    fun leExp (e1, e2) = Cx (fn (t,f) => (Tree.CJUMP(Tree.LE, unEx e1, unEx e2, t, f)))
    fun gtExp (e1, e2) = Cx (fn (t,f) => (Tree.CJUMP(Tree.GT, unEx e1, unEx e2, t, f)))
    fun geExp (e1, e2) = Cx (fn (t,f) => (Tree.CJUMP(Tree.GE, unEx e1, unEx e2, t, f)))
    fun eqExp (e1, e2) = Cx (fn (t,f) => (Tree.CJUMP(Tree.EQ, unEx e1, unEx e2, t, f)))
    fun neqExp (e1, e2) = Cx (fn (t,f) => (Tree.CJUMP(Tree.NE, unEx e1, unEx e2, t, f)))

    fun stringComp (e1, e2) = Frame.externalCall ("strcmp", [unEx e1, unEx e2])

    fun strLtExp (e1, e2) = Cx (fn (t,f) => (Tree.CJUMP(Tree.LT, stringComp(e1, e2), Tree.CONST 0, t, f)))
    fun strLeExp (e1, e2) = Cx (fn (t,f) => (Tree.CJUMP(Tree.LE, stringComp(e1, e2), Tree.CONST 0, t, f)))
    fun strGtExp (e1, e2) = Cx (fn (t,f) => (Tree.CJUMP(Tree.GT, stringComp(e1, e2), Tree.CONST 0, t, f)))
    fun strGeExp (e1, e2) = Cx (fn (t,f) => (Tree.CJUMP(Tree.GE, stringComp(e1, e2), Tree.CONST 0, t, f)))
    fun strEqExp (e1, e2) = Cx (fn (t,f) => (Tree.CJUMP(Tree.EQ, stringComp(e1, e2), Tree.CONST 0, t, f)))
    fun strNeqExp (e1, e2) = Cx (fn (t,f) => (Tree.CJUMP(Tree.NE, stringComp(e1, e2), Tree.CONST 0, t, f)))

    (* leaving this ifExp ineffecient and not optimized as described on Appel 162 since I'm lazy -Pete *)
    fun ifExp (cond, thenExp, elseExp) = 
        let
            val condCx = unCx cond
            val thenEx = unEx thenExp
            val elseEx = unEx elseExp
            val r = Temp.newtemp()
            val t = Temp.newlabel()
            val f = Temp.newlabel()
            val endLabel = Temp.newlabel()
        in
            Ex (Tree.ESEQ(seq[condCx(t,f),
                                Tree.LABEL t,
                                Tree.MOVE(Tree.TEMP r, thenEx),
                                Tree.JUMP(Tree.NAME endLabel, [endLabel]),
                                Tree.LABEL f,
                                Tree.MOVE(Tree.TEMP r, elseEx),
                                Tree.LABEL endLabel
                                ], Tree.TEMP r))
        end

    fun whileExp (cond, body, doneLabel) =
        let
            val testLabel = Temp.newlabel()
            val bodyLabel = Temp.newlabel()
            val condCx = unCx cond
            val bodyEx = unNx body
        in
            Nx (seq[Tree.LABEL testLabel,
                    condCx(bodyLabel, doneLabel),
                    Tree.LABEL bodyLabel,
                    bodyEx,
                    Tree.JUMP(Tree.NAME testLabel, [testLabel]),
                    Tree.LABEL doneLabel
                    ])
        end

    fun forExp (loopVarLoc, lo, hi, body, doneLabel) =
        let
          val L1 = Temp.newlabel()
          val L2 = Temp.newlabel()
          val loopVarEx = unEx loopVarLoc
          val loEx = unEx lo
          val hiEx = unEx hi

        in
            Nx (seq[Tree.MOVE(loopVarEx, loEx),
                    Tree.CJUMP(Tree.LE, loopVarEx, hiEx, L2, doneLabel),
                    Tree.LABEL L1,
                    Tree.MOVE(loopVarEx, Tree.BINOP(Tree.PLUS, loopVarEx, Tree.CONST 1)),
                    Tree.LABEL L2,
                    unNx body,
                    Tree.CJUMP(Tree.LT, loopVarEx, hiEx, L1, doneLabel),
                    Tree.LABEL doneLabel
                    ])
        end

    fun breakExp doneLabel = Nx (Tree.JUMP(Tree.NAME doneLabel, [doneLabel]))

    fun getResult () = !frags

    fun stringLit str = 
    let
        val label = Temp.newlabel()
        val () = frags := Frame.STRING (label, str) :: !frags
    in
        Ex (Tree.NAME label)
    end

end
