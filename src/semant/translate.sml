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

    val outermost = EMPTY
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

    fun getDummyExp () = Ex (Tree.CONST 0)

    fun simpleVar (((accParentLev, accFrame, accUq), frameAcc), level) = 
        let
          fun followStaticLinks ((_, _, uq), currFpAddr) = 
            if accUq = uq then currFpAddr else Tree.MEM(Tree.BINOP(Tree.PLUS, Tree.CONST 0, Tree.MEM currFpAddr))
          val framePointerAddrExp = followStaticLinks level
        in
          Frame.exp frameAcc framePointerAddrExp
        end

end
