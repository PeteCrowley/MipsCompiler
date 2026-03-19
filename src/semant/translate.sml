structure Translate: TRANSLATE = 
struct 
    structure Frame = MipsFrame
    type exp = unit (* I think Tree.exp but don't want to break things just yet *)

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


end
