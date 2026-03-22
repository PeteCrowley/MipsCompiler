structure MipsFrame: FRAME =
struct
  datatype access = InFrame of int | InReg of Temp.temp
  type frame =
    {name: Temp.label, formals: access list, numLocalsInFrame: int ref, numRegArgs: int}

  val FP = 30
  val SP = 29
  val RA = 31
  val wordsize = 4
  val RV = 2
  val a0 = 4

  fun name (frame: frame) = #name frame

  fun access_escapes (access: access) =
    case access of
      InFrame _ => false
    | InReg _ => true

  fun formals (frame: frame) = (#formals frame)

  val argRegisterCount = 4

  fun allocLocal (frame: frame) =
    let
      fun allocLocalForFrame (false) =
        let
          val t = Temp.newtemp ()
          (* val () = print ("allocated local in register " ^ Int.toString t ^ "\n") *)
        in
          InReg t
        end
            
        | allocLocalForFrame (true) =
            let
              val inFrameRef = #numLocalsInFrame frame
              val _ = inFrameRef := !inFrameRef + 1
            in
              InFrame (!inFrameRef * ~4)
            end
    in
      allocLocalForFrame
    end

  fun exp (InFrame k) framePtrAddr = Tree.MEM(Tree.BINOP(Tree.PLUS, framePtrAddr, Tree.CONST k))
    | exp (InReg t) _ = Tree.TEMP t

  (* this is straight duplicate I'm too lazy not to -Pete *)
  fun seq [s] = s
        | seq (a::l) = Tree.SEQ(a, seq l)
        | seq [] = Tree.EXP(Tree.CONST 0)

  
  fun prologue (label, stackSpace, formalAccesses) = 
      let 
        fun buildPrologue ([], ~1) = seq [Tree.LABEL label,
                          Tree.MOVE(Tree.MEM (Tree.BINOP(Tree.MINUS, Tree.TEMP SP, Tree.CONST wordsize)), Tree.TEMP FP),  (* save old frame pointer in stack *)
                          Tree.MOVE(Tree.TEMP FP, Tree.TEMP SP),  (* update frame pointer *)
                          Tree.MOVE(Tree.TEMP SP, Tree.BINOP(Tree.MINUS, Tree.TEMP SP, Tree.CONST stackSpace)), (* decrement stack pointer *)
                          Tree.MOVE(Tree.MEM (Tree.BINOP(Tree.MINUS, Tree.TEMP FP, Tree.CONST (2 * wordsize))), Tree.TEMP RA) (* save RA *)
                        ]
          | buildPrologue ((InReg r)::rest, j) = Tree.SEQ(buildPrologue (rest, j-1), Tree.MOVE(Tree.TEMP r, Tree.TEMP (a0 + j))) (* move args to temps *)
          | buildPrologue (_, _) = raise Fail "Unexpected access type in formalAccesses list"
        val formalsWithRegAccesses = List.filter (fn access => case access of InReg _ => true | InFrame _ => false) formalAccesses
      in
        buildPrologue (formalsWithRegAccesses, List.length formalsWithRegAccesses - 1)
      end
          
      

  fun epilogue (body) = seq[Tree.MOVE(Tree.TEMP RV, body),
                                  Tree.MOVE(Tree.TEMP RA, Tree.MEM (Tree.BINOP(Tree.MINUS, Tree.TEMP FP, Tree.CONST (2 * wordsize)))), (* restore RA *)
                                  Tree.MOVE(Tree.TEMP SP, Tree.TEMP FP), (* collapse stack frame *)
                                  Tree.MOVE(Tree.TEMP FP, Tree.MEM (Tree.BINOP(Tree.MINUS, Tree.TEMP FP, Tree.CONST (wordsize)))), (* restore FP *)
                                  Tree.JUMP(Tree.TEMP RA, [])
                                ]


  
  fun addPrologueEpliogue (frame: frame, bodyExp: Tree.exp) =
    (* code for moving args into general purpose registers *)
    let
      val stackSpace = wordsize * !(#numLocalsInFrame frame)
      val numRegArgs = #numRegArgs frame
      val label = #name frame
      val formalAccesses = #formals frame
      (* print numlocals in frame for debugging *)
      val () = print ("Number of locals in frame: " ^ Int.toString (!(#numLocalsInFrame frame)) ^ "\n")

    in
      Tree.SEQ(prologue(label, stackSpace, formalAccesses) , epilogue bodyExp)
    end

  fun newFrame {name: Temp.label, formals: bool list} =
    let
      fun oneFormalToAccess (true, (offset, formals, numRegArgs)) =
            (offset + wordsize, (InFrame offset) :: formals, numRegArgs)
        | oneFormalToAccess (false, (offset, formals, numRegArgs)) =
            if numRegArgs < argRegisterCount  (* only 4 parameters can be passed in a0-a3 *)
              then (offset, (InReg (Temp.newtemp ())) :: formals, numRegArgs + 1)
              else (offset + wordsize, (InFrame offset) :: formals, numRegArgs)
      val (numBytesForFormals, accesses, numRegArgs) =
        foldr oneFormalToAccess (0, [], 0) formals
      val f = {name = name, formals = accesses, numLocalsInFrame = ref 0, numRegArgs=numRegArgs}
    in
      allocLocal f true; (* for FP *)
      allocLocal f true; (* for RA *)
      f
    end
end
