structure MipsFrame: FRAME =
struct
  datatype access = InFrame of int | InReg of Temp.temp
  type frame =
    {name: Temp.label, formals: access list, numLocalsInFrame: int ref}

  val FP = Temp.newtemp()
  val wordsize = 4

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
            InReg (Temp.newtemp ())
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
    

  fun newFrame {name: Temp.label, formals: bool list} =
    let
      fun oneFormalToAccess (true, (offset, formals, numRegArgs)) =
            (offset + 4, (InFrame offset) :: formals, numRegArgs)
        | oneFormalToAccess (false, (offset, formals, numRegArgs)) =
            if numRegArgs < argRegisterCount  (* only 4 parameters can be passed in a0-a3 *)
              then (offset, (InReg (Temp.newtemp ())) :: formals, numRegArgs + 1)
              else (offset + 4, (InFrame offset) :: formals, numRegArgs)
      val (numBytesForFormals, accesses, numRegArgs) =
        foldr oneFormalToAccess (0, [], 0) formals
    in
      {name = name, formals = accesses, numLocalsInFrame = ref 0}
    end
end
